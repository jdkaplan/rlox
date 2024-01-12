use std::io;
use std::mem;
use std::ptr::{addr_of_mut, NonNull};
use std::time::Duration;

use crate::chunk::Opcode;
use crate::compiler::{compile, CompileError};
use crate::gc::Gc;
use crate::heap::Heap;
use crate::object::{
    NativeFn, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative,
    ObjString, ObjType, ObjUpvalue,
};
use crate::table::Table;
use crate::value::Value;
use crate::{FRAMES_MAX, STACK_MAX};

pub type InterpretResult<T> = Result<T, InterpretError>;

#[derive(thiserror::Error, Debug)]
pub enum InterpretError {
    #[error(transparent)]
    Compile(#[from] CompileError),

    #[error(transparent)]
    Runtime(#[from] RuntimeError),
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

// TODO: Include error data
#[derive(thiserror::Error, Debug)]
#[error("runtime error")]
pub struct RuntimeError;

pub fn clock_native(_argc: usize, _argv: NonNull<Value>) -> Value {
    Value::number(clock().as_secs_f64())
}

fn clock() -> Duration {
    let tp = unsafe {
        let mut tp = mem::MaybeUninit::uninit();
        if libc::clock_gettime(libc::CLOCK_PROCESS_CPUTIME_ID, tp.as_mut_ptr()) != 0 {
            panic!("clock: {}", std::io::Error::last_os_error());
        }
        tp.assume_init()
    };

    Duration::new(tp.tv_sec as u64, tp.tv_nsec as u32)
}

#[repr(C)]
pub struct CallFrame {
    pub(crate) closure: NonNull<ObjClosure>,
    ip: usize,
    slots: NonNull<Value>,
}

#[repr(C)]
pub struct Vm<'output> {
    pub(crate) frames: Vec<CallFrame>,
    pub(crate) stack: Vec<Value>,

    pub(crate) globals: Table,
    pub(crate) open_upvalues: Option<NonNull<ObjUpvalue>>,

    pub(crate) init_string: NonNull<ObjString>,

    pub(crate) heap: Heap,

    pub(crate) opts: VmOptions<'output>,
}

impl Default for Vm<'_> {
    fn default() -> Self {
        Self::new(VmOptions {
            stdout: Box::new(io::stdout()),
            stderr: Box::new(io::stderr()),
        })
    }
}

pub struct VmOptions<'output> {
    pub stdout: Box<dyn io::Write + 'output>,
    pub stderr: Box<dyn io::Write + 'output>,
}

// Alloc
impl<'output> Vm<'output> {
    pub fn new(opts: VmOptions<'output>) -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_MAX),
            frames: Vec::with_capacity(FRAMES_MAX),
            open_upvalues: None,

            globals: Table::new(),

            // Will be immediately overwritten before ever being read.
            init_string: NonNull::dangling(),

            heap: Heap::new(),

            opts,
        };

        // The boot Gc skips garbage collection. This ensures that vm.init_string is never
        // dereferenced to mark as reachable.
        vm.init_string = ObjString::from_str(Gc::boot(&mut vm), "init");

        vm.define_native("clock", clock_native);

        vm
    }

    fn reset_stack(&mut self) {
        self.stack = Vec::with_capacity(STACK_MAX);
        self.frames = Vec::with_capacity(FRAMES_MAX);
        self.open_upvalues = None;
    }
}

// Errors
impl Vm<'_> {
    #[must_use]
    pub(crate) fn runtime_error(&mut self, msg: impl AsRef<str>) -> RuntimeError {
        // TODO: Print error message after stack trace.
        writeln!(self.opts.stderr, "{}", msg.as_ref()).unwrap();
        self.stack_trace();
        self.reset_stack();
        RuntimeError
    }

    pub(crate) fn stack_trace(&mut self) {
        for frame in self.frames.iter().rev() {
            let closure = unsafe { frame.closure.as_ref() };
            let func = unsafe { closure.function.as_ref() };

            // The ip has already moved past the instruction that failed, so subtract
            // one extra.
            let line = func.chunk.lines[frame.ip - 1];
            writeln!(self.opts.stderr, "[line {}] in {}", line, func.name()).unwrap();
        }
    }
}

// Stack management
impl Vm<'_> {
    pub(crate) fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    pub(crate) fn pop(&mut self) -> Value {
        self.stack.pop().expect("non-empty stack")
    }

    pub(crate) fn popn(&mut self, n: usize) {
        let last = self.stack.len();
        self.stack.truncate(last - n);
    }

    pub(crate) fn peek(&mut self, offset: usize) -> Value {
        let last = self.stack.len() - 1;
        self.stack[last - offset]
    }

    pub(crate) fn set(&mut self, offset: usize, value: Value) {
        let last = self.stack.len();
        self.stack[last - offset] = value;
    }

    pub(crate) fn sp(&mut self, offset: usize) -> NonNull<Value> {
        let base = self.stack.as_mut_ptr();
        let ptr = unsafe { base.add(self.stack.len() - offset) };
        NonNull::new(ptr).unwrap()
    }

    pub(crate) fn set_sp(&mut self, sp: NonNull<Value>) {
        let top = self.sp(0);
        let n = unsafe { top.as_ptr().offset_from(sp.as_ptr()) as usize };
        self.popn(n);
    }
}

// Calls
impl Vm<'_> {
    pub(crate) fn call_value(&mut self, callee: Value, argc: usize) -> RuntimeResult<()> {
        let gc = Gc::runtime(self);

        let Some(callee) = callee.try_obj::<Obj>() else {
            return Err(self.runtime_error("Can only call functions and classes."));
        };

        match unsafe { callee.as_ref() }.ty {
            ObjType::BoundMethod => {
                let callee = callee.cast::<ObjBoundMethod>();
                let bound = unsafe { callee.as_ref() };
                self.set(argc + 1, bound.receiver);
                self.call(bound.method, argc)
            }
            ObjType::Class => {
                let callee = callee.cast::<ObjClass>();

                // Replace the class that was called with an empty instance of that class.
                let instance = Value::obj(ObjInstance::new(gc, callee).cast::<Obj>());
                self.set(argc + 1, instance);

                // Init!
                let klass = unsafe { callee.as_ref() };
                if let Some(init) = klass.methods.get(self.init_string) {
                    assert!(init.is_obj_type(ObjType::Closure));
                    let init = unsafe { init.as_obj::<ObjClosure>() };
                    return self.call(init, argc);
                } else if argc != 0 {
                    return Err(
                        self.runtime_error(format!("Expected 0 arguments but got {}.", argc))
                    );
                }
                Ok(())
            }
            ObjType::Closure => {
                let callee = callee.cast::<ObjClosure>();
                self.call(callee, argc)
            }
            ObjType::Native => {
                let callee = callee.cast::<ObjNative>();
                let func = unsafe { callee.as_ref() }.func;
                let argv = self.sp(argc);
                let res = func(argc, argv);

                // Pop the whole call at once and then push the result.
                self.popn(argc + 1);
                self.push(res);
                Ok(())
            }
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    pub(crate) fn invoke_from_class(
        &mut self,
        klass: NonNull<ObjClass>,
        name: NonNull<ObjString>,
        argc: usize,
    ) -> RuntimeResult<()> {
        let klass = unsafe { klass.as_ref() };
        let Some(method) = klass.methods.get(name) else {
            return Err(
                self.runtime_error(format!("Undefined property '{}'.", unsafe {
                    name.as_ref()
                })),
            );
        };

        assert!(method.is_obj_type(ObjType::Closure));
        let method = unsafe { method.as_obj::<ObjClosure>() };
        self.call(method, argc)
    }

    pub(crate) fn invoke(&mut self, name: NonNull<ObjString>, argc: usize) -> RuntimeResult<()> {
        let receiver = self.peek(argc);
        if !receiver.is_obj_type(ObjType::Instance) {
            return Err(self.runtime_error("Can't call method on non-instance."));
        }
        let instance = unsafe { receiver.as_obj::<ObjInstance>().as_ref() };

        if let Some(value) = instance.fields.get(name) {
            // Turns out this was `obj.field(...)`, so replace the receiver with the
            // field value and then call it.
            self.set(argc + 1, value);
            return self.call_value(value, argc);
        }

        self.invoke_from_class(instance.klass, name, argc)
    }

    pub(crate) fn call(
        &mut self,
        mut closure: NonNull<ObjClosure>,
        argc: usize,
    ) -> RuntimeResult<()> {
        let func = unsafe { closure.as_mut().function.as_mut() };
        if argc != func.arity {
            return Err(self.runtime_error(format!(
                "Expected {} arguments but got {}.",
                func.arity, argc
            )));
        }

        if self.frames.len() == FRAMES_MAX {
            return Err(self.runtime_error("Stack overflow."));
        }

        let frame = CallFrame {
            closure,
            ip: 0,
            // Subtract an extra slot for stack slot zero (which contains the caller).
            slots: self.sp(argc + 1),
        };
        self.frames.push(frame);

        Ok(())
    }

    pub(crate) fn define_method(&mut self, name: NonNull<ObjString>) {
        let gc = Gc::runtime(self);

        let method = self.peek(0);
        let mut klass = unsafe { self.peek(1).as_obj::<ObjClass>() };
        unsafe { klass.as_mut() }.methods.set(gc, name, method);
        self.pop(); // method
    }

    pub(crate) fn bind_method(
        &mut self,
        klass: NonNull<ObjClass>,
        name: NonNull<ObjString>,
    ) -> RuntimeResult<()> {
        let klass = unsafe { klass.as_ref() };

        let gc = Gc::runtime(self);

        let Some(method) = klass.methods.get(name) else {
            return Err(
                self.runtime_error(format!("Undefined property '{}'.", unsafe {
                    name.as_ref()
                })),
            );
        };

        assert!(method.is_obj_type(ObjType::Closure));

        let bound = ObjBoundMethod::new(gc, self.peek(0), unsafe { method.as_obj::<ObjClosure>() });
        self.pop(); // method
        self.push(Value::obj(bound.cast::<Obj>()));
        Ok(())
    }

    pub(crate) fn define_native(&mut self, name: &'static str, func: NativeFn) {
        let gc = Gc::runtime(self);

        // GC: Ensure the name and value objects are reachable in case resizing the
        // table triggers garbage collection.
        //
        // TODO: Use Alloc stash instead
        {
            self.push(Value::obj(ObjString::from_str(gc, name).cast::<Obj>()));
            self.push(Value::obj(ObjNative::new(gc, func).cast::<Obj>()));
            self.globals.set(
                gc,
                unsafe { self.stack[0].as_obj::<ObjString>() },
                self.stack[1],
            );
            self.pop();
            self.pop();
        }
    }
}

// Upvalues
impl Vm<'_> {
    pub(crate) fn capture_upvalue(&mut self, local: NonNull<Value>) -> NonNull<ObjUpvalue> {
        // Keep the list sorted by pointer value for early exits to searches.
        //
        // `prev` will be the node just before the one we want.
        //
        // `upvalue` will be either the node we want or the one that would be after it
        let mut prev = None;
        let mut upvalue = self.open_upvalues;
        loop {
            let Some(current) = upvalue else { break };
            let Some(location) = unsafe { current.as_ref() }.location else {
                break;
            };
            if location <= local {
                break;
            }

            upvalue = unsafe { current.as_ref() }.next;
            prev = Some(current);
        }

        if let Some(current) = upvalue {
            let location = unsafe { current.as_ref() }.location.unwrap();
            if location == local {
                return current;
            }
        }

        let gc = Gc::runtime(self);

        // Linked-list insert between `prev` and `upvalue` (next).
        let mut created = ObjUpvalue::new(gc, Some(local));
        unsafe { created.as_mut() }.next = upvalue;
        if let Some(mut prev) = prev {
            unsafe { prev.as_mut() }.next = Some(created);
        } else {
            self.open_upvalues = Some(created);
        }
        created
    }

    pub(crate) fn close_upvalues(&mut self, last: NonNull<Value>) {
        while let Some(mut upvalue) = self.open_upvalues {
            let upvalue = unsafe { upvalue.as_mut() };
            let Some(location) = upvalue.location else {
                break;
            };
            if location < last {
                break;
            };

            // TODO: Self-referential struct should probably be pinned.
            upvalue.closed = unsafe { *location.as_ref() };
            upvalue.location = NonNull::new(addr_of_mut!(upvalue.closed));
            self.open_upvalues = upvalue.next;
        }
    }
}

// Execution
impl Vm<'_> {
    pub fn interpret(&mut self, source: &str) -> InterpretResult<()> {
        let gc = Gc::runtime(self);

        let function = compile(self, source)?;

        // GC: Temporarily make the function reachable.
        let closure = {
            self.push(Value::obj(function.cast::<Obj>()));
            let closure = ObjClosure::new(gc, function);
            self.pop();
            closure
        };

        self.push(Value::obj(closure.cast::<Obj>()));
        self.call(closure, 0)?;

        Ok(self.run()?)
    }

    #[allow(unused_unsafe)]
    pub(crate) fn run(&mut self) -> Result<(), RuntimeError> {
        let gc = Gc::runtime(self);

        macro_rules! last_frame {
            () => {{
                NonNull::from(self.frames.last_mut().unwrap())
            }};
        }

        let mut frame: NonNull<CallFrame> = last_frame!();

        macro_rules! read_byte {
            () => {{
                let ip = unsafe { frame.as_ref() }.ip;
                let b = unsafe { frame.as_ref().closure.as_ref().function.as_ref() }
                    .chunk
                    .code[ip];
                unsafe { frame.as_mut().ip += 1 };
                b
            }};
        }

        macro_rules! read_short {
            () => {{
                let hi: u16 = read_byte!().into();
                let lo: u16 = read_byte!().into();
                (hi << 8) | lo
            }};
        }

        macro_rules! read_constant {
            () => {{
                let idx = read_byte!();
                unsafe { (frame.as_ref().closure.as_ref()).function.as_ref() }
                    .chunk
                    .constants[idx as usize]
            }};
        }

        macro_rules! read_string {
            () => {{
                unsafe {
                    let value: crate::value::Value = read_constant!();
                    value.as_obj::<ObjString>()
                }
            }};
        }

        macro_rules! compare_op {
            ($op:expr) => {{
                if self.peek(0).is_number() && self.peek(1).is_number() {
                    let b = self.pop().try_number().expect("peek");
                    let a = self.pop().try_number().expect("peek");
                    let v = $op(&a, &b);
                    self.push(Value::bool(v));
                    Ok(())
                } else {
                    Err(self.runtime_error("Operands must be numbers."))
                }
            }};
        }

        macro_rules! binary_op {
            ($op:expr) => {{
                if self.peek(0).is_number() && self.peek(1).is_number() {
                    let b = self.pop().try_number().expect("peek");
                    let a = self.pop().try_number().expect("peek");
                    let v = $op(a, b);
                    self.push(Value::number(v));
                    Ok(())
                } else {
                    Err(self.runtime_error("Operands must be numbers."))
                }
            }};
        }

        loop {
            #[cfg(feature = "trace_execution")]
            {
                for value in &self.stack {
                    write!(self.opts.stderr, "[ {} ]", value).unwrap();
                }
                writeln!(self.opts.stderr).unwrap();
            }

            let opcode: Opcode = match read_byte!().try_into() {
                Ok(opcode) => opcode,
                Err(err) => panic!("unknown opcode: {}", err.opcode),
            };

            #[cfg(feature = "trace_execution")]
            unsafe {
                let frame = unsafe { frame.as_ref() };
                let closure = unsafe { frame.closure.as_ref() };
                let func = unsafe { closure.function.as_ref() };

                // The ip has already moved past the instruction that failed, so subtract
                // one extra.
                func.chunk
                    .disassemble_instruction(frame.ip - 1, &mut self.opts.stderr)
                    .unwrap();
            }

            match opcode {
                Opcode::Constant => {
                    self.push(read_constant!());
                }

                Opcode::Nil => {
                    self.push(Value::nil());
                }
                Opcode::True => {
                    self.push(Value::bool(true));
                }
                Opcode::False => {
                    self.push(Value::bool(false));
                }

                Opcode::Pop => {
                    self.pop();
                }

                Opcode::GetLocal => {
                    let slot = read_byte!();
                    let value = unsafe { *frame.as_ref().slots.as_ptr().offset(slot as isize) };
                    self.push(value);
                }
                Opcode::SetLocal => {
                    let slot = read_byte!();
                    let value = self.peek(0);
                    unsafe { *frame.as_ref().slots.as_ptr().offset(slot as isize) = value };
                }

                Opcode::DefineGlobal => {
                    let name = read_string!();

                    // GC: Leave the value on the stack temporarily in case this triggers
                    // garbage collection via table resize.
                    let value = self.peek(0);
                    self.globals.set(gc, name, value);
                    self.pop();
                }
                Opcode::GetGlobal => {
                    let name = read_string!();
                    let Some(value) = self.globals.get(name) else {
                        return Err(self
                            .runtime_error(format!("Undefined variable '{}'.", unsafe {
                                name.as_ref()
                            })));
                    };

                    self.push(value);
                }
                Opcode::SetGlobal => {
                    let name = read_string!();

                    let value = self.peek(0);
                    let new_key = self.globals.set(gc, name, value);
                    if new_key {
                        // If this was a new key, that means the global wasn't actually defined
                        // yet! Delete it back out and throw the runtime error.
                        self.globals.delete(name);
                        return Err(self
                            .runtime_error(format!("Undefined variable '{}'.", unsafe {
                                name.as_ref()
                            })));
                    }
                }

                Opcode::GetUpvalue => {
                    let slot = read_byte!();
                    let upvalue =
                        unsafe { frame.as_ref().closure.as_ref() }.upvalues[slot as usize].unwrap();
                    let location = unsafe { upvalue.as_ref().location.unwrap() };
                    self.push(unsafe { *location.as_ref() });
                }
                Opcode::SetUpvalue => {
                    let slot = read_byte!();
                    let value = self.peek(0);
                    let mut upvalue =
                        unsafe { frame.as_mut().closure.as_mut() }.upvalues[slot as usize].unwrap();
                    *unsafe { upvalue.as_mut().location.unwrap().as_mut() } = value;
                }

                Opcode::GetProperty => {
                    if !self.peek(0).is_obj_type(ObjType::Instance) {
                        return Err(self.runtime_error("Only instances have properties."));
                    }

                    let instance = unsafe { self.peek(0).as_obj::<ObjInstance>() };
                    let name = read_string!();

                    // Check fields first because they can shadow methods.
                    if let Some(value) = unsafe { instance.as_ref() }.fields.get(name) {
                        self.pop(); // instance
                        self.push(value);
                        continue;
                    };

                    // Method
                    let klass = unsafe { instance.as_ref() }.klass;
                    self.bind_method(klass, name)?;
                }
                Opcode::SetProperty => {
                    if !self.peek(1).is_obj_type(ObjType::Instance) {
                        return Err(self.runtime_error("Only instances have fields."));
                    }

                    let mut instance = unsafe { self.peek(1).as_obj::<ObjInstance>() };
                    let name = read_string!();

                    let fields = &mut unsafe { instance.as_mut() }.fields;
                    fields.set(gc, name, self.peek(0));

                    let value = self.pop();
                    self.pop(); // instance
                    self.push(value);
                }
                Opcode::GetSuper => {
                    let name = read_string!();
                    let superclass = unsafe { self.pop().as_obj::<ObjClass>() };
                    self.bind_method(superclass, name)?;
                }

                Opcode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::bool(a == b));
                }
                Opcode::Greater => compare_op!(PartialOrd::gt)?,
                Opcode::Less => compare_op!(PartialOrd::lt)?,

                Opcode::Not => {
                    let a = self.pop();
                    self.push(Value::bool(a.is_falsey()));
                }

                Opcode::Neg => {
                    if !self.peek(0).is_number() {
                        return Err(self.runtime_error("Operand must be a number."));
                    }
                    let a = self.pop().try_number().expect("peek");
                    self.push(Value::number(-a));
                }

                Opcode::Add => {
                    if self.peek(0).is_obj_type(ObjType::String)
                        && self.peek(1).is_obj_type(ObjType::String)
                    {
                        // GC: Keep the source strings reachable while allocating the result in
                        // case that triggers garbage collection.
                        {
                            let b = unsafe { self.peek(0).as_obj::<ObjString>() };
                            let a = unsafe { self.peek(1).as_obj::<ObjString>() };
                            let res = ObjString::concatenate(gc, a, b);
                            self.pop(); // b
                            self.pop(); // a
                            self.push(Value::obj(res.cast::<Obj>()));
                        }
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let b = self.pop().try_number().expect("peek");
                        let a = self.pop().try_number().expect("peek");
                        self.push(Value::number(a + b));
                    } else {
                        return Err(
                            self.runtime_error("Operands must be two numbers or two strings.")
                        );
                    }
                }
                Opcode::Sub => {
                    binary_op!(std::ops::Sub::<f64>::sub)?;
                }
                Opcode::Mul => {
                    binary_op!(std::ops::Mul::<f64>::mul)?;
                }
                Opcode::Div => {
                    binary_op!(std::ops::Div::<f64>::div)?;
                }
                Opcode::Rem => {
                    binary_op!(std::ops::Rem::<f64>::rem)?;
                }

                Opcode::Print => {
                    let v = self.pop();
                    writeln!(self.opts.stdout, "{}", v).unwrap();
                }

                Opcode::Jump => {
                    let offset = read_short!();
                    unsafe { frame.as_mut().ip += offset as usize };
                }
                Opcode::JumpIfFalse => {
                    let offset = read_short!();
                    if self.peek(0).is_falsey() {
                        unsafe { frame.as_mut().ip += offset as usize };
                    }
                }
                Opcode::Loop => {
                    let offset = read_short!();
                    unsafe { frame.as_mut().ip -= offset as usize };
                }

                Opcode::Call => {
                    let argc = read_byte!() as usize;
                    let callee = self.peek(argc);
                    self.call_value(callee, argc)?;
                    frame = last_frame!();
                }
                Opcode::Invoke => {
                    let method = read_string!();
                    let argc = read_byte!() as usize;
                    self.invoke(method, argc)?;
                    frame = last_frame!();
                }
                Opcode::SuperInvoke => {
                    let method = read_string!();
                    let argc = read_byte!() as usize;
                    let superclass = unsafe { self.pop().as_obj::<ObjClass>() };
                    self.invoke_from_class(superclass, method, argc)?;
                    frame = last_frame!();
                }
                Opcode::Closure => {
                    let func = unsafe { read_constant!().as_obj::<ObjFunction>() };
                    let mut closure = ObjClosure::new(gc, func);
                    self.push(Value::obj(closure.cast::<Obj>()));

                    let count = unsafe { closure.as_ref() }.upvalues.len();
                    for i in 0..count {
                        let is_local = read_byte!();
                        let index = read_byte!();

                        if is_local != 0 {
                            let slot = unsafe { frame.as_ref().slots.as_ptr().add(index as usize) };
                            let slot = NonNull::new(slot).unwrap();
                            unsafe { closure.as_mut() }.upvalues[i] =
                                Some(self.capture_upvalue(slot));
                        } else {
                            unsafe {
                                closure.as_mut().upvalues[i] =
                                    frame.as_mut().closure.as_mut().upvalues[index as usize];
                            };
                        }
                    }
                }
                Opcode::CloseUpvalue => {
                    let last = self.sp(1);
                    self.close_upvalues(last);
                    self.pop();
                }
                Opcode::Return => {
                    let res = self.pop();
                    self.close_upvalues(unsafe { frame.as_ref() }.slots);

                    self.frames.pop();
                    if self.frames.is_empty() {
                        self.pop();
                        return Ok(());
                    }

                    self.set_sp(unsafe { frame.as_ref().slots });
                    self.push(res);
                    frame = last_frame!();
                }

                Opcode::Class => {
                    let name = read_string!();
                    let klass = ObjClass::new(gc, name);
                    self.push(Value::obj(klass.cast::<Obj>()));
                }
                Opcode::Inherit => {
                    let superclass = self.peek(1);
                    if !superclass.is_obj_type(ObjType::Class) {
                        return Err(self.runtime_error("Superclass must be a class."));
                    }

                    // Classes are closed after definition, so the set of methods on a class
                    // can never change. Flattening all methods onto the subclass saves time
                    // on lookups.
                    let mut subclass = unsafe { self.peek(0).as_obj::<ObjClass>() };
                    let superclass = unsafe { superclass.as_obj::<ObjClass>() };

                    let submethods = &mut unsafe { subclass.as_mut() }.methods;
                    let supermethods = &unsafe { superclass.as_ref() }.methods;
                    submethods.extend(gc, supermethods);

                    self.pop(); // subclass
                }
                Opcode::Method => {
                    let name = read_string!();
                    self.define_method(name);
                }
            }
        }
    }
}

impl Vm<'_> {
    pub(crate) fn mark_roots(&mut self) {
        debug_log_gc!("---- mark slots");
        for value in &self.stack {
            self.heap.mark_value(*value);
        }

        debug_log_gc!("---- mark frames");
        for frame in &mut self.frames {
            let obj = frame.closure.cast::<Obj>();
            self.heap.mark_obj(obj);
        }

        debug_log_gc!("---- mark upvalues");
        let mut upvalue = self.open_upvalues;
        while let Some(current) = upvalue {
            self.heap.mark_obj(current.cast::<Obj>());
            upvalue = unsafe { current.as_ref() }.next;
        }

        debug_log_gc!("---- mark globals");
        self.heap.mark_table(&mut self.globals);

        debug_log_gc!("---- mark init string");
        self.heap.mark_obj(self.init_string.cast::<Obj>());
    }
}
