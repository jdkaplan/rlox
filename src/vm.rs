use std::mem::MaybeUninit;
use std::ptr;
use std::time::Duration;

use crate::alloc::Gc;
use crate::chunk::Opcode;
use crate::compiler::{compile, CompileError};
use crate::object::{NativeFn, Obj, ObjClosure, ObjNative, ObjString, ObjType, ObjUpvalue, *};
use crate::table::Table;
use crate::value::{Value, ValueType};
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

pub fn clock_native(_argc: usize, _argv: *const Value) -> Value {
    Value::number(clock().as_secs_f64())
}

fn clock() -> Duration {
    let tp = unsafe {
        let mut tp = std::mem::MaybeUninit::uninit();
        if libc::clock_gettime(libc::CLOCK_PROCESS_CPUTIME_ID, tp.as_mut_ptr()) != 0 {
            panic!("clock: {}", std::io::Error::last_os_error());
        }
        tp.assume_init()
    };

    Duration::new(tp.tv_sec as u64, tp.tv_nsec as u32)
}

#[repr(C)]
pub struct CallFrame {
    pub(crate) closure: *mut ObjClosure,
    ip: *mut u8,
    slots: *mut Value,
}

#[repr(C)]
pub struct Vm {
    // TODO: This is a Vec
    pub(crate) frames: [CallFrame; FRAMES_MAX],
    pub(crate) frame_count: usize,

    // TODO: This is a Vec
    pub(crate) stack: [Value; STACK_MAX],
    pub(crate) stack_top: *mut Value,

    pub(crate) globals: Table,

    pub(crate) strings: Table,
    pub(crate) init_string: *mut ObjString,

    pub(crate) open_upvalues: *mut ObjUpvalue,

    pub(crate) objects: *mut Obj,

    pub(crate) gc_pending: Vec<*mut Obj>,

    pub(crate) bytes_allocated: usize,
    pub(crate) next_gc: usize,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

// Alloc
impl Vm {
    pub fn new() -> Self {
        let mut vm = MaybeUninit::uninit();
        unsafe {
            let ptr: *mut Self = vm.as_mut_ptr();
            Self::init(ptr.as_mut().unwrap());
            vm.assume_init()
        }
    }

    fn zero(&mut self) {
        self.reset_stack();

        self.bytes_allocated = 0;
        self.next_gc = 0;

        // TODO: This is a Vec
        self.gc_pending = Vec::new();

        self.globals = Table::new();
        self.strings = Table::new();
        self.init_string = ptr::null_mut();

        self.open_upvalues = ptr::null_mut();
        self.objects = ptr::null_mut();
    }

    pub(crate) fn reset_stack(&mut self) {
        self.stack_top = ptr::addr_of_mut!(self.stack[0]);
        self.frame_count = 0;
        self.open_upvalues = ptr::null_mut();
    }

    pub(crate) fn init(&mut self) {
        self.zero();

        self.next_gc = 1024 * 1024;

        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        self.init_string = ObjString::from_str(gc, "init");

        self.define_native("clock", clock_native);
    }

    pub(crate) fn free(&mut self) {
        let mut gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        std::mem::take(&mut self.globals);
        std::mem::take(&mut self.strings);

        self.init_string = ptr::null_mut();

        // GC: This is going to free every known object managed by this VM. This
        // somehow tries to free more bytes than the VM ever allocated. Give this a
        // null VM to avoid segfaulting when that happens.
        gc.vm = ptr::null_mut();
        gc.free_objects(self.objects);
        std::mem::take(&mut self.gc_pending);
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        self.free()
    }
}

// Errors
impl Vm {
    #[must_use]
    pub(crate) fn runtime_error(&mut self, msg: impl AsRef<str>) -> RuntimeError {
        // TODO: Print error message after stack trace.
        eprintln!("{}", msg.as_ref());
        self.eprint_stack_trace();
        self.reset_stack();
        RuntimeError
    }

    pub(crate) fn eprint_stack_trace(&mut self) {
        for i in (0..self.frame_count).rev() {
            let frame = &self.frames[i];
            let closure = unsafe { frame.closure.as_ref().unwrap() };
            let func = unsafe { closure.function.as_ref().unwrap() };

            // The ip has already moved past the instruction that failed, so subtract
            // one extra.
            let base = ptr::addr_of!(func.chunk.code[0]);
            let instruction = unsafe { frame.ip.offset_from(base) - 1 } as usize;
            let line = func.chunk.lines[instruction];
            eprintln!("[line {}] in {}", line, func.name());
        }
    }
}

// Stack management
impl Vm {
    pub(crate) fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }

    pub(crate) fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            *self.stack_top
        }
    }

    pub(crate) fn peek(&mut self, offset: usize) -> Value {
        // Offset by one extra because stack_top points to the first _unused_ slot.
        unsafe { *self.stack_top.sub(offset + 1) }
    }
}

// Calls
impl Vm {
    pub(crate) fn call_value(&mut self, callee: Value, argc: usize) -> RuntimeResult<()> {
        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        if callee.r#type != ValueType::Obj {
            return Err(self.runtime_error("Can only call functions and classes."));
        }

        let callee = unsafe { callee.r#as.obj };
        match unsafe { callee.as_ref().unwrap() }.r#type {
            ObjType::BoundMethod => {
                let callee = callee as *mut ObjBoundMethod;
                let bound = unsafe { callee.as_ref().unwrap() };
                unsafe { *self.stack_top.sub(argc + 1) = bound.receiver };
                self.call(bound.method, argc)
            }
            ObjType::Class => {
                let callee = callee as *mut ObjClass;

                // Replace the class that was called with an empty instance of that class.
                let instance = Value::obj(ObjInstance::new(gc, callee) as *mut Obj);
                unsafe { *self.stack_top.sub(argc + 1) = instance };

                // Init!
                let klass = unsafe { callee.as_ref().unwrap() };
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
                let callee = callee as *mut ObjClosure;
                self.call(callee, argc)
            }
            ObjType::Native => {
                let callee = callee as *mut ObjNative;
                let func = unsafe { callee.as_ref().unwrap() }.r#fn;
                let argv = unsafe { self.stack_top.sub(argc) };
                let res = func(argc, argv);

                // Pop the whole call at once an then push on the result.
                self.stack_top = unsafe { self.stack_top.sub(argc + 1) };
                self.push(res);
                Ok(())
            }
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    pub(crate) fn invoke_from_class(
        &mut self,
        klass: *const ObjClass,
        name: *const ObjString,
        argc: usize,
    ) -> RuntimeResult<()> {
        let klass = unsafe { klass.as_ref().unwrap() };
        let Some(method) = klass.methods.get(name) else {
            return Err(
                self.runtime_error(format!("Undefined property '{}'.", unsafe {
                    name.as_ref().unwrap()
                })),
            );
        };

        assert!(method.is_obj_type(ObjType::Closure));
        let method = unsafe { method.as_obj::<ObjClosure>() };
        self.call(method, argc)
    }

    pub(crate) fn invoke(&mut self, name: *const ObjString, argc: usize) -> RuntimeResult<()> {
        let receiver = self.peek(argc);
        if !receiver.is_obj_type(ObjType::Instance) {
            return Err(self.runtime_error("Can't call method on non-instance."));
        }
        let instance = unsafe { receiver.as_obj::<ObjInstance>().as_ref().unwrap() };

        if let Some(value) = instance.fields.get(name) {
            // Turns out this was `obj.field(...)`, so replace the receiver with the
            // field value and then call it.
            unsafe { *self.stack_top.sub(argc + 1) = value };
            return self.call_value(value, argc);
        }

        self.invoke_from_class(instance.klass, name, argc)
    }

    pub(crate) fn call(&mut self, closure: *mut ObjClosure, argc: usize) -> RuntimeResult<()> {
        let func = unsafe { closure.as_mut().unwrap().function.as_mut().unwrap() };
        if argc != func.arity {
            return Err(self.runtime_error(format!(
                "Expected {} arguments but got {}.",
                func.arity, argc
            )));
        }

        if self.frame_count == FRAMES_MAX {
            return Err(self.runtime_error("Stack overflow."));
        }

        let frame = &mut self.frames[self.frame_count];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = ptr::addr_of_mut!(func.chunk.code[0]);
        // Subtract an extra slot for stack slot zero (which contains the caller).
        frame.slots = unsafe { self.stack_top.sub(argc + 1) };
        Ok(())
    }

    pub(crate) fn define_method(&mut self, name: *mut ObjString) {
        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        let method = self.peek(0);
        let klass = unsafe { self.peek(1).as_obj::<ObjClass>() };
        unsafe { &mut *klass }.methods.set(gc, name, method);
        self.pop(); // method
    }

    pub(crate) fn bind_method(
        &mut self,
        klass: *const ObjClass,
        name: *const ObjString,
    ) -> RuntimeResult<()> {
        let klass = unsafe { klass.as_ref().unwrap() };

        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        let Some(method) = klass.methods.get(name) else {
            return Err(
                self.runtime_error(format!("Undefined property '{}'.", unsafe {
                    name.as_ref().unwrap()
                })),
            );
        };

        assert!(method.is_obj_type(ObjType::Closure));

        let bound = ObjBoundMethod::new(gc, self.peek(0), unsafe { method.as_obj::<ObjClosure>() });
        self.pop(); // method
        self.push(Value::obj(bound as *mut Obj));
        Ok(())
    }

    pub(crate) fn define_native(&mut self, name: &'static str, func: NativeFn) {
        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        // GC: Ensure the name and value objects are reachable in case resizing the
        // table triggers garbage collection.
        //
        // TODO: Use Alloc stash instead
        {
            self.push(Value::obj(ObjString::from_str(gc, name) as *mut Obj));
            self.push(Value::obj(ObjNative::new(gc, func) as *mut Obj));
            self.globals.set(
                gc,
                unsafe { self.stack[0].r#as.obj as *mut ObjString },
                self.stack[1],
            );
            self.pop();
            self.pop();
        }
    }
}

// Upvalues
impl Vm {
    pub(crate) fn capture_upvalue(&mut self, local: *mut Value) -> *mut ObjUpvalue {
        // Keep the list sorted by pointer value for early exits to searches.
        //
        // `prev` will be the node just before the one we want.
        //
        // `upvalue` will be either the node we want or the one that would be after it
        let mut prev = ptr::null_mut();
        let mut upvalue = self.open_upvalues;
        while !upvalue.is_null() && unsafe { &*upvalue }.location > local {
            prev = upvalue;
            upvalue = unsafe { &*upvalue }.next;
        }

        if !upvalue.is_null() && unsafe { &*upvalue }.location == local {
            return upvalue;
        }

        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        // Linked-list insert between `prev` and `upvalue` (next).
        let created = ObjUpvalue::new(gc, local);
        unsafe { (*created).next = upvalue };
        if prev.is_null() {
            self.open_upvalues = created;
        } else {
            unsafe { (*prev).next = created };
        }
        created
    }

    pub(crate) fn close_upvalues(&mut self, last: *mut Value) {
        while !self.open_upvalues.is_null() && unsafe { &*self.open_upvalues }.location >= last {
            let upvalue = self.open_upvalues;
            unsafe {
                (*upvalue).closed = *(*upvalue).location;
                (*upvalue).location = ptr::addr_of_mut!((*upvalue).closed);
                self.open_upvalues = (*upvalue).next;
            }
        }
    }
}

// Execution
impl Vm {
    pub fn interpret(&mut self, source: &str) -> InterpretResult<()> {
        let gc = Gc::new(ptr::null_mut(), self);

        let function = compile(self, source)?;

        // GC: Temporarily make the function reachable.
        let closure = {
            self.push(Value::obj(function as *mut Obj));
            let closure = ObjClosure::new(gc, function);
            self.pop();
            closure
        };

        self.push(Value::obj(closure as *mut Obj));
        self.call(closure, 0)?;

        Ok(self.run()?)
    }

    #[allow(unused_unsafe)]
    pub(crate) fn run(&mut self) -> Result<(), RuntimeError> {
        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        macro_rules! frame_at {
            ($idx:expr) => {{
                unsafe { ::std::ptr::addr_of_mut!(self.frames[0]).add($idx) }
            }};
        }

        let mut frame: *mut CallFrame = frame_at![self.frame_count - 1];

        macro_rules! read_byte {
            () => {{
                let b = unsafe { *(*frame).ip };
                unsafe { (*frame).ip = (*frame).ip.add(1) };
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
                let constants = &unsafe { &*(&*(*frame).closure).function }.chunk.constants;
                unsafe { constants[idx as usize] }
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
                    let b = unsafe { self.pop().as_number() };
                    let a = unsafe { self.pop().as_number() };
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
                    let b = unsafe { self.pop().as_number() };
                    let a = unsafe { self.pop().as_number() };
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
                let mut slot = ptr::addr_of_mut!(self.stack[0]);
                while slot < self.stack_top {
                    print!("[ {} ]", unsafe { *slot });
                    slot = unsafe { slot.add(1) };
                }
                println!();
            }

            let opcode: Opcode = match read_byte!().try_into() {
                Ok(opcode) => opcode,
                Err(err) => panic!("unknown opcode: {}", err.opcode),
            };

            #[cfg(feature = "trace_execution")]
            unsafe {
                let frame = unsafe { frame.as_ref().unwrap() };
                let closure = unsafe { frame.closure.as_ref().unwrap() };
                let func = unsafe { closure.function.as_ref().unwrap() };

                // The ip has already moved past the instruction that failed, so subtract
                // one extra.
                let base = ptr::addr_of!(func.chunk.code[0]);
                let instruction = unsafe { frame.ip.offset_from(base) - 1 } as usize;
                func.chunk.disassemble_instruction(instruction);
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
                    let value = unsafe { *((*frame).slots.offset(slot as isize)) };
                    self.push(value);
                }
                Opcode::SetLocal => {
                    let slot = read_byte!();
                    let value = self.peek(0);
                    unsafe { *((*frame).slots.offset(slot as isize)) = value };
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
                                &*name
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
                                &*name
                            })));
                    }
                }

                Opcode::GetUpvalue => {
                    let slot = read_byte!();
                    let value = unsafe {
                        *(*(*(*(*frame).closure).upvalues.offset(slot as isize))).location
                    };
                    self.push(value);
                }
                Opcode::SetUpvalue => {
                    let slot = read_byte!();
                    let value = self.peek(0);
                    unsafe {
                        *(*(*(*(*frame).closure).upvalues.offset(slot as isize))).location = value
                    };
                }

                Opcode::GetProperty => {
                    if !self.peek(0).is_obj_type(ObjType::Instance) {
                        return Err(self.runtime_error("Only instances have properties."));
                    }

                    let instance = unsafe { self.peek(0).as_obj::<ObjInstance>() };
                    let name = read_string!();

                    // Check fields first because they can shadow methods.
                    if let Some(value) = unsafe { &*instance }.fields.get(name) {
                        self.pop(); // instance
                        self.push(value);
                        continue;
                    };

                    // Method
                    let klass = unsafe { &*instance }.klass;
                    self.bind_method(klass, name)?;
                }
                Opcode::SetProperty => {
                    if !self.peek(1).is_obj_type(ObjType::Instance) {
                        return Err(self.runtime_error("Only instances have fields."));
                    }

                    let instance = unsafe { self.peek(1).as_obj::<ObjInstance>() };
                    let name = read_string!();

                    let fields = &mut unsafe { &mut *instance }.fields;
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
                    let a = unsafe { self.pop().as_number() };
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
                            self.push(Value::obj(res as *mut Obj));
                        }
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let b = unsafe { self.pop().as_number() };
                        let a = unsafe { self.pop().as_number() };
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

                Opcode::Print => {
                    let v = self.pop();
                    println!("{}", v);
                }

                Opcode::Jump => {
                    let offset = read_short!();
                    unsafe { (*frame).ip = (*frame).ip.add(offset as usize) };
                }
                Opcode::JumpIfFalse => {
                    let offset = read_short!();
                    if self.peek(0).is_falsey() {
                        unsafe { (*frame).ip = (*frame).ip.add(offset as usize) };
                    }
                }
                Opcode::Loop => {
                    let offset = read_short!();
                    unsafe { (*frame).ip = (*frame).ip.sub(offset as usize) };
                }

                Opcode::Call => {
                    let argc = read_byte!() as usize;
                    let callee = self.peek(argc);
                    self.call_value(callee, argc)?;
                    frame = frame_at![self.frame_count - 1];
                }
                Opcode::Invoke => {
                    let method = read_string!();
                    let argc = read_byte!() as usize;
                    self.invoke(method, argc)?;
                    frame = frame_at![self.frame_count - 1];
                }
                Opcode::SuperInvoke => {
                    let method = read_string!();
                    let argc = read_byte!() as usize;
                    let superclass = unsafe { self.pop().as_obj::<ObjClass>() };
                    self.invoke_from_class(superclass, method, argc)?;
                    frame = frame_at![self.frame_count - 1];
                }
                Opcode::Closure => {
                    let func = unsafe { read_constant!().as_obj::<ObjFunction>() };
                    let closure = ObjClosure::new(gc, unsafe { func.as_mut().unwrap() });
                    self.push(Value::obj(closure as *mut Obj));

                    let count = unsafe { &*closure }.upvalue_count as usize;
                    for i in 0..count {
                        let is_local = read_byte!();
                        let index = read_byte!();

                        if is_local != 0 {
                            unsafe {
                                *(*closure).upvalues.add(i) =
                                    self.capture_upvalue((*frame).slots.add(index as usize))
                            };
                        } else {
                            unsafe {
                                *(*closure).upvalues.add(i) =
                                    *(*(*frame).closure).upvalues.add(index as usize)
                            };
                        }
                    }
                }
                Opcode::CloseUpvalue => {
                    self.close_upvalues(unsafe { self.stack_top.sub(1) });
                    self.pop();
                }
                Opcode::Return => {
                    let res = self.pop();
                    self.close_upvalues(unsafe { &*frame }.slots);

                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.stack_top = unsafe { &*frame }.slots;
                    self.push(res);
                    frame = &mut self.frames[self.frame_count - 1];
                }

                Opcode::Class => {
                    let name = read_string!();
                    let klass = ObjClass::new(gc, name);
                    self.push(Value::obj(klass as *mut Obj));
                }
                Opcode::Inherit => {
                    let superclass = self.peek(1);
                    if !superclass.is_obj_type(ObjType::Class) {
                        return Err(self.runtime_error("Superclass must be a class."));
                    }

                    // Classes are closed after definition, so the set of methods on a class
                    // can never change. Flattening all methods onto the subclass saves time
                    // on lookups.
                    let subclass = unsafe { self.peek(0).as_obj::<ObjClass>() };
                    let superclass = unsafe { superclass.as_obj::<ObjClass>() };

                    let submethods = &mut unsafe { &mut *subclass }.methods;
                    let supermethods = &mut unsafe { &mut *superclass }.methods;
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
