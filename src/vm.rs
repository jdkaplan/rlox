use std::ffi::{c_char, c_uint, CStr, CString};
use std::mem::MaybeUninit;
use std::ptr;

use once_cell::sync::Lazy;

use crate::alloc::Gc;
use crate::object::{NativeFn, Obj, ObjClosure, ObjNative, ObjString, ObjType, ObjUpvalue, *};
use crate::table::Table;
use crate::value::{Value, ValueType};
use crate::{clock_native, concatenate, InterpretResult, Opcode, FRAMES_MAX, STACK_MAX};

#[repr(C)]
pub struct RuntimeError;

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
    pub(crate) frame_count: c_uint,

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

        self.globals.init();
        self.strings.init();
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

        static INIT_STR: Lazy<CString> = Lazy::new(|| CString::new("init").unwrap());
        self.init_string = ObjString::from_static(gc, &INIT_STR);

        static CLOCK_STR: Lazy<CString> = Lazy::new(|| CString::new("clock").unwrap());
        self.define_native(&CLOCK_STR, clock_native);
    }

    pub(crate) fn free(&mut self) {
        let mut gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        self.globals.free(&mut gc);
        self.strings.free(&mut gc);

        self.init_string = ptr::null_mut();

        // GC: This is going to free every known object managed by this VM. This
        // somehow tries to free more bytes than the VM ever allocated. Give this a
        // null VM to avoid segfaulting when that happens.
        gc.vm = ptr::null_mut();
        gc.free_objects(self.objects);
        std::mem::take(&mut self.gc_pending);
    }
}

// Errors
impl Vm {
    pub(crate) fn runtime_error(&mut self, msg: impl AsRef<str>) {
        eprintln!("{}", msg.as_ref());
        self.eprint_stack_trace();
        self.reset_stack();
    }

    pub(crate) fn eprint_stack_trace(&mut self) {
        for i in (0..self.frame_count as usize).rev() {
            let frame = &self.frames[i];
            let closure = unsafe { frame.closure.as_ref().unwrap() };
            let func = unsafe { closure.function.as_ref().unwrap() };

            // The ip has already moved past the instruction that failed, so subtract
            // one extra.
            let base = func.chunk.code.base_ptr();
            let instruction = unsafe { frame.ip.offset_from(base) - 1 } as c_uint;
            let line = func.chunk.lines.get(instruction);
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

    pub(crate) fn peek(&mut self, offset: c_uint) -> Value {
        // Offset by one extra because stack_top points to the first _unused_ slot.
        unsafe { *self.stack_top.sub(offset as usize + 1) }
    }
}

// Calls
impl Vm {
    pub(crate) fn call_value(&mut self, callee: Value, argc: c_uint) -> bool {
        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        if callee.r#type != ValueType::TObj {
            self.runtime_error("Can only call functions and classes.");
            return false;
        }

        let callee = unsafe { callee.r#as.obj };
        match unsafe { callee.as_ref().unwrap() }.r#type {
            ObjType::OBoundMethod => {
                let callee = callee as *mut ObjBoundMethod;
                let bound = unsafe { callee.as_ref().unwrap() };
                unsafe { *self.stack_top.sub(argc as usize + 1) = bound.receiver };
                self.call(bound.method, argc)
            }
            ObjType::OClass => {
                let callee = callee as *mut ObjClass;

                // Replace the class that was called with an empty instance of that class.
                let instance = Value::obj(ObjInstance::new(gc, callee) as *mut Obj);
                unsafe { *self.stack_top.sub(argc as usize + 1) = instance };

                // Init!
                let klass = unsafe { callee.as_ref().unwrap() };
                if let Some(init) = klass.methods.get(self.init_string) {
                    assert!(init.is_obj_type(ObjType::OClosure));
                    let init = unsafe { init.as_obj::<ObjClosure>() };
                    return self.call(init, argc);
                } else if argc != 0 {
                    self.runtime_error(format!("Expected 0 arguments but got {}.", argc));
                    return false;
                }
                true
            }
            ObjType::OClosure => {
                let callee = callee as *mut ObjClosure;
                self.call(callee, argc)
            }
            ObjType::ONative => {
                let callee = callee as *mut ObjNative;
                let func = unsafe { callee.as_ref().unwrap() }.r#fn;
                let argv = unsafe { self.stack_top.sub(argc as usize) };
                let res = func(argc, argv);

                // Pop the whole call at once an then push on the result.
                self.stack_top = unsafe { self.stack_top.sub(argc as usize + 1) };
                self.push(res);
                true
            }
            _ => {
                self.runtime_error("Can only call functions and classes.");
                false
            }
        }
    }

    pub(crate) fn invoke_from_class(
        &mut self,
        klass: *const ObjClass,
        name: *const ObjString,
        argc: c_uint,
    ) -> bool {
        let klass = unsafe { klass.as_ref().unwrap() };
        let Some(method) = klass.methods.get(name) else {
            self.runtime_error(format!("Undefined property '{}'.", unsafe {
                name.as_ref().unwrap()
            }));
            return false;
        };

        assert!(method.is_obj_type(ObjType::OClosure));
        let method = unsafe { method.as_obj::<ObjClosure>() };
        self.call(method, argc)
    }

    pub(crate) fn invoke(&mut self, name: *const ObjString, argc: c_uint) -> bool {
        let receiver = self.peek(argc);
        if !receiver.is_obj_type(ObjType::OInstance) {
            self.runtime_error("Can't call method on non-instance.");
            return false;
        }
        let instance = unsafe { receiver.as_obj::<ObjInstance>().as_ref().unwrap() };

        if let Some(value) = instance.fields.get(name) {
            // Turns out this was `obj.field(...)`, so replace the receiver with the
            // field value and then call it.
            unsafe { *self.stack_top.sub(argc as usize + 1) = value };
            return self.call_value(value, argc);
        }

        self.invoke_from_class(instance.klass, name, argc)
    }

    pub(crate) fn call(&mut self, closure: *mut ObjClosure, argc: c_uint) -> bool {
        let func = unsafe { closure.as_ref().unwrap().function.as_ref().unwrap() };
        if argc != func.arity {
            self.runtime_error(format!(
                "Expected {} arguments but got {}.",
                func.arity, argc
            ));
            return false;
        }

        if self.frame_count as usize == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let frame = &mut self.frames[self.frame_count as usize];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = func.chunk.code.base_ptr();
        // Subtract an extra slot for stack slot zero (which contains the caller).
        frame.slots = unsafe { self.stack_top.sub(argc as usize + 1) };
        true
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

    pub(crate) fn bind_method(&mut self, klass: *const ObjClass, name: *const ObjString) -> bool {
        let klass = unsafe { klass.as_ref().unwrap() };

        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        let Some(method) = klass.methods.get(name) else {
            self.runtime_error(format!("Undefined property '{}'.", unsafe {
                name.as_ref().unwrap()
            }));
            return false;
        };

        assert!(method.is_obj_type(ObjType::OClosure));

        let bound = ObjBoundMethod::new(gc, self.peek(0), unsafe { method.as_obj::<ObjClosure>() });
        self.pop(); // method
        self.push(Value::obj(bound as *mut Obj));
        true
    }

    pub(crate) fn define_native(&mut self, name: &'static CStr, func: NativeFn) {
        let gc = Gc {
            vm: self,
            compiler: ptr::null_mut(),
        };

        // GC: Ensure the name and value objects are reachable in case resizing the
        // table triggers garbage collection.
        //
        // TODO: Use Alloc stash instead
        {
            self.push(Value::obj(ObjString::from_static(gc, name) as *mut Obj));
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
    pub fn interpret(&mut self, source: *const c_char) -> InterpretResult {
        let gc = Gc::new(ptr::null_mut(), self);

        let function = crate::compiler::compile(self, source);
        if function.is_null() {
            return InterpretResult::InterpretCompileError;
        }

        // GC: Temporarily make the function reachable.
        let closure = {
            self.push(Value::obj(function as *mut Obj));
            let closure = ObjClosure::new(gc, function);
            self.pop();
            closure
        };

        self.push(Value::obj(closure as *mut Obj));
        self.call(closure, 0);

        match self.run() {
            Ok(_) => InterpretResult::InterpretOk,
            Err(_) => InterpretResult::InterpretRuntimeError,
        }
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

        let mut frame: *mut CallFrame = frame_at![(self.frame_count as usize) - 1];

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
                let constants: &crate::chunk::Values =
                    &unsafe { &*(&*(*frame).closure).function }.chunk.constants;
                unsafe { *constants.base_ptr().offset(idx as isize) }
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
                    self.runtime_error("Operands must be numbers.");
                    Err(RuntimeError)
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
                    self.runtime_error("Operands must be numbers.");
                    Err(RuntimeError)
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
                let base = func.chunk.code.base_ptr();
                let instruction = unsafe { frame.ip.offset_from(base) - 1 } as c_uint;
                func.chunk.disassemble_instruction(instruction);
            }

            match opcode {
                Opcode::OpConstant => {
                    self.push(read_constant!());
                }

                Opcode::OpNil => {
                    self.push(Value::nil());
                }
                Opcode::OpTrue => {
                    self.push(Value::bool(true));
                }
                Opcode::OpFalse => {
                    self.push(Value::bool(false));
                }

                Opcode::OpPop => {
                    self.pop();
                }

                Opcode::OpGetLocal => {
                    let slot = read_byte!();
                    let value = unsafe { *((*frame).slots.offset(slot as isize)) };
                    self.push(value);
                }
                Opcode::OpSetLocal => {
                    let slot = read_byte!();
                    let value = self.peek(0);
                    unsafe { *((*frame).slots.offset(slot as isize)) = value };
                }

                Opcode::OpDefineGlobal => {
                    let name = read_string!();

                    // GC: Leave the value on the stack temporarily in case this triggers
                    // garbage collection via table resize.
                    let value = self.peek(0);
                    self.globals.set(gc, name, value);
                    self.pop();
                }
                Opcode::OpGetGlobal => {
                    let name = read_string!();
                    let Some(value) = self.globals.get(name) else {
                        self.runtime_error(format!("Undefined variable '{}'.", unsafe { &*name }));
                        return Err(RuntimeError);
                    };

                    self.push(value);
                }
                Opcode::OpSetGlobal => {
                    let name = read_string!();

                    let value = self.peek(0);
                    let new_key = self.globals.set(gc, name, value);
                    if new_key {
                        // If this was a new key, that means the global wasn't actually defined
                        // yet! Delete it back out and throw the runtime error.
                        self.globals.delete(name);
                        self.runtime_error(format!("Undefined variable '{}'.", unsafe { &*name }));
                        return Err(RuntimeError);
                    }
                }

                Opcode::OpGetUpvalue => {
                    let slot = read_byte!();
                    let value = unsafe {
                        *(*(*(*(*frame).closure).upvalues.offset(slot as isize))).location
                    };
                    self.push(value);
                }
                Opcode::OpSetUpvalue => {
                    let slot = read_byte!();
                    let value = self.peek(0);
                    unsafe {
                        *(*(*(*(*frame).closure).upvalues.offset(slot as isize))).location = value
                    };
                }

                Opcode::OpGetProperty => {
                    if !self.peek(0).is_obj_type(ObjType::OInstance) {
                        self.runtime_error("Only instances have properties.");
                        return Err(RuntimeError);
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
                    if !self.bind_method(klass, name) {
                        return Err(RuntimeError);
                    };
                }
                Opcode::OpSetProperty => {
                    if !self.peek(1).is_obj_type(ObjType::OInstance) {
                        self.runtime_error("Only instances have fields.");
                        return Err(RuntimeError);
                    }

                    let instance = unsafe { self.peek(1).as_obj::<ObjInstance>() };
                    let name = read_string!();

                    let fields = &mut unsafe { &mut *instance }.fields;
                    fields.set(gc, name, self.peek(0));

                    let value = self.pop();
                    self.pop(); // instance
                    self.push(value);
                }
                Opcode::OpGetSuper => {
                    let name = read_string!();
                    let superclass = unsafe { self.pop().as_obj::<ObjClass>() };

                    if !self.bind_method(superclass, name) {
                        return Err(RuntimeError);
                    }
                }

                Opcode::OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::bool(a == b));
                }
                Opcode::OpGreater => compare_op!(PartialOrd::gt)?,
                Opcode::OpLess => compare_op!(PartialOrd::lt)?,

                Opcode::OpNot => {
                    let a = self.pop();
                    self.push(Value::bool(a.is_falsey()));
                }

                Opcode::OpNeg => {
                    if !self.peek(0).is_number() {
                        self.runtime_error("Operand must be a number.");
                        return Err(RuntimeError);
                    }
                    let a = unsafe { self.pop().as_number() };
                    self.push(Value::number(-a));
                }

                Opcode::OpAdd => {
                    if self.peek(0).is_obj_type(ObjType::OString)
                        && self.peek(1).is_obj_type(ObjType::OString)
                    {
                        // GC: Keep the source strings reachable while allocating the result in
                        // case that triggers garbage collection.
                        {
                            let b = unsafe { self.peek(0).as_obj::<ObjString>() };
                            let a = unsafe { self.peek(1).as_obj::<ObjString>() };
                            let res = concatenate(gc, a, b);
                            self.pop(); // b
                            self.pop(); // a
                            self.push(Value::obj(res as *mut Obj));
                        }
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let b = unsafe { self.pop().as_number() };
                        let a = unsafe { self.pop().as_number() };
                        self.push(Value::number(a + b));
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return Err(RuntimeError);
                    }
                }
                Opcode::OpSub => {
                    binary_op!(std::ops::Sub::<f64>::sub)?;
                }
                Opcode::OpMul => {
                    binary_op!(std::ops::Mul::<f64>::mul)?;
                }
                Opcode::OpDiv => {
                    binary_op!(std::ops::Div::<f64>::div)?;
                }

                Opcode::OpPrint => {
                    let v = self.pop();
                    println!("{}", v);
                }

                Opcode::OpJump => {
                    let offset = read_short!();
                    unsafe { (*frame).ip = (*frame).ip.add(offset as usize) };
                }
                Opcode::OpJumpIfFalse => {
                    let offset = read_short!();
                    if self.peek(0).is_falsey() {
                        unsafe { (*frame).ip = (*frame).ip.add(offset as usize) };
                    }
                }
                Opcode::OpLoop => {
                    let offset = read_short!();
                    unsafe { (*frame).ip = (*frame).ip.sub(offset as usize) };
                }

                Opcode::OpCall => {
                    let argc = read_byte!() as c_uint;
                    let callee = self.peek(argc);
                    if !self.call_value(callee, argc) {
                        return Err(RuntimeError);
                    };
                    frame = frame_at![(self.frame_count as usize) - 1];
                }
                Opcode::OpInvoke => {
                    let method = read_string!();
                    let argc = read_byte!() as c_uint;
                    if !self.invoke(method, argc) {
                        return Err(RuntimeError);
                    };
                    frame = frame_at![(self.frame_count as usize) - 1];
                }
                Opcode::OpSuperInvoke => {
                    let method = read_string!();
                    let argc = read_byte!() as c_uint;
                    let superclass = unsafe { self.pop().as_obj::<ObjClass>() };
                    if !self.invoke_from_class(superclass, method, argc) {
                        return Err(RuntimeError);
                    }
                    frame = frame_at![(self.frame_count as usize) - 1];
                }
                Opcode::OpClosure => {
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
                Opcode::OpCloseUpvalue => {
                    self.close_upvalues(unsafe { self.stack_top.sub(1) });
                    self.pop();
                }
                Opcode::OpReturn => {
                    let res = self.pop();
                    self.close_upvalues(unsafe { &*frame }.slots);

                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.stack_top = unsafe { &*frame }.slots;
                    self.push(res);
                    frame = &mut self.frames[(self.frame_count as usize) - 1];
                }

                Opcode::OpClass => {
                    let name = read_string!();
                    let klass = ObjClass::new(gc, name);
                    self.push(Value::obj(klass as *mut Obj));
                }
                Opcode::OpInherit => {
                    let superclass = self.peek(1);
                    if !superclass.is_obj_type(ObjType::OClass) {
                        self.runtime_error("Superclass must be a class.");
                        return Err(RuntimeError);
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
                Opcode::OpMethod => {
                    let name = read_string!();
                    self.define_method(name);
                }
            }
        }
    }
}
