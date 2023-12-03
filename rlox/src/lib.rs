use std::alloc::{dealloc, realloc, Layout};
use std::ffi::{c_char, c_int, c_uint, c_void};
use std::io::Write;
use std::ptr;

// The same as the println macro but only prints in debug builds.
macro_rules! debugln {
    ($($arg:tt)*) => {{
        #[cfg(debug_assertions)]
        ::std::println!($($arg)*);
    }};
}

mod object;
mod scanner;
mod value;

pub use object::{Obj, ObjType};
pub use object::{
    ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative, ObjString,
    ObjUpvalue,
};
pub use scanner::{Scanner, Token, TokenType};
pub use value::{Value, ValueAs, ValueType};

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn scanner_init(scanner: *mut Scanner, source: *const c_char) {
    unsafe { scanner.as_mut().unwrap().init(source) }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn scanner_next(scanner: *mut Scanner) -> Token {
    unsafe { scanner.as_mut().unwrap().next_token() }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn dbg_token(token: Token) {
    debugln!("{:?}", token);
}

#[no_mangle]
pub extern "C" fn print_value(value: Value) {
    Value::print(&value);
    std::io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn println_value(value: Value) {
    Value::print(&value);
    println!();
}

#[no_mangle]
pub extern "C" fn value_eq(a: Value, b: Value) -> bool {
    a == b
}

#[no_mangle]
pub extern "C" fn hello() {
    println!("Hello from Rust!");
}

#[no_mangle]
pub extern "C" fn _reallocate(ptr: *mut c_void, new: usize) -> *mut c_void {
    let layout = Layout::new::<c_void>();
    if new == 0 {
        unsafe { dealloc(ptr as *mut u8, layout) };
        return ptr::null_mut();
    }

    let ptr = unsafe { realloc(ptr as *mut u8, layout, new) as *mut c_void };
    if ptr.is_null() {
        panic!("could not allocate memory");
    }
    ptr
}

pub const U8_COUNT: usize = (u8::MAX as usize) + 1;
pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

/// cbindgen:rename-all=ScreamingSnakeCase
#[repr(C)]
pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

#[repr(C)]
pub struct Gc {
    vm: *mut Vm,
    compiler: *mut Compiler,
}

#[repr(C)]
pub struct Vm {
    // TODO: This is a Vec
    frames: [CallFrame; FRAMES_MAX],
    frame_count: c_uint,

    // TODO: This is a Vec
    stack: [Value; STACK_MAX],
    stack_top: *mut Value,

    globals: Table,

    strings: Table,
    init_string: *mut ObjString,

    open_upvalues: *mut ObjUpvalue,

    objects: *mut Obj,

    // TODO: This is a Vec
    gc_pending_len: usize,
    gc_pending_cap: usize,
    gc_pending_stack: *mut *mut Obj,

    bytes_allocated: usize,
    next_gc: usize,
}

#[repr(C)]
pub struct CallFrame {
    closure: *mut ObjClosure,
    ip: *mut u8,
    slots: *mut Value,
}

#[repr(C)]
pub struct Parser {
    current: Token,
    previous: Token,

    scanner: *mut Scanner,
    compiler: *mut Compiler,
    klass: *mut ClassCompiler,

    had_error: bool,
    panicking: bool,

    vm: *mut Vm,
}

#[repr(C)]
pub struct Compiler {
    enclosing: *mut Compiler,
    function: *mut ObjFunction,
    mode: FunctionMode,

    // TODO: This is a Vec
    locals: [Local; U8_COUNT],
    local_count: c_int,

    scope_depth: c_int,

    // TODO: This is a Vec
    upvalues: [Upvalue; U8_COUNT],
    upvalue_count: c_int,
}

/// cbindgen:rename-all=ScreamingSnakeCase
#[repr(C)]
pub enum FunctionMode {
    ModeFunction,
    ModeInitializer,
    ModeMethod,
    ModeScript,
}

#[repr(C)]
pub struct ClassCompiler {
    enclosing: *mut ClassCompiler,
    has_superclass: bool,
}

#[repr(C)]
pub struct Local {
    name: Token,
    depth: c_int,
    is_captured: bool,
}

#[repr(C)]
pub struct Upvalue {
    index: u8,
    is_local: bool,
}

#[repr(C)]
pub struct Chunk {
    code: Bytecode,
    constants: Values,
    lines: Lines,
}

#[repr(C)]
pub struct Vec<T> {
    len: c_uint,
    cap: c_uint,
    items: *mut T,
}

#[repr(transparent)]
pub struct Bytecode(Vec<u8>);

#[repr(transparent)]
pub struct Lines(Vec<c_int>);

#[repr(transparent)]
pub struct Values(Vec<Value>);

/// cbindgen:rename-all=ScreamingSnakeCase
#[repr(C)]
pub enum Opcode {
    OpConstant,

    OpNil,
    OpTrue,
    OpFalse,

    OpPop,

    OpGetLocal,
    OpSetLocal,
    OpDefineGlobal,
    OpGetGlobal,
    OpSetGlobal,
    OpGetUpvalue,
    OpSetUpvalue,
    OpGetProperty,
    OpSetProperty,
    OpGetSuper,

    OpNot,
    OpEqual,
    OpGreater,
    OpLess,
    // TODO: The other three _are_ needed to handle NaN properly.
    OpNeg,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,

    OpPrint,

    OpJump,
    OpJumpIfFalse,
    OpLoop,

    OpCall,
    OpInvoke,
    OpSuperInvoke,
    OpClosure,
    OpCloseUpvalue,
    OpReturn,

    OpClass,
    OpInherit,
    OpMethod,
}

// TODO: This is a HashMap
#[repr(C)]
pub struct Table {
    size: c_uint,
    cap: c_uint,
    entries: *mut Entry,
}

#[repr(C)]
pub struct Entry {
    key: *mut ObjString,
    value: Value,
}

#[no_mangle]
pub extern "C" fn ___export_all(
    _: Bytecode,
    _: CallFrame,
    _: Chunk,
    _: ClassCompiler,
    _: Compiler,
    _: Entry,
    _: FunctionMode,
    _: Gc,
    _: InterpretResult,
    _: Lines,
    _: Local,
    _: Obj,
    _: ObjBoundMethod,
    _: ObjClass,
    _: ObjClosure,
    _: ObjFunction,
    _: ObjInstance,
    _: ObjNative,
    _: ObjString,
    _: ObjType,
    _: ObjUpvalue,
    _: Opcode,
    _: Parser,
    _: Scanner,
    _: Table,
    _: Token,
    _: TokenType,
    _: Upvalue,
    _: Value,
    _: ValueAs,
    _: Values,
    _: ValueType,
    _: Vm,
) {
}
