use std::ffi::{c_char, c_int, c_uint, c_void, CStr};
use std::io::Write;
use std::time::Duration;

// The same as the println macro but only prints in debug builds.
macro_rules! debugln {
    ($($arg:tt)*) => {{
        #[cfg(debug_assertions)]
        ::std::println!($($arg)*);
    }};
}

mod alloc;
mod chunk;
mod compiler;
mod object;
mod scanner;
mod table;
mod value;
mod vec;
mod vm;

pub use alloc::Gc;
pub use chunk::{Bytecode, Chunk, Lines, Opcode, Values};
pub use compiler::{ClassCompiler, Compiler, FunctionMode, Local, Parser, Upvalue};
pub use object::{NativeFn, Obj, ObjType};
pub use object::{
    ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative, ObjString,
    ObjUpvalue,
};
pub use scanner::{Scanner, Token, TokenType};
pub use table::{Entry, Table};
pub use value::{Value, ValueAs, ValueType};
pub use vec::Vec;
pub use vm::{CallFrame, Vm};

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

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn chunk_init(chunk: *mut Chunk) {
    unsafe { chunk.as_mut().unwrap() }.init();
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn chunk_free(mut gc: Gc, chunk: *mut Chunk) {
    unsafe { chunk.as_mut().unwrap() }.free(&mut gc);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn chunk_write(gc: Gc, chunk: *mut Chunk, byte: u8, line: c_int) {
    unsafe { chunk.as_mut().unwrap() }.write_byte(gc, byte, line);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn chunk_add_constant(gc: Gc, chunk: *mut Chunk, value: Value) -> u8 {
    unsafe { chunk.as_mut().unwrap() }.add_constant(gc, value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_init(table: *mut Table) {
    unsafe { table.as_mut().unwrap() }.init()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_free(mut gc: Gc, table: *mut Table) {
    unsafe { table.as_mut().unwrap() }.free(&mut gc)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_get(table: *const Table, key: *const ObjString, value: *mut Value) -> bool {
    if let Some(v) = unsafe { table.as_ref().unwrap() }.get(key) {
        unsafe { *value = v };
        return true;
    }
    false
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_set(
    gc: Gc,
    table: *mut Table,
    key: *const ObjString,
    value: Value,
) -> bool {
    unsafe { table.as_mut().unwrap() }.set(gc, key, value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_delete(table: *mut Table, key: *const ObjString) -> bool {
    unsafe { table.as_mut().unwrap() }.delete(key)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_extend(gc: Gc, dest: *mut Table, src: *const Table) {
    let dest = unsafe { dest.as_mut().unwrap() };
    let src = unsafe { src.as_ref().unwrap() };
    dest.extend(gc, src);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_find_string(
    table: *const Table,
    chars: *const c_char,
    len: usize,
    hash: u32,
) -> *mut ObjString {
    unsafe { table.as_ref().unwrap() }.find_string(chars, len, hash)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn table_remove_unreachable(table: *mut Table) {
    unsafe { table.as_ref().unwrap() }.remove_unreachable()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn bound_method_new(
    gc: Gc,
    receiver: Value,
    method: *mut ObjClosure,
) -> *mut ObjBoundMethod {
    ObjBoundMethod::new(gc, receiver, method)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn class_new(gc: Gc, name: *mut ObjString) -> *mut ObjClass {
    ObjClass::new(gc, name)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn closure_new(gc: Gc, func: *mut ObjFunction) -> *mut ObjClosure {
    ObjClosure::new(gc, func)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn instance_new(gc: Gc, klass: *mut ObjClass) -> *mut ObjInstance {
    ObjInstance::new(gc, klass)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn function_new(gc: Gc) -> *mut ObjFunction {
    ObjFunction::new(gc)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn native_new(gc: Gc, func: NativeFn) -> *mut ObjNative {
    ObjNative::new(gc, func)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn upvalue_new(gc: Gc, slot: *mut Value) -> *mut ObjUpvalue {
    ObjUpvalue::new(gc, slot)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn str_take(gc: Gc, chars: *mut c_char, length: usize) -> *mut ObjString {
    ObjString::from_owned(gc, chars, length)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn str_clone(gc: Gc, chars: *const c_char, length: usize) -> *mut ObjString {
    ObjString::from_borrowed(gc, chars, length)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn free_objects(mut gc: Gc, root: *mut Obj) {
    gc.free_objects(root);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn obj_free(mut gc: Gc, obj: *mut Obj) {
    Obj::free(obj, &mut gc);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_init(vm: *mut Vm) {
    unsafe { vm.as_mut().unwrap() }.init();
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_free(vm: *mut Vm) {
    unsafe { vm.as_mut().unwrap() }.free();
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_interpret(vm: *mut Vm, source: *const c_char) -> InterpretResult {
    let vm = unsafe { vm.as_mut().unwrap() };

    let gc = Gc::new(std::ptr::null_mut(), vm);

    let function = crate::compiler::compile(vm, source);
    if function.is_null() {
        return InterpretResult::InterpretCompileError;
    }

    // GC: Temporarily make the function reachable.
    let closure = {
        vm.push(Value::obj(function as *mut Obj));
        let closure = ObjClosure::new(gc, function);
        vm.pop();
        closure
    };

    vm.push(Value::obj(closure as *mut Obj));
    vm.call(closure, 0);

    match vm.run() {
        Ok(_) => InterpretResult::InterpretOk,
        Err(_) => InterpretResult::InterpretRuntimeError,
    }
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_push(vm: *mut Vm, value: Value) {
    unsafe { vm.as_mut().unwrap() }.push(value);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_pop(vm: *mut Vm) -> Value {
    unsafe { vm.as_mut().unwrap() }.pop()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn clock_native(_argc: c_uint, _argv: *const Value) -> Value {
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

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn compile(vm: *mut Vm, source: *const c_char) -> *mut ObjFunction {
    crate::compiler::compile(unsafe { vm.as_mut().unwrap() }, source)
}

#[no_mangle]
pub extern "C" fn hello() {
    println!("Hello from Rust!");
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn advance(parser: *mut Parser) {
    unsafe { parser.as_mut().unwrap() }.advance()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn r#match(parser: *mut Parser, ty: TokenType) -> bool {
    unsafe { parser.as_mut().unwrap() }.match_(ty)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn number(parser: *mut Parser, can_assign: bool) {
    compiler::number(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn literal(parser: *mut Parser, can_assign: bool) {
    compiler::literal(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn string(parser: *mut Parser, can_assign: bool) {
    compiler::string(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn this_(parser: *mut Parser, can_assign: bool) {
    compiler::this_(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn super_(parser: *mut Parser, can_assign: bool) {
    compiler::super_(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn group(parser: *mut Parser, can_assign: bool) {
    compiler::group(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn call(parser: *mut Parser, can_assign: bool) {
    compiler::call(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn unary(parser: *mut Parser, can_assign: bool) {
    compiler::unary(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn and_(parser: *mut Parser, can_assign: bool) {
    compiler::and_(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn or_(parser: *mut Parser, can_assign: bool) {
    compiler::or_(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn dot(parser: *mut Parser, can_assign: bool) {
    compiler::dot(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn variable(parser: *mut Parser, can_assign: bool) {
    compiler::variable(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn binary(parser: *mut Parser, can_assign: bool) {
    compiler::binary(unsafe { parser.as_mut().unwrap() }, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn expression(parser: *mut Parser) {
    Parser::expression(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn named_variable(parser: *mut Parser, name: Token, can_assign: bool) {
    Parser::named_variable(unsafe { parser.as_mut().unwrap() }, &name, can_assign)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn define_variable(parser: *mut Parser, global: u8) {
    Parser::define_variable(unsafe { parser.as_mut().unwrap() }, global)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn declare_variable(parser: *mut Parser) {
    Parser::declare_variable(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn function(parser: *mut Parser, mode: FunctionMode) {
    Parser::function(unsafe { parser.as_mut().unwrap() }, mode)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn statement(parser: *mut Parser) {
    Parser::statement(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn add_upvalue(
    parser: *mut Parser,
    compiler: *mut Compiler,
    index: u8,
    is_local: bool,
) -> c_int {
    Parser::add_upvalue(
        unsafe { parser.as_mut().unwrap() },
        unsafe { compiler.as_mut().unwrap() },
        index,
        is_local,
    )
    .unwrap_or_default()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn resolve_local(
    parser: *mut Parser,
    compiler: *mut Compiler,
    name: Token,
) -> c_int {
    Parser::resolve_local(
        unsafe { parser.as_mut().unwrap() },
        unsafe { compiler.as_mut().unwrap() },
        &name,
    )
    .unwrap_or(-1)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn identifiers_equal(a: *const Token, b: *const Token) -> bool {
    let a = unsafe { a.as_ref().unwrap() };
    let b = unsafe { b.as_ref().unwrap() };
    compiler::identifiers_equal(a, b)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn identifier_constant(parser: *mut Parser, name: *const Token) -> u8 {
    let parser = unsafe { parser.as_mut().unwrap() };
    let name = unsafe { name.as_ref().unwrap() };
    Parser::identifier_constant(parser, name)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn add_local(parser: *mut Parser, name: Token) {
    Parser::add_local(unsafe { parser.as_mut().unwrap() }, name)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn mark_initialized(compiler: *mut Compiler) {
    Compiler::mark_initialized(unsafe { compiler.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn make_constant(parser: *mut Parser, value: Value) -> u8 {
    Parser::make_constant(unsafe { parser.as_mut().unwrap() }, value)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn emit_return(parser: *mut Parser) {
    Parser::emit_return(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn patch_jump(parser: *mut Parser, offset: c_uint) {
    Parser::patch_jump(unsafe { parser.as_mut().unwrap() }, offset)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn emit_jump(parser: *mut Parser, op: Opcode) -> c_uint {
    Parser::emit_jump(unsafe { parser.as_mut().unwrap() }, op)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn emit_loop(parser: *mut Parser, start: c_uint) {
    Parser::emit_loop(unsafe { parser.as_mut().unwrap() }, start)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn emit_bytes(parser: *mut Parser, b1: u8, b2: u8) {
    Parser::emit_bytes(unsafe { parser.as_mut().unwrap() }, b1, b2)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn emit_byte(parser: *mut Parser, b: u8) {
    Parser::emit_byte(unsafe { parser.as_mut().unwrap() }, b)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn check(parser: *mut Parser, ty: TokenType) -> bool {
    Parser::check(unsafe { parser.as_mut().unwrap() }, ty)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn consume(parser: *mut Parser, ty: TokenType, msg: *const c_char) {
    Parser::consume(unsafe { parser.as_mut().unwrap() }, ty, unsafe {
        CStr::from_ptr(msg).to_str().unwrap()
    })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn scope_begin(parser: *mut Parser) {
    Parser::scope_begin(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn scope_end(parser: *mut Parser) {
    Parser::scope_end(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn stmt_if(parser: *mut Parser) {
    Parser::stmt_if(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn stmt_return(parser: *mut Parser) {
    Parser::stmt_return(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn stmt_expr(parser: *mut Parser) {
    Parser::stmt_expr(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn stmt_print(parser: *mut Parser) {
    Parser::stmt_print(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn parse_variable(parser: *mut Parser, msg: *const c_char) -> u8 {
    Parser::parse_variable(unsafe { parser.as_mut().unwrap() }, unsafe {
        CStr::from_ptr(msg).to_str().unwrap()
    })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn current_chunk(parser: *mut Parser) -> *mut Chunk {
    Parser::current_chunk(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn synchronize(parser: *mut Parser) {
    Parser::synchronize(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn decl_var(parser: *mut Parser) {
    Parser::decl_var(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn decl_class(parser: *mut Parser) {
    Parser::decl_class(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn declaration(parser: *mut Parser) {
    Parser::declaration(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn method(parser: *mut Parser) {
    Parser::method(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn block(parser: *mut Parser) {
    Parser::block(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn stmt_while(parser: *mut Parser) {
    Parser::stmt_while(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn stmt_for(parser: *mut Parser) {
    Parser::stmt_for(unsafe { parser.as_mut().unwrap() })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn error_at_current(parser: *mut Parser, msg: *const c_char) {
    Parser::error_at_current(unsafe { parser.as_mut().unwrap() }, unsafe {
        CStr::from_ptr(msg).to_str().unwrap()
    })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn error(parser: *mut Parser, msg: *const c_char) {
    Parser::error(unsafe { parser.as_mut().unwrap() }, unsafe {
        CStr::from_ptr(msg).to_str().unwrap()
    })
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn parser_init(parser: *mut Parser, scanner: *mut Scanner, vm: *mut Vm) {
    unsafe { parser.as_mut().unwrap() }.init(scanner, vm)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn compiler_init(parser: *mut Parser, compiler: *mut Compiler, mode: FunctionMode) {
    unsafe { parser.as_mut().unwrap() }.compiler_init(compiler, mode)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn end_compiler(parser: *mut Parser) -> *mut ObjFunction {
    unsafe { parser.as_mut().unwrap() }.end_compiler()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_peek(vm: *mut Vm, offset: c_uint) -> Value {
    unsafe { vm.as_mut().unwrap() }.peek(offset)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_reset_stack(vm: *mut Vm) {
    unsafe { vm.as_mut().unwrap() }.reset_stack();
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_print_stack_trace(vm: *mut Vm) {
    unsafe { vm.as_mut().unwrap() }.eprint_stack_trace();
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_call(vm: *mut Vm, closure: *mut ObjClosure, argc: c_uint) -> bool {
    unsafe { vm.as_mut().unwrap() }.call(closure, argc)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_call_value(vm: *mut Vm, callee: Value, argc: c_uint) -> bool {
    unsafe { vm.as_mut().unwrap() }.call_value(callee, argc)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_invoke_from_class(
    vm: *mut Vm,
    klass: *const ObjClass,
    name: *const ObjString,
    argc: c_uint,
) -> bool {
    unsafe { vm.as_mut().unwrap() }.invoke_from_class(klass, name, argc)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_invoke(vm: *mut Vm, name: *const ObjString, argc: c_uint) -> bool {
    unsafe { vm.as_mut().unwrap() }.invoke(name, argc)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_bind_method(
    vm: *mut Vm,
    klass: *const ObjClass,
    name: *const ObjString,
) -> bool {
    unsafe { vm.as_mut().unwrap() }.bind_method(klass, name)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_define_method(vm: *mut Vm, name: *mut ObjString) {
    unsafe { vm.as_mut().unwrap() }.define_method(name);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_capture_upvalue(vm: *mut Vm, local: *mut Value) -> *mut ObjUpvalue {
    unsafe { vm.as_mut().unwrap() }.capture_upvalue(local)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_close_upvalues(vm: *mut Vm, last: *mut Value) {
    unsafe { vm.as_mut().unwrap() }.close_upvalues(last);
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn is_falsey(value: Value) -> bool {
    value.is_falsey()
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn concatenate(
    mut gc: Gc,
    a: *const ObjString,
    b: *const ObjString,
) -> *mut ObjString {
    let length = unsafe { &*a }.length + unsafe { &*b }.length;
    let chars = gc.resize_array(std::ptr::null_mut(), 0, length + 1);

    unsafe {
        std::ptr::copy_nonoverlapping((*a).chars, chars, (*a).length);
        std::ptr::copy_nonoverlapping((*b).chars, chars.add((*a).length), (*b).length);
        *chars.add(length) = '\0' as c_char;
    }

    str_take(gc, chars, length)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[no_mangle]
pub extern "C" fn vm_run(vm: *mut Vm) -> InterpretResult {
    let res = unsafe { vm.as_mut().unwrap() }.run();
    match res {
        Ok(_) => InterpretResult::InterpretOk,
        Err(_) => InterpretResult::InterpretRuntimeError,
    }
}

#[no_mangle]
pub extern "C" fn reallocate(mut gc: Gc, ptr: *mut c_void, old: usize, new: usize) -> *mut c_void {
    gc.reallocate(ptr, old, new)
}

#[no_mangle]
pub extern "C" fn _reallocate(ptr: *mut c_void, new: usize) -> *mut c_void {
    crate::alloc::_reallocate(ptr, new)
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
