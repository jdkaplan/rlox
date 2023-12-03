use std::ffi::{c_char, c_int, c_uint};
use std::fmt;

use crate::value::Value;
use crate::{Chunk, Table};

#[repr(C)]
pub struct Obj {
    pub(crate) r#type: ObjType,
    pub(crate) is_marked: bool,
    pub(crate) next: *mut Obj,
}

pub fn print_object(obj: *const Obj) {
    match unsafe { obj.as_ref().unwrap() }.r#type {
        ObjType::OBoundMethod => print_bound_method(obj as *const ObjBoundMethod),
        ObjType::OClass => print_class(obj as *const ObjClass),
        ObjType::OClosure => print_closure(obj as *const ObjClosure),
        ObjType::OFunction => print_function(obj as *const ObjFunction),
        ObjType::OInstance => print_instance(obj as *const ObjInstance),
        ObjType::ONative => print!("<native fn>"),
        ObjType::OString => print!("{}", unsafe { (obj as *const ObjString).as_ref().unwrap() }),
        ObjType::OUpvalue => print!("upvalue"),
    }
}

/// cbindgen:rename-all=ScreamingSnakeCase
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum ObjType {
    OBoundMethod,
    OClass,
    OClosure,
    OFunction,
    OInstance,
    ONative,
    OString,
    OUpvalue,
}

#[repr(C)]
pub struct ObjBoundMethod {
    obj: Obj,
    receiver: Value,
    method: *mut ObjClosure,
}

pub fn print_bound_method(bound: *const ObjBoundMethod) {
    print_function(unsafe { bound.as_ref().unwrap().method.as_ref().unwrap() }.function);
}

#[repr(C)]
pub struct ObjClass {
    obj: Obj,

    name: *mut ObjString,
    methods: Table,
}

pub fn print_class(klass: *const ObjClass) {
    print!("{}", unsafe {
        klass.as_ref().unwrap().name.as_ref().unwrap()
    });
}

#[repr(C)]
pub struct ObjClosure {
    obj: Obj,

    function: *mut ObjFunction,

    // TODO: This is a Vec
    upvalues: *mut *mut ObjUpvalue,
    upvalue_count: c_int,
}

pub fn print_closure(closure: *const ObjClosure) {
    print_function(unsafe { closure.as_ref().unwrap() }.function);
}

#[repr(C)]
pub struct ObjInstance {
    obj: Obj,
    klass: *mut ObjClass,
    fields: Table,
}

pub fn print_instance(instance: *const ObjInstance) {
    print!("{} instance", unsafe {
        instance
            .as_ref()
            .unwrap()
            .klass
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .unwrap()
    });
}

#[repr(C)]
pub struct ObjFunction {
    obj: Obj,

    arity: c_uint,
    upvalue_count: c_int,
    chunk: Chunk,
    name: *mut ObjString,
}

pub fn print_function(func: *const ObjFunction) {
    let name = unsafe { func.as_ref().unwrap() }.name;
    if name.is_null() {
        print!("<script>");
    } else {
        print!("<fn {}>", unsafe { name.as_ref().unwrap() });
    }
}

type NativeFn = extern "C" fn(argc: c_uint, argv: *const Value) -> Value;

#[repr(C)]
pub struct ObjNative {
    obj: Obj,
    r#fn: NativeFn,
}

#[repr(C)]
pub struct ObjString {
    pub(crate) obj: Obj,

    pub(crate) length: usize,
    pub(crate) chars: *mut c_char,
    pub(crate) hash: u32,
}

impl fmt::Display for ObjString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { string_from_c(self.chars, self.length) })
    }
}

#[repr(C)]
pub struct ObjUpvalue {
    obj: Obj,
    location: *mut Value,
    closed: Value,
    next: *mut ObjUpvalue,
}

unsafe fn string_from_c(chars: *const c_char, len: usize) -> String {
    let bytes = std::slice::from_raw_parts(chars as *const u8, len);
    String::from_utf8(bytes.to_vec()).expect("utf-8 source")
}
