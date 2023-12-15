use std::ffi::c_char;
use std::fmt;
use std::num::Wrapping;
use std::ptr;

use crate::alloc::Gc;
use crate::chunk::Chunk;
use crate::table::Table;
use crate::value::Value;

#[derive(Debug)]
#[repr(C)]
pub struct Obj {
    pub(crate) r#type: ObjType,
    pub(crate) is_marked: bool,
    pub(crate) next: *mut Obj,
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.r#type {
                ObjType::BoundMethod => {
                    write!(f, "{}", std::mem::transmute::<&Obj, &ObjBoundMethod>(self))
                }

                ObjType::Class => write!(f, "{}", std::mem::transmute::<&Obj, &ObjClass>(self)),
                ObjType::Closure => {
                    write!(f, "{}", std::mem::transmute::<&Obj, &ObjClosure>(self))
                }
                ObjType::Function => {
                    write!(f, "{}", std::mem::transmute::<&Obj, &ObjFunction>(self))
                }
                ObjType::Instance => {
                    write!(f, "{}", std::mem::transmute::<&Obj, &ObjInstance>(self))
                }
                ObjType::Native => write!(f, "<native fn>"),
                ObjType::String => write!(f, "{}", std::mem::transmute::<&Obj, &ObjString>(self)),
                ObjType::Upvalue => write!(f, "upvalue"),
            }
        }
    }
}

impl Obj {
    pub(crate) fn free(obj: *const Obj, gc: &mut Gc) {
        match unsafe { obj.as_ref().unwrap() }.r#type {
            ObjType::BoundMethod => gc.free(obj as *mut ObjBoundMethod),
            ObjType::Class => {
                let klass = obj as *mut ObjClass;
                unsafe { klass.as_mut().unwrap() }.methods.free(gc);
                gc.free(klass)
            }
            ObjType::Closure => gc.free(obj as *mut ObjClosure),
            ObjType::Function => {
                let function = obj as *mut ObjFunction;
                unsafe { function.as_mut().unwrap() }.chunk.free(gc);
                gc.free(function)
            }
            ObjType::Instance => {
                let instance = obj as *mut ObjInstance;
                unsafe { instance.as_mut().unwrap() }.fields.free(gc);
                gc.free(instance)
            }
            ObjType::Native => gc.free(obj as *mut ObjNative),
            ObjType::String => {
                let str = obj as *mut ObjString;
                let str_ref = unsafe { str.as_mut().unwrap() };
                gc.resize_array(str_ref.chars, str_ref.length + 1, 0);
                gc.free(str)
            }
            ObjType::Upvalue => gc.free(obj as *mut ObjUpvalue),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum ObjType {
    BoundMethod,
    Class,
    Closure,
    Function,
    Instance,
    Native,
    String,
    Upvalue,
}

macro_rules! allocate_obj {
    ($gc:expr, $T:ty, $obj_ty:expr) => {{
        let null = ::std::ptr::null_mut();
        let size = ::std::mem::size_of::<$T>();
        let obj: *mut $T = $gc.reallocate(null, 0, size);
        unsafe {
            (*obj).obj.r#type = $obj_ty;
            (*obj).obj.is_marked = false;
        }

        unsafe {
            (*obj).obj.next = (*$gc.vm).objects;
            (*$gc.vm).objects = obj as *mut Obj;
        }
        obj
    }};
}

#[repr(C)]
pub struct ObjBoundMethod {
    pub(crate) obj: Obj,
    pub(crate) receiver: Value,
    pub(crate) method: *mut ObjClosure,
}

impl ObjBoundMethod {
    pub(crate) fn new(mut gc: Gc, receiver: Value, method: *mut ObjClosure) -> *mut Self {
        let bound = allocate_obj!(gc, ObjBoundMethod, ObjType::BoundMethod);
        unsafe {
            (*bound).receiver = receiver;
            (*bound).method = method;
        }
        bound
    }
}

impl fmt::Display for ObjBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { &*(*self.method).function })
    }
}

#[repr(C)]
pub struct ObjClass {
    pub(crate) obj: Obj,

    pub(crate) name: *mut ObjString,
    pub(crate) methods: Table,
}

impl ObjClass {
    pub(crate) fn new(mut gc: Gc, name: *mut ObjString) -> *mut ObjClass {
        let klass = allocate_obj!(gc, ObjClass, ObjType::Class);
        unsafe {
            (*klass).name = name;
            (*klass).methods.init();
        }
        klass
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { &*self.name })
    }
}

#[repr(C)]
pub struct ObjClosure {
    pub(crate) obj: Obj,

    pub(crate) function: *mut ObjFunction,

    // TODO: This is a Vec
    pub(crate) upvalues: *mut *mut ObjUpvalue,
    pub(crate) upvalue_count: usize,
}

impl ObjClosure {
    pub(crate) fn new(mut gc: Gc, function: *mut ObjFunction) -> *mut ObjClosure {
        // TODO: This is a Vec
        let upvalue_count = unsafe { function.as_ref().unwrap() }.upvalue_count;
        let upvalues: *mut *mut ObjUpvalue = gc.resize_array(ptr::null_mut(), 0, upvalue_count);
        for i in 0..upvalue_count {
            unsafe { (*upvalues.add(i)) = ptr::null_mut() };
        }

        let closure = allocate_obj!(gc, ObjClosure, ObjType::Closure);
        unsafe {
            (*closure).function = function;
            (*closure).upvalues = upvalues;
            (*closure).upvalue_count = upvalue_count;
        }
        closure
    }
}

impl fmt::Display for ObjClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { &*self.function })
    }
}

#[repr(C)]
pub struct ObjFunction {
    pub(crate) obj: Obj,

    pub(crate) arity: usize,
    pub(crate) upvalue_count: usize,
    pub(crate) chunk: Chunk,
    pub(crate) name: *mut ObjString,
}

impl ObjFunction {
    pub(crate) fn new(mut gc: Gc) -> *mut ObjFunction {
        let function = allocate_obj!(gc, ObjFunction, ObjType::Function);
        unsafe {
            (*function).arity = 0;
            (*function).upvalue_count = 0;
            (*function).name = ptr::null_mut();
            (*function).chunk.init();
        }
        function
    }

    pub(crate) fn name(&self) -> String {
        let name = self.name;
        if name.is_null() {
            String::from("script")
        } else {
            format!("{}", unsafe { name.as_ref().unwrap() })
        }
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = unsafe { self.name.as_ref() } {
            write!(f, "<fn {}>", name)
        } else {
            write!(f, "<script>")
        }
    }
}

#[repr(C)]
pub struct ObjInstance {
    pub(crate) obj: Obj,
    pub(crate) klass: *mut ObjClass,
    pub(crate) fields: Table,
}

impl ObjInstance {
    pub(crate) fn new(mut gc: Gc, klass: *mut ObjClass) -> *mut ObjInstance {
        let instance = allocate_obj!(gc, ObjInstance, ObjType::Instance);
        unsafe {
            (*instance).klass = klass;
            (*instance).fields.init();
        }
        instance
    }
}

impl fmt::Display for ObjInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", unsafe { &*(*self.klass).name })
    }
}

pub type NativeFn = fn(argc: usize, argv: *const Value) -> Value;

#[repr(C)]
pub struct ObjNative {
    pub(crate) obj: Obj,
    pub(crate) r#fn: NativeFn,
}

impl ObjNative {
    pub(crate) fn new(mut gc: Gc, func: NativeFn) -> *mut ObjNative {
        let native = allocate_obj!(gc, ObjNative, ObjType::Native);
        unsafe { (*native).r#fn = func };
        native
    }
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

impl ObjString {
    pub(crate) fn allocate(mut gc: Gc, chars: *mut c_char, length: usize, hash: u32) -> *mut Self {
        let str = allocate_obj!(gc, ObjString, ObjType::String);
        unsafe {
            (*str).length = length;
            (*str).chars = chars;
            (*str).hash = hash;
        }

        // GC: Ensure `str` is reachable temporarily in case resizing the table
        // triggers garbage collection.
        let vm = unsafe { gc.vm.as_mut().unwrap() };
        vm.push(Value::obj(str as *mut Obj));
        vm.strings.set(gc, str, Value::nil());
        vm.pop();

        str
    }

    pub(crate) fn from_ptr(mut gc: Gc, chars: *mut c_char, length: usize) -> *mut Self {
        let hash = str_hash(chars, length);

        let interned = unsafe { gc.vm.as_mut().unwrap() }
            .strings
            .find_string(chars, length, hash);
        if !interned.is_null() {
            // This takes ownership of `chars`, so free the memory if it's not going to
            // be stored anywhere.
            gc.resize_array(chars, length + 1, 0);
            return interned;
        }

        Self::allocate(gc, chars, length, hash)
    }

    pub(crate) fn from_str(mut gc: Gc, s: &str) -> *mut Self {
        let chars = s.as_ptr() as *const c_char;
        let length = s.len();
        let hash = str_hash(chars, length);

        let interned = unsafe { gc.vm.as_mut().unwrap() }
            .strings
            .find_string(chars, length, hash);
        if !interned.is_null() {
            return interned;
        }

        let heap_chars = gc.resize_array(ptr::null_mut(), 0, length + 1);
        unsafe {
            ptr::copy_nonoverlapping(chars, heap_chars, length);
            *heap_chars.add(length) = '\0' as c_char;
        }
        Self::allocate(gc, heap_chars, length, hash)
    }
}

fn str_hash(chars: *const c_char, length: usize) -> u32 {
    let mut hash = Wrapping(2166136261);
    for i in 0..length {
        hash ^= unsafe { *chars.add(i) as u32 };
        hash *= 16777619;
    }
    hash.0
}

#[repr(C)]
pub struct ObjUpvalue {
    pub(crate) obj: Obj,
    pub(crate) location: *mut Value,
    pub(crate) closed: Value,
    pub(crate) next: *mut ObjUpvalue,
}

impl ObjUpvalue {
    pub(crate) fn new(mut gc: Gc, location: *mut Value) -> *mut ObjUpvalue {
        let upvalue = allocate_obj!(gc, ObjUpvalue, ObjType::Upvalue);
        unsafe {
            (*upvalue).location = location;
            (*upvalue).closed = Value::nil();
            (*upvalue).next = ptr::null_mut();
        }
        upvalue
    }
}

impl fmt::Display for ObjUpvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "upvalue")
    }
}

unsafe fn string_from_c(chars: *const c_char, len: usize) -> String {
    let bytes = std::slice::from_raw_parts(chars as *const u8, len);
    String::from_utf8(bytes.to_vec()).expect("utf-8 source")
}
