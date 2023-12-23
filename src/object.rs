use std::fmt;
use std::num::Wrapping;
use std::ptr::NonNull;

use crate::alloc::Gc;
use crate::chunk::Chunk;
use crate::table::Table;
use crate::value::Value;

#[derive(Debug)]
#[repr(C)]
pub struct Obj {
    pub(crate) r#type: ObjType,
    pub(crate) is_marked: bool,
    pub(crate) next: Option<NonNull<Obj>>,
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
                std::mem::take(&mut unsafe { &mut *klass }.methods);
                gc.free(klass)
            }
            ObjType::Closure => gc.free(obj as *mut ObjClosure),
            ObjType::Function => {
                let function = obj as *mut ObjFunction;
                std::mem::take(&mut unsafe { function.as_mut().unwrap() }.chunk);
                gc.free(function)
            }
            ObjType::Instance => {
                let instance = obj as *mut ObjInstance;
                std::mem::take(&mut unsafe { &mut *instance }.fields);
                gc.free(instance)
            }
            ObjType::Native => gc.free(obj as *mut ObjNative),
            ObjType::String => {
                let str = obj as *mut ObjString;
                std::mem::take(&mut unsafe { &mut *str }.chars);
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

        let ptr = NonNull::new(obj).unwrap();
        unsafe { $gc.claim(ptr.cast::<Obj>()) };
        ptr
    }};
}

#[repr(C)]
pub struct ObjBoundMethod {
    pub(crate) obj: Obj,
    pub(crate) receiver: Value,
    pub(crate) method: NonNull<ObjClosure>,
}

impl ObjBoundMethod {
    pub(crate) fn new(mut gc: Gc, receiver: Value, method: NonNull<ObjClosure>) -> NonNull<Self> {
        let mut bound = allocate_obj!(gc, ObjBoundMethod, ObjType::BoundMethod);
        unsafe {
            bound.as_mut().receiver = receiver;
            bound.as_mut().method = method;
        }
        bound
    }
}

impl fmt::Display for ObjBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { self.method.as_ref().function.as_ref() })
    }
}

#[repr(C)]
pub struct ObjClass {
    pub(crate) obj: Obj,

    pub(crate) name: NonNull<ObjString>,
    pub(crate) methods: Table,
}

// TODO: ManuallyDrop instead?
macro_rules! forget_uninit {
    ($x:expr) => {{
        let old = ::std::mem::take($x);
        std::mem::forget(old);
    }};
}

impl ObjClass {
    pub(crate) fn new(mut gc: Gc, name: NonNull<ObjString>) -> NonNull<Self> {
        let mut klass = allocate_obj!(gc, ObjClass, ObjType::Class);
        unsafe {
            klass.as_mut().name = name;
            forget_uninit!(&mut (klass.as_mut()).methods);
        }
        klass
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { self.name.as_ref() })
    }
}

#[repr(C)]
pub struct ObjClosure {
    pub(crate) obj: Obj,

    pub(crate) function: NonNull<ObjFunction>,
    pub(crate) upvalues: Vec<Option<NonNull<ObjUpvalue>>>,
}

impl ObjClosure {
    pub(crate) fn new(mut gc: Gc, function: NonNull<ObjFunction>) -> NonNull<Self> {
        let upvalue_count = unsafe { function.as_ref() }.upvalue_count;
        let upvalues = vec![None; upvalue_count];

        let mut closure = allocate_obj!(gc, ObjClosure, ObjType::Closure);
        unsafe {
            closure.as_mut().function = function;
            forget_uninit!(&mut (closure.as_mut()).upvalues);
            closure.as_mut().upvalues = upvalues;
        }
        closure
    }
}

impl fmt::Display for ObjClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe { self.function.as_ref() })
    }
}

#[repr(C)]
pub struct ObjFunction {
    pub(crate) obj: Obj,

    pub(crate) arity: usize,
    pub(crate) upvalue_count: usize,
    pub(crate) chunk: Chunk,
    pub(crate) name: Option<NonNull<ObjString>>,
}

impl ObjFunction {
    pub(crate) fn new(mut gc: Gc) -> NonNull<Self> {
        let mut function = allocate_obj!(gc, ObjFunction, ObjType::Function);
        unsafe {
            function.as_mut().arity = 0;
            function.as_mut().upvalue_count = 0;
            function.as_mut().name = None;
            forget_uninit!(&mut (function.as_mut()).chunk);
        }
        function
    }

    pub(crate) fn name(&self) -> String {
        let name = self.name;
        if let Some(name) = name {
            format!("{}", unsafe { name.as_ref() })
        } else {
            String::from("script")
        }
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name {
            write!(f, "<fn {}>", unsafe { name.as_ref() })
        } else {
            write!(f, "<script>")
        }
    }
}

#[repr(C)]
pub struct ObjInstance {
    pub(crate) obj: Obj,
    pub(crate) klass: NonNull<ObjClass>,
    pub(crate) fields: Table,
}

impl ObjInstance {
    pub(crate) fn new(mut gc: Gc, klass: NonNull<ObjClass>) -> NonNull<Self> {
        let mut instance = allocate_obj!(gc, ObjInstance, ObjType::Instance);
        unsafe {
            instance.as_mut().klass = klass;
            forget_uninit!(&mut (instance.as_mut()).fields);
        }
        instance
    }
}

impl fmt::Display for ObjInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", unsafe {
            self.klass.as_ref().name.as_ref()
        })
    }
}

pub type NativeFn = fn(argc: usize, argv: *const Value) -> Value;

#[repr(C)]
pub struct ObjNative {
    pub(crate) obj: Obj,
    pub(crate) r#fn: NativeFn,
}

impl ObjNative {
    pub(crate) fn new(mut gc: Gc, func: NativeFn) -> NonNull<Self> {
        let mut native = allocate_obj!(gc, ObjNative, ObjType::Native);
        unsafe { native.as_mut().r#fn = func };
        native
    }
}

#[repr(C)]
pub struct ObjString {
    pub(crate) obj: Obj,

    pub(crate) chars: String,
    pub(crate) hash: u32,
}

impl fmt::Display for ObjString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.chars)
    }
}

impl ObjString {
    pub(crate) fn allocate(mut gc: Gc, chars: String, hash: u32) -> NonNull<Self> {
        let mut str = allocate_obj!(gc, ObjString, ObjType::String);
        unsafe {
            forget_uninit!(&mut (str.as_mut()).chars);
            str.as_mut().chars = chars;
            str.as_mut().hash = hash;
        }

        // GC: Ensure `str` is reachable temporarily in case resizing the table
        // triggers garbage collection.
        let vm = unsafe { gc.vm.as_mut().unwrap() };
        vm.push(Value::obj(str.cast::<Obj>()));
        vm.strings.set(gc, str, Value::nil());
        vm.pop();

        str
    }

    pub(crate) fn from_string(gc: Gc, chars: String) -> NonNull<Self> {
        let hash = str_hash(&chars);

        let interned = unsafe { gc.vm.as_mut().unwrap() }
            .strings
            .find_string(&chars, hash);
        match interned {
            Some(interned) => interned,
            None => Self::allocate(gc, chars, hash),
        }
    }

    pub(crate) fn from_str(gc: Gc, chars: &str) -> NonNull<Self> {
        let hash = str_hash(chars);

        let interned = unsafe { gc.vm.as_mut().unwrap() }
            .strings
            .find_string(chars, hash);
        match interned {
            Some(interned) => interned,
            None => Self::allocate(gc, String::from(chars), hash),
        }
    }

    pub(crate) fn concatenate(
        gc: Gc,
        a: NonNull<ObjString>,
        b: NonNull<ObjString>,
    ) -> NonNull<ObjString> {
        let a_chars = &unsafe { a.as_ref() }.chars;
        let b_chars = &unsafe { b.as_ref() }.chars;
        let length = a_chars.len() + b_chars.len();

        let mut chars = String::with_capacity(length);
        chars.push_str(a_chars);
        chars.push_str(b_chars);

        ObjString::from_string(gc, chars)
    }
}

fn str_hash(chars: &str) -> u32 {
    let mut hash = Wrapping(2166136261_u32);
    for b in chars.bytes() {
        hash ^= b as u32;
        hash *= 16777619;
    }
    hash.0
}

#[repr(C)]
pub struct ObjUpvalue {
    pub(crate) obj: Obj,
    pub(crate) location: Option<NonNull<Value>>,
    pub(crate) closed: Value,
    pub(crate) next: Option<NonNull<ObjUpvalue>>,
}

impl ObjUpvalue {
    pub(crate) fn new(mut gc: Gc, location: Option<NonNull<Value>>) -> NonNull<Self> {
        let mut upvalue = allocate_obj!(gc, ObjUpvalue, ObjType::Upvalue);
        unsafe {
            upvalue.as_mut().location = location;
            upvalue.as_mut().closed = Value::nil();
            upvalue.as_mut().next = None
        }
        upvalue
    }
}

impl fmt::Display for ObjUpvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "upvalue")
    }
}
