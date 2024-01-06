use std::fmt;
use std::mem;
use std::num::Wrapping;
use std::ptr::NonNull;

use crate::chunk::Chunk;
use crate::gc::Gc;
use crate::heap::HeapObj;
use crate::table::Table;
use crate::value::Value;

macro_rules! heap_alloc {
    ($gc:expr, $obj:expr) => {{
        crate::gc::Gc::<'_, '_>::_run_collection(&mut $gc);
        crate::gc::Gc::<'_, '_>::_claim(&mut $gc, Box::new($obj))
    }};
}

#[derive(Debug)]
#[repr(C)]
pub struct Obj {
    pub(crate) ty: ObjType,
    pub(crate) is_marked: bool,
    pub(crate) next: Option<NonNull<Obj>>,
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.ty {
                ObjType::BoundMethod => {
                    write!(f, "{}", mem::transmute::<&Obj, &ObjBoundMethod>(self))
                }

                ObjType::Class => write!(f, "{}", mem::transmute::<&Obj, &ObjClass>(self)),
                ObjType::Closure => {
                    write!(f, "{}", mem::transmute::<&Obj, &ObjClosure>(self))
                }
                ObjType::Function => {
                    write!(f, "{}", mem::transmute::<&Obj, &ObjFunction>(self))
                }
                ObjType::Instance => {
                    write!(f, "{}", mem::transmute::<&Obj, &ObjInstance>(self))
                }
                ObjType::Native => write!(f, "<native fn>"),
                ObjType::String => write!(f, "{}", mem::transmute::<&Obj, &ObjString>(self)),
                ObjType::Upvalue => write!(f, "upvalue"),
            }
        }
    }
}

impl Obj {
    fn new(ty: ObjType) -> Self {
        Self {
            ty,
            is_marked: false,
            next: None,
        }
    }

    pub(crate) fn free(obj: NonNull<Obj>) -> usize {
        match unsafe { obj.as_ref() }.ty {
            ObjType::BoundMethod => _free(obj.cast::<ObjBoundMethod>()),

            ObjType::Class => {
                let mut klass = obj.cast::<ObjClass>();
                mem::take(&mut unsafe { klass.as_mut() }.methods);
                _free(klass)
            }

            ObjType::Closure => _free(obj.cast::<ObjClosure>()),

            ObjType::Function => {
                let mut function = obj.cast::<ObjFunction>();
                mem::take(&mut unsafe { function.as_mut() }.chunk);
                _free(function)
            }

            ObjType::Instance => {
                let mut instance = obj.cast::<ObjInstance>();
                mem::take(&mut unsafe { instance.as_mut() }.fields);
                _free(instance)
            }

            ObjType::Native => _free(obj.cast::<ObjNative>()),

            ObjType::String => {
                let mut str = obj.cast::<ObjString>();
                mem::take(&mut unsafe { str.as_mut() }.chars);
                _free(str)
            }

            ObjType::Upvalue => _free(obj.cast::<ObjUpvalue>()),
        }
    }
}

fn _free<T>(ptr: NonNull<T>) -> usize {
    debug_log_gc!(
        "free: {:?} {} as {:?}",
        ptr,
        mem::size_of::<T>(),
        std::any::type_name::<T>()
    );

    let layout = std::alloc::Layout::new::<T>();
    unsafe { std::alloc::dealloc(ptr.as_ptr() as *mut u8, layout) };

    mem::size_of::<T>()
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

#[repr(C)]
pub struct ObjBoundMethod {
    pub(crate) obj: Obj,
    pub(crate) receiver: Value,
    pub(crate) method: NonNull<ObjClosure>,
}

unsafe impl HeapObj for ObjBoundMethod {}

impl ObjBoundMethod {
    pub(crate) fn new(mut gc: Gc, receiver: Value, method: NonNull<ObjClosure>) -> NonNull<Self> {
        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::BoundMethod),
                receiver,
                method,
            }
        )
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

unsafe impl HeapObj for ObjClass {}

impl ObjClass {
    pub(crate) fn new(mut gc: Gc, name: NonNull<ObjString>) -> NonNull<Self> {
        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::Class),
                name,
                methods: Table::default(),
            }
        )
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

unsafe impl HeapObj for ObjClosure {}

impl ObjClosure {
    pub(crate) fn new(mut gc: Gc, function: NonNull<ObjFunction>) -> NonNull<Self> {
        let upvalue_count = unsafe { function.as_ref() }.upvalue_count;
        let upvalues = vec![None; upvalue_count];

        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::Closure),
                function,
                upvalues,
            }
        )
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

unsafe impl HeapObj for ObjFunction {}

impl ObjFunction {
    pub(crate) fn new(mut gc: Gc) -> NonNull<Self> {
        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::Function),
                arity: 0,
                upvalue_count: 0,
                name: None,
                chunk: Chunk::default(),
            }
        )
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

unsafe impl HeapObj for ObjInstance {}

impl ObjInstance {
    pub(crate) fn new(mut gc: Gc, klass: NonNull<ObjClass>) -> NonNull<Self> {
        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::Instance),
                klass,
                fields: Table::default(),
            }
        )
    }
}

impl fmt::Display for ObjInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", unsafe {
            self.klass.as_ref().name.as_ref()
        })
    }
}

pub type NativeFn = fn(argc: usize, argv: NonNull<Value>) -> Value;

#[repr(C)]
pub struct ObjNative {
    pub(crate) obj: Obj,
    pub(crate) func: NativeFn,
}

unsafe impl HeapObj for ObjNative {}

impl ObjNative {
    pub(crate) fn new(mut gc: Gc, func: NativeFn) -> NonNull<Self> {
        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::Native),
                func,
            }
        )
    }
}

#[repr(C)]
pub struct ObjString {
    pub(crate) obj: Obj,

    pub(crate) chars: String,
    pub(crate) hash: u32,
}

unsafe impl HeapObj for ObjString {}

impl fmt::Display for ObjString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.chars)
    }
}

impl ObjString {
    pub(crate) fn allocate(mut gc: Gc, chars: String, hash: u32) -> NonNull<Self> {
        let str = heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::String),
                chars,
                hash,
            }
        );

        // GC: Ensure `str` is reachable temporarily in case resizing the table
        // triggers garbage collection.
        let vm = unsafe { gc.vm.as_mut() };
        vm.push(Value::obj(str.cast::<Obj>()));
        vm.heap.strings.set(gc, str, Value::nil());
        vm.pop();

        str
    }

    pub(crate) fn from_string(mut gc: Gc, chars: String) -> NonNull<Self> {
        let hash = str_hash(&chars);

        let interned = unsafe { gc.vm.as_mut() }
            .heap
            .strings
            .find_string(&chars, hash);
        match interned {
            Some(interned) => interned,
            None => Self::allocate(gc, chars, hash),
        }
    }

    pub(crate) fn from_str(mut gc: Gc, chars: &str) -> NonNull<Self> {
        let hash = str_hash(chars);

        let interned = unsafe { gc.vm.as_mut() }
            .heap
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

unsafe impl HeapObj for ObjUpvalue {}

impl ObjUpvalue {
    pub(crate) fn new(mut gc: Gc, location: Option<NonNull<Value>>) -> NonNull<Self> {
        heap_alloc!(
            gc,
            Self {
                obj: Obj::new(ObjType::Upvalue),
                location,
                closed: Value::nil(),
                next: None,
            }
        )
    }
}

impl fmt::Display for ObjUpvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "upvalue")
    }
}
