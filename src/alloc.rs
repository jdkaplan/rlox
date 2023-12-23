use std::alloc::{dealloc, realloc, Layout};
use std::mem;
use std::ptr::{self, NonNull};
use std::sync::{Mutex, OnceLock};

use crate::compiler::Compiler;
use crate::object::{
    Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjType, ObjUpvalue,
};
use crate::table::Table;
use crate::value::Value;
use crate::vm::Vm;

const GC_GROWTH_FACTOR: usize = 2;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Gc<'compiler> {
    pub(crate) vm: NonNull<Vm>,
    pub(crate) compiler: Option<NonNull<Compiler<'compiler>>>,

    can_gc: bool,
}

impl<'compiler> Gc<'compiler> {
    pub(crate) fn comptime(compiler: Option<NonNull<Compiler<'compiler>>>, vm: &mut Vm) -> Self {
        Self {
            compiler,
            vm: NonNull::from(vm),
            can_gc: true,
        }
    }

    pub(crate) fn runtime(vm: &mut Vm) -> Self {
        Self {
            compiler: None,
            vm: NonNull::from(vm),
            can_gc: true,
        }
    }

    pub(crate) fn boot(vm: &mut Vm) -> Self {
        Self {
            compiler: None,
            vm: NonNull::from(vm),
            can_gc: false,
        }
    }
}

macro_rules! debug_log_gc {
    ($($arg:tt)*) => {{
        #[cfg(feature = "log_gc")]
        println!($($arg)*);
    }};
}

// TODO: This part feels more like an Alloc
impl Gc<'_> {
    pub fn reallocate<T>(&mut self, ptr: *mut T, old: usize, new: usize) -> *mut T {
        // bytes = bytes - old + new; // but avoiding overflow
        let vm = unsafe { self.vm.as_mut() };
        if new >= old {
            vm.bytes_allocated += new - old;
        } else {
            vm.bytes_allocated -= old - new;
        }

        #[cfg(feature = "stress_gc")]
        if self.can_gc && new > old {
            debug_log_gc!("-- gc stress");
            self.collect_garbage();
        }

        let vm = unsafe { self.vm.as_mut() };
        if self.can_gc && vm.bytes_allocated > vm.next_gc {
            debug_log_gc!("-- gc now: {} > {}", vm.bytes_allocated, vm.next_gc);

            self.collect_garbage();
        }

        let new_ptr = _reallocate(ptr, new);
        debug_log_gc!(
            "reallocated: {:?} {} => {:?} {} as {:?}",
            ptr,
            old,
            new_ptr,
            new,
            std::any::type_name::<T>()
        );
        new_ptr
    }

    #[allow(unused)]
    fn resize_array<T>(&mut self, ptr: *mut T, old_cap: usize, new_cap: usize) -> *mut T {
        let old = mem::size_of::<T>() * old_cap;
        let new = mem::size_of::<T>() * new_cap;
        self.reallocate(ptr, old, new)
    }

    #[allow(unused)]
    pub(crate) fn free_objects(&mut self, mut root: Option<NonNull<Obj>>) {
        while let Some(current) = root {
            let next = unsafe { current.as_ref().next };
            Obj::free(current.as_ptr(), self);
            root = next;
        }
    }

    pub(crate) fn free<T>(&mut self, ptr: *mut T) {
        self.reallocate(ptr, mem::size_of::<T>(), 0);
    }

    pub(crate) fn claim<T>(&mut self, obj: T) -> NonNull<T> {
        let ptr = NonNull::new(Box::into_raw(Box::new(obj))).unwrap();
        unsafe {
            let mut obj = ptr.cast::<Obj>();
            (obj.as_mut()).next = self.vm.as_ref().objects;
            self.vm.as_mut().objects = Some(obj);
        };
        ptr
    }
}

fn counter() -> &'static Mutex<u8> {
    static COUNT: OnceLock<Mutex<u8>> = OnceLock::new();
    COUNT.get_or_init(|| Mutex::new(0))
}

impl Gc<'_> {
    pub fn collect_garbage(&mut self) {
        let mut count = counter().lock().unwrap();
        assert_eq!(*count, 0);
        *count += 1;

        let mut obj = unsafe { self.vm.as_ref() }.objects;
        while let Some(r) = obj {
            unsafe {
                assert!(!r.as_ref().is_marked);
                obj = r.as_ref().next;
            }
        }

        assert_eq!(unsafe { self.vm.as_ref() }.gc_pending.len(), 0);

        debug_log_gc!("-- gc start");
        #[cfg(feature = "log_gc")]
        let before = unsafe { self.vm.as_ref() }.bytes_allocated;

        self.mark_roots();
        self.trace_references();
        unsafe { self.vm.as_mut() }.strings.remove_unreachable();
        self.sweep();

        unsafe { self.vm.as_mut() }.next_gc =
            unsafe { self.vm.as_mut() }.bytes_allocated * GC_GROWTH_FACTOR;

        #[cfg(feature = "log_gc")]
        let after = unsafe { self.vm.as_ref() }.bytes_allocated;
        debug_log_gc!(
            "   gc collected {} bytes ({} => {}) next at {}",
            before - after,
            before,
            after,
            unsafe { self.vm.as_ref() }.next_gc
        );

        *count -= 1;
        assert_eq!(*count, 0);
    }
}

// Mark
impl Gc<'_> {
    fn mark_roots(&mut self) {
        let vm = unsafe { self.vm.as_mut() };

        debug_log_gc!("---- mark slots");
        for value in &vm.stack {
            self.mark_value(*value);
        }

        debug_log_gc!("---- mark frames");
        for frame in &mut vm.frames {
            let obj = frame.closure.cast::<Obj>();
            self.mark_obj(obj);
        }

        debug_log_gc!("---- mark upvalues");
        let mut upvalue = vm.open_upvalues;
        while let Some(current) = upvalue {
            self.mark_obj(current.cast::<Obj>());
            upvalue = unsafe { current.as_ref() }.next;
        }

        debug_log_gc!("---- mark globals");
        self.mark_table(&mut vm.globals);

        debug_log_gc!("---- mark init string");
        self.mark_obj(vm.init_string.cast::<Obj>());

        debug_log_gc!("---- mark compilers");
        let mut compiler = self.compiler;
        while let Some(mut inner) = compiler {
            self.mark_obj(unsafe { inner.as_mut() }.function.cast::<Obj>());
            compiler = unsafe { inner.as_ref() }.enclosing;
        }
    }

    fn mark_value(&mut self, value: Value) {
        if value.is_obj() {
            self.mark_obj(unsafe { value.as_obj::<Obj>() });
        }
    }

    fn mark_obj(&mut self, mut obj: NonNull<Obj>) {
        debug_log_gc!("mark_obj: {:?}", obj);

        let obj_ref = unsafe { obj.as_mut() };

        if obj_ref.is_marked {
            return;
        }
        obj_ref.is_marked = true;

        // Don't use `reallocate` or any of its wrappers to avoid triggering recursive
        // garbage collection.
        //
        // TODO: Don't use the global allocator either because it's untracked.
        unsafe { self.vm.as_mut() }.gc_pending.push(obj);
    }

    fn mark_table(&mut self, table: &mut Table) {
        for entry in &mut table.entries {
            if let Some(key) = entry.key {
                self.mark_obj(key.cast::<Obj>());
            }
            self.mark_value(entry.value);
        }
    }
}

// Trace
impl Gc<'_> {
    fn trace_references(&mut self) {
        debug_log_gc!("---- trace references");
        let vm = unsafe { self.vm.as_mut() };
        while let Some(obj) = vm.gc_pending.pop() {
            self.expand_obj(obj);
        }
    }

    fn expand_obj(&mut self, obj: NonNull<Obj>) {
        debug_log_gc!("expand obj: {:?}", obj);
        match unsafe { obj.as_ref().r#type } {
            ObjType::BoundMethod => {
                let bound = unsafe { (obj.cast::<ObjBoundMethod>()).as_mut() };
                debug_log_gc!("expand as: {}", bound);

                self.mark_value(bound.receiver);
                self.mark_obj(bound.method.cast::<Obj>());
            }
            ObjType::Class => {
                let klass = unsafe { (obj.cast::<ObjClass>()).as_mut() };
                debug_log_gc!("expand as: {}", klass);

                self.mark_obj(klass.name.cast::<Obj>());
                self.mark_table(&mut klass.methods);
            }
            ObjType::Closure => {
                let closure = unsafe { (obj.cast::<ObjClosure>()).as_mut() };
                debug_log_gc!("expand as: {}", closure);

                self.mark_obj(closure.function.cast::<Obj>());
                for upvalue in closure.upvalues.iter().flatten() {
                    self.mark_obj(upvalue.cast::<Obj>());
                }
            }
            ObjType::Function => {
                let function = unsafe { (obj.cast::<ObjFunction>()).as_mut() };
                debug_log_gc!("expand as: {}", function);

                if let Some(name) = function.name {
                    self.mark_obj(name.cast::<Obj>());
                }
                for i in 0..function.chunk.constants.len() {
                    self.mark_value(function.chunk.constants[i]);
                }
            }
            ObjType::Instance => {
                let instance = unsafe { (obj.cast::<ObjInstance>()).as_mut() };
                debug_log_gc!("expand as: {}", instance);

                self.mark_obj(instance.klass.cast::<Obj>());
                self.mark_table(&mut instance.fields);
            }
            ObjType::Upvalue => {
                let upvalue = unsafe { (obj.cast::<ObjUpvalue>()).as_mut() };
                debug_log_gc!("expand as: {}", upvalue);

                self.mark_value(upvalue.closed);
            }

            // No further references
            ObjType::Native | ObjType::String => {}
        }
    }
}

// Sweep
impl Gc<'_> {
    fn sweep(&mut self) {
        debug_log_gc!("---- sweep");
        let vm = unsafe { self.vm.as_mut() };

        let mut prev = None;
        let mut obj = vm.objects;

        while let Some(mut obj_ptr) = obj {
            let obj_ref = unsafe { obj_ptr.as_mut() };

            // Reachable => reset and continue
            if obj_ref.is_marked {
                obj_ref.is_marked = false;
                debug_log_gc!("reachable: {:?}", obj);

                prev = obj;
                obj = obj_ref.next;
                continue;
            }

            // Pop the unreachable object from the list...
            let unreachable = obj_ptr;
            debug_log_gc!("unreachable: {:?}", obj);
            obj = obj_ref.next;
            if let Some(mut prev) = prev {
                unsafe { prev.as_mut() }.next = obj;
            } else {
                // The first object was the unreachable one, so start the VM's object list
                // at the next one.
                vm.objects = obj;
            }

            // ... and free it.
            self.free(unreachable.as_ptr());
        }
    }
}

pub fn _reallocate<T>(ptr: *mut T, new: usize) -> *mut T {
    let layout = Layout::new::<T>();
    if new == 0 {
        unsafe { dealloc(ptr as *mut u8, layout) };
        return ptr::null_mut();
    }

    let ptr = unsafe { realloc(ptr as *mut u8, layout, new) as *mut T };
    if ptr.is_null() {
        panic!("could not allocate memory");
    }
    ptr
}
