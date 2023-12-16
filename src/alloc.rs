use std::alloc::{dealloc, realloc, Layout};
use std::mem;
use std::ptr;
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
    pub(crate) vm: *mut Vm,
    pub(crate) compiler: *mut Compiler<'compiler>,
}

impl<'compiler> Gc<'compiler> {
    pub(crate) fn new(compiler: *mut Compiler<'compiler>, vm: *mut Vm) -> Self {
        Self { compiler, vm }
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
        if self.vm.is_null() {
            return _reallocate(ptr, new);
        }

        // bytes = bytes - old + new; // but avoiding overflow
        if new >= old {
            unsafe { (*self.vm).bytes_allocated += new - old };
        } else {
            unsafe { (*self.vm).bytes_allocated -= old - new };
        }

        #[cfg(feature = "stress_gc")]
        if new > old {
            debug_log_gc!("-- gc stress");
            self.collect_garbage();
        }

        if unsafe { (*self.vm).bytes_allocated > (*self.vm).next_gc } {
            debug_log_gc!(
                "-- gc now: {} > {}",
                unsafe { (*self.vm).bytes_allocated },
                unsafe { (*self.vm).next_gc }
            );

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

    pub fn resize_array<T>(&mut self, ptr: *mut T, old_cap: usize, new_cap: usize) -> *mut T {
        let old = mem::size_of::<T>() * old_cap;
        let new = mem::size_of::<T>() * new_cap;
        self.reallocate(ptr, old, new)
    }

    pub(crate) fn free_objects(&mut self, mut root: *mut Obj) {
        while !root.is_null() {
            let next = unsafe { (*root).next };
            Obj::free(root, self);
            root = next;
        }
    }

    pub(crate) fn free<T>(&mut self, ptr: *mut T) {
        self.reallocate(ptr, mem::size_of::<T>(), 0);
    }

    pub(crate) unsafe fn claim(&mut self, ptr: *mut Obj) {
        (*ptr).next = (*self.vm).objects;
        (*self.vm).objects = ptr;
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

        let mut obj = unsafe { &*self.vm }.objects;
        while let Some(r) = unsafe { obj.as_mut() } {
            assert!(!r.is_marked);
            obj = r.next;
        }

        assert_eq!(unsafe { &*self.vm }.gc_pending.len(), 0);

        debug_log_gc!("-- gc start");
        #[cfg(feature = "log_gc")]
        let before = unsafe { self.vm.as_ref().unwrap() }.bytes_allocated;

        self.mark_roots();
        self.trace_references();
        unsafe { self.vm.as_mut().unwrap() }
            .strings
            .remove_unreachable();
        self.sweep();

        unsafe { self.vm.as_mut().unwrap() }.next_gc =
            unsafe { self.vm.as_mut().unwrap() }.bytes_allocated * GC_GROWTH_FACTOR;

        #[cfg(feature = "log_gc")]
        let after = unsafe { self.vm.as_ref().unwrap() }.bytes_allocated;
        debug_log_gc!(
            "   gc collected {} bytes ({} => {}) next at {}",
            before - after,
            before,
            after,
            unsafe { self.vm.as_ref().unwrap() }.next_gc
        );

        *count -= 1;
        assert_eq!(*count, 0);
    }
}

// Mark
impl Gc<'_> {
    fn mark_roots(&mut self) {
        let vm = unsafe { self.vm.as_mut().unwrap() };

        debug_log_gc!("---- mark slots");
        for value in &vm.stack {
            self.mark_value(*value);
        }

        debug_log_gc!("---- mark frames");
        for frame in &mut vm.frames {
            let obj = frame.closure as *mut Obj;
            self.mark_obj(obj);
        }

        debug_log_gc!("---- mark upvalues");
        let mut upvalue = vm.open_upvalues;
        while !upvalue.is_null() {
            self.mark_obj(upvalue as *mut Obj);
            upvalue = unsafe { &*upvalue }.next;
        }

        debug_log_gc!("---- mark globals");
        self.mark_table(&mut vm.globals);

        debug_log_gc!("---- mark init string");
        self.mark_obj(vm.init_string as *mut Obj);

        debug_log_gc!("---- mark compilers");
        let mut compiler = self.compiler;
        while !compiler.is_null() {
            self.mark_obj(unsafe { compiler.as_mut().unwrap() }.function as *mut Obj);
            compiler = unsafe { &*compiler }.enclosing;
        }
    }

    fn mark_value(&mut self, value: Value) {
        if value.is_obj() {
            self.mark_obj(unsafe { value.as_obj::<Obj>() });
        }
    }

    fn mark_obj(&mut self, obj: *mut Obj) {
        debug_log_gc!("mark_obj: {:?}", obj);

        let Some(obj) = (unsafe { obj.as_mut() }) else {
            return;
        };

        if obj.is_marked {
            return;
        }
        obj.is_marked = true;

        let vm = unsafe { self.vm.as_mut().unwrap() };

        // Don't use `reallocate` or any of its wrappers to avoid triggering recursive
        // garbage collection.
        //
        // TODO: Don't use the global allocator either because it's untracked.
        vm.gc_pending.push(obj);
    }

    fn mark_table(&mut self, table: &mut Table) {
        for entry in &mut table.entries {
            self.mark_obj(entry.key as *mut Obj);
            self.mark_value(entry.value);
        }
    }
}

// Trace
impl Gc<'_> {
    fn trace_references(&mut self) {
        debug_log_gc!("---- trace references");
        let vm = unsafe { self.vm.as_mut().unwrap() };
        while let Some(obj) = vm.gc_pending.pop() {
            self.expand_obj(obj);
        }
    }

    fn expand_obj(&mut self, obj: *mut Obj) {
        debug_log_gc!("expand obj: {:?}", obj);
        match unsafe { obj.as_ref().unwrap().r#type } {
            ObjType::BoundMethod => {
                let bound = unsafe { (obj as *mut ObjBoundMethod).as_mut().unwrap() };
                debug_log_gc!("expand as: {}", bound);

                self.mark_value(bound.receiver);
                self.mark_obj(bound.method as *mut Obj);
            }
            ObjType::Class => {
                let klass = unsafe { (obj as *mut ObjClass).as_mut().unwrap() };
                debug_log_gc!("expand as: {}", klass);

                self.mark_obj(klass.name as *mut Obj);
                self.mark_table(&mut klass.methods);
            }
            ObjType::Closure => {
                let closure = unsafe { (obj as *mut ObjClosure).as_mut().unwrap() };
                debug_log_gc!("expand as: {}", closure);

                self.mark_obj(closure.function as *mut Obj);
                for upvalue in &closure.upvalues {
                    self.mark_obj((*upvalue) as *mut Obj);
                }
            }
            ObjType::Function => {
                let function = unsafe { (obj as *mut ObjFunction).as_mut().unwrap() };
                debug_log_gc!("expand as: {}", function);

                self.mark_obj(function.name as *mut Obj);
                for i in 0..function.chunk.constants.len() {
                    self.mark_value(function.chunk.constants[i]);
                }
            }
            ObjType::Instance => {
                let instance = unsafe { (obj as *mut ObjInstance).as_mut().unwrap() };
                debug_log_gc!("expand as: {}", instance);

                self.mark_obj(instance.klass as *mut Obj);
                self.mark_table(&mut instance.fields);
            }
            ObjType::Upvalue => {
                let upvalue = unsafe { (obj as *mut ObjUpvalue).as_mut().unwrap() };
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
        let vm = unsafe { self.vm.as_mut().unwrap() };

        let mut prev = ptr::null_mut();
        let mut obj = vm.objects;

        while let Some(obj_ref) = unsafe { obj.as_mut() } {
            // Reachable => reset and continue
            if obj_ref.is_marked {
                obj_ref.is_marked = false;
                debug_log_gc!("reachable: {:?}", obj);

                prev = obj;
                obj = obj_ref.next;
                continue;
            }

            // Pop the unreachable object from the list...
            let unreachable = obj;
            debug_log_gc!("unreachable: {:?}", obj);
            obj = obj_ref.next;
            if !prev.is_null() {
                unsafe { (*prev).next = obj };
            } else {
                // The first object was the unreachable one, so start the VM's object list
                // at the next one.
                vm.objects = obj;
            }

            // ... and free it.
            self.free(unreachable);
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
