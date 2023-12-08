use std::alloc::{dealloc, realloc, Layout};
use std::mem;
use std::ptr;

use crate::{Compiler, Obj, Vm};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Gc {
    pub(crate) vm: *mut Vm,
    pub(crate) compiler: *mut Compiler,
}

impl Gc {
    pub(crate) fn new(compiler: *mut Compiler, vm: *mut Vm) -> Self {
        Self { compiler, vm }
    }

    pub fn collect_garbage(&mut self) {
        debugln!("-- gc start");
        let before = unsafe { self.vm.as_ref().unwrap() }.bytes_allocated;

        // TODO: self.mark_roots();
        // TODO: self.trace_references();
        // TODO: unsafe { *self.vm }.strings.remove_unreachable();
        // TODO: self.sweep();

        let after = unsafe { self.vm.as_ref().unwrap() }.bytes_allocated;
        debugln!(
            "   gc collected {} bytes ({} => {}) next at {}",
            before - after,
            before,
            after,
            unsafe { self.vm.as_ref().unwrap() }.next_gc
        );
    }

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
            debugln!("-- gc stress");
            self.collect_garbage();
        }

        if unsafe { (*self.vm).bytes_allocated > (*self.vm).next_gc } {
            debugln!(
                "-- gc now: {} > {}",
                unsafe { (*self.vm).bytes_allocated },
                unsafe { (*self.vm).next_gc }
            );

            self.collect_garbage();
        }

        _reallocate(ptr, new)
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
