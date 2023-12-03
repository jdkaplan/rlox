use std::alloc::{dealloc, realloc, Layout};
use std::ffi::c_void;
use std::ptr;

use crate::{Compiler, Vm};

#[repr(C)]
pub struct Gc {
    vm: *mut Vm,
    compiler: *mut Compiler,
}

impl Gc {
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

    pub fn reallocate(&mut self, ptr: *mut c_void, old: usize, new: usize) -> *mut c_void {
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
}

pub fn _reallocate(ptr: *mut c_void, new: usize) -> *mut c_void {
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
