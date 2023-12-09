use std::ffi::c_uint;
use std::ptr;

use crate::alloc::Gc;

#[derive(Debug)]
#[repr(C)]
pub struct Vec<T> {
    len: c_uint,
    cap: c_uint,
    items: *mut T,
}

impl<T> Vec<T> {
    pub(crate) fn init(&mut self) {
        self.len = 0;
        self.cap = 0;
        self.items = ptr::null_mut();
    }

    pub(crate) fn free(&mut self, gc: &mut Gc) {
        gc.resize_array(self.items, self.cap as usize, 0);
        self.init();
    }

    pub(crate) fn len(&self) -> c_uint {
        self.len
    }

    pub(crate) fn get(&self, idx: c_uint) -> &T {
        unsafe { self.items.add(idx as usize).as_ref().unwrap() }
    }

    pub(crate) fn set(&self, idx: c_uint, val: T) {
        unsafe { *self.items.add(idx as usize) = val };
    }

    pub(crate) fn base_ptr(&self) -> *mut T {
        self.items
    }

    pub(crate) fn push(&mut self, mut gc: Gc, val: T) {
        if self.len + 1 > self.cap {
            let old_cap = self.cap;
            self.cap = grow_cap(old_cap);
            self.items = gc.resize_array(self.items, old_cap as usize, self.cap as usize);
        }

        unsafe {
            *self.items.add(self.len as usize) = val;
        }

        self.len += 1;
    }
}

fn grow_cap(cap: u32) -> u32 {
    if cap < 8 {
        8
    } else {
        2 * cap
    }
}
