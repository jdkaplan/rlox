use std::ffi::{c_char, c_uint};
use std::ptr;

use crate::alloc::Gc;
use crate::object::ObjString;
use crate::value::Value;

const TABLE_MAX_LOAD: f64 = 0.75;

// TODO: This is a HashMap
#[repr(C)]
pub struct Table {
    size: c_uint,
    cap: u32,
    entries: *mut Entry,
}

#[repr(C)]
pub struct Entry {
    key: *mut ObjString,
    value: Value,
}

fn find_entry(entries: *mut Entry, cap: c_uint, key: *const ObjString) -> *mut Entry {
    let mut idx = unsafe { key.as_ref().unwrap() }.hash % cap;
    let mut tombstone = ptr::null_mut::<Entry>();

    loop {
        let entry = unsafe { entries.add(idx as usize) };
        if unsafe { entry.as_ref().unwrap() }.key.is_null() {
            if unsafe { entry.as_ref().unwrap() }.value.is_nil() {
                // Empty entry. If there was a tombstone along the way, return that
                // instead to reuse the space.
                if tombstone.is_null() {
                    return entry;
                } else {
                    return tombstone;
                };
            } else {
                // Found a tombstone. Store it for later unless we already found one.
                if tombstone.is_null() {
                    tombstone = entry;
                }
            }
        } else if unsafe { entry.as_ref().unwrap() }.key as *const ObjString == key {
            return entry;
        }

        idx = (idx + 1) % cap;
    }
}

impl Table {
    pub(crate) fn init(&mut self) {
        self.size = 0;
        self.cap = 0;
        self.entries = ptr::null_mut();
    }

    pub(crate) fn free(&mut self, gc: &mut Gc) {
        gc.resize_array(self.entries, self.cap as usize, 0);
        self.init();
    }

    pub(crate) fn resize(&mut self, mut gc: Gc, cap: u32) {
        let entries = gc.resize_array::<Entry>(ptr::null_mut(), 0, cap as usize);
        for i in 0..(cap as usize) {
            unsafe {
                (*entries.add(i)).key = ptr::null_mut();
                (*entries.add(i)).value = Value::nil();
            }
        }

        self.size = 0;
        for i in 0..(self.cap as usize) {
            let entry = unsafe { self.entries.add(i) };
            let key = unsafe { entry.as_ref().unwrap() }.key;
            if key.is_null() {
                continue;
            }

            let dest = find_entry(entries, cap, key);
            unsafe {
                (*dest).key = (*entry).key;
                (*dest).value = (*entry).value;
            }
            self.size += 1;
        }

        gc.resize_array(self.entries, self.cap as usize, 0);
        self.entries = entries;
        self.cap = cap;
    }

    pub(crate) fn get(&self, key: *const ObjString) -> Option<Value> {
        if self.size == 0 {
            return None;
        }

        let entry = find_entry(self.entries, self.cap, key);
        let entry = unsafe { entry.as_ref().unwrap() };
        if entry.key.is_null() {
            return None;
        }

        Some(entry.value)
    }

    pub(crate) fn set(&mut self, gc: Gc, key: *const ObjString, value: Value) -> bool {
        if (self.size + 1) as f64 > (self.cap as f64) * TABLE_MAX_LOAD {
            let cap = grow_cap(self.cap);
            self.resize(gc, cap);
        }

        let entry = find_entry(self.entries, self.cap, key);

        let is_new = unsafe { entry.as_ref().unwrap() }.key.is_null();
        if is_new && unsafe { entry.as_ref().unwrap() }.value.is_nil() {
            self.size += 1;
        }

        unsafe {
            (*entry).key = key as *mut ObjString;
            (*entry).value = value;
        }
        is_new
    }

    pub(crate) fn delete(&self, key: *const ObjString) -> bool {
        if self.size == 0 {
            return false;
        }

        let entry = find_entry(self.entries, self.cap, key);
        if entry.is_null() {
            return false;
        }

        // Put a tombstone in the entry so linear probing still considers this as part
        // of the chain.
        unsafe {
            (*entry).key = ptr::null_mut();
            (*entry).value = Value::bool(false);
        }
        true
    }

    pub(crate) fn extend(&mut self, gc: Gc, src: &Table) {
        for i in 0..(src.cap as usize) {
            let entry = unsafe { src.entries.add(i) };
            let entry = unsafe { entry.as_ref().unwrap() };
            if !entry.key.is_null() {
                self.set(gc, entry.key, entry.value);
            }
        }
    }

    pub(crate) fn find_string(
        &self,
        chars: *const c_char,
        length: usize,
        hash: u32,
    ) -> *mut ObjString {
        if self.size == 0 {
            return ptr::null_mut();
        }

        let mut idx = hash % self.cap;
        loop {
            let entry = unsafe { self.entries.add(idx as usize) };
            let entry = unsafe { entry.as_ref().unwrap() };
            if entry.key.is_null() {
                if entry.value.is_nil() {
                    // Truly empty.
                    return ptr::null_mut();
                }
                // This was a tombstone, so keep searching.
            } else if unsafe { str_equal(entry.key, chars, hash, length) } {
                // Found it!
                return entry.key;
            }

            idx = (idx + 1) % self.cap;
        }
    }

    pub(crate) fn remove_unreachable(&self) {
        for i in 0..(self.cap as usize) {
            let entry = unsafe { self.entries.add(i) };
            let entry = unsafe { entry.as_ref().unwrap() };
            if entry.key.is_null() {
                continue;
            }

            let key = unsafe { entry.key.as_mut().unwrap() };
            if !key.obj.is_marked {
                self.delete(entry.key);
            }
        }
    }
}

fn grow_cap(cap: u32) -> u32 {
    if cap < 8 {
        8
    } else {
        2 * cap
    }
}

unsafe fn str_equal(
    a: *const ObjString,
    b_chars: *const c_char,
    b_hash: u32,
    length: usize,
) -> bool {
    unsafe {
        (*a).length == length && (*a).hash == b_hash && chars_equal((*a).chars, b_chars, length)
    }
}

unsafe fn chars_equal(a: *const c_char, b_chars: *const c_char, length: usize) -> bool {
    let aa = std::slice::from_raw_parts(a as *const u8, length);
    let bb = std::slice::from_raw_parts(b_chars as *const u8, length);
    aa == bb
}
