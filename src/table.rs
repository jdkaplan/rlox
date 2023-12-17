use std::ptr;

use crate::alloc::Gc;
use crate::object::ObjString;
use crate::value::Value;

const TABLE_MAX_LOAD: f64 = 0.75;

// TODO: This is a HashMap
#[derive(Default)]
#[repr(C)]
pub struct Table {
    pub(crate) size: u32,
    pub(crate) entries: Vec<Entry>,
}

#[derive(Clone)]
#[repr(C)]
pub struct Entry {
    pub(crate) key: *mut ObjString,
    pub(crate) value: Value,
}

impl Default for Entry {
    fn default() -> Self {
        Self {
            key: ptr::null_mut(),
            value: Default::default(),
        }
    }
}

fn find_slot(entries: &[Entry], key: *const ObjString) -> usize {
    let cap = entries.len() as u32;

    let mut idx = unsafe { key.as_ref().unwrap() }.hash % cap;
    let mut tombstone = None;

    loop {
        let slot = idx as usize;
        let entry = &entries[slot];
        if entry.key.is_null() {
            if entry.value.is_nil() {
                // Empty entry. If there was a tombstone along the way, return that
                // instead to reuse the space.
                return tombstone.unwrap_or(slot);
            } else {
                // Found a tombstone. Store it for later unless we already found one.
                if tombstone.is_none() {
                    tombstone = Some(slot);
                }
            }
        } else if entry.key as *const ObjString == key {
            return slot;
        }

        idx = (idx + 1) % cap;
    }
}

impl Table {
    pub(crate) fn new() -> Self {
        Self {
            size: 0,
            entries: Vec::new(),
        }
    }

    pub(crate) fn resize(&mut self, mut _gc: Gc, cap: usize) {
        let mut entries = vec![Entry::default(); cap];

        self.size = 0;
        for entry in &self.entries {
            let key = entry.key;
            if key.is_null() {
                continue;
            }

            let slot = find_slot(&entries, key);
            let dest = &mut entries[slot];
            dest.key = entry.key;
            dest.value = entry.value;
            self.size += 1;
        }

        self.entries = entries;
    }

    fn cap(&self) -> usize {
        self.entries.len()
    }

    pub(crate) fn get(&self, key: *const ObjString) -> Option<Value> {
        if self.size == 0 {
            return None;
        }

        let slot = find_slot(&self.entries, key);
        let entry = &self.entries[slot];
        if entry.key.is_null() {
            return None;
        }

        Some(entry.value)
    }

    pub(crate) fn set(&mut self, gc: Gc, key: *mut ObjString, value: Value) -> bool {
        if (self.size + 1) as f64 > (self.cap() as f64) * TABLE_MAX_LOAD {
            let cap = grow_cap(self.cap());
            self.resize(gc, cap);
        }

        let slot = find_slot(&self.entries, key);
        let entry = &mut self.entries[slot];

        let is_new = entry.key.is_null();
        if is_new && entry.value.is_nil() {
            self.size += 1;
        }

        entry.key = key;
        entry.value = value;

        is_new
    }

    pub(crate) fn delete(&mut self, key: *const ObjString) -> bool {
        if self.size == 0 {
            return false;
        }

        let slot = find_slot(&self.entries, key);
        self.delete_slot(slot)
    }

    pub(crate) fn delete_slot(&mut self, slot: usize) -> bool {
        let entry = &mut self.entries[slot];
        if entry.key.is_null() {
            // Already tombstoned, so there was no value here.
            return false;
        }

        // Put a tombstone in the entry so linear probing still considers this as part
        // of the chain.
        entry.key = ptr::null_mut();
        entry.value = Value::bool(false);

        true
    }

    pub(crate) fn extend(&mut self, gc: Gc, src: &Table) {
        for entry in &src.entries {
            if !entry.key.is_null() {
                self.set(gc, entry.key, entry.value);
            }
        }
    }

    pub(crate) fn find_string(&self, chars: &str, hash: u32) -> *mut ObjString {
        if self.size == 0 {
            return ptr::null_mut();
        }

        let cap = self.cap() as u32;

        let mut idx = hash % cap;
        loop {
            let entry = &self.entries[idx as usize];
            if entry.key.is_null() {
                if entry.value.is_nil() {
                    // Truly empty.
                    return ptr::null_mut();
                }
                // This was a tombstone, so keep searching.
            } else if unsafe { &*entry.key }.hash == hash && unsafe { &*entry.key }.chars == chars {
                // Found it!
                return entry.key;
            }

            idx = (idx + 1) % cap;
        }
    }

    pub(crate) fn remove_unreachable(&mut self) {
        for i in 0..self.cap() {
            let entry = &self.entries[i];
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

fn grow_cap(cap: usize) -> usize {
    if cap < 8 {
        8
    } else {
        2 * cap
    }
}
