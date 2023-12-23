use std::ptr::NonNull;

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

#[derive(Clone, Default)]
#[repr(C)]
pub struct Entry {
    pub(crate) key: Option<NonNull<ObjString>>,
    pub(crate) value: Value,
}

fn find_slot(entries: &[Entry], key: NonNull<ObjString>) -> usize {
    let cap = entries.len() as u32;

    let mut idx = unsafe { key.as_ref() }.hash % cap;
    let mut tombstone = None;

    loop {
        let slot = idx as usize;
        let entry = &entries[slot];
        match entry.key {
            Some(k) => {
                if k == key {
                    return slot;
                }
            }
            None => {
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
            }
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
            let Some(key) = entry.key else {
                continue;
            };

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

    pub(crate) fn get(&self, key: NonNull<ObjString>) -> Option<Value> {
        if self.size == 0 {
            return None;
        }

        let slot = find_slot(&self.entries, key);
        let entry = &self.entries[slot];
        entry.key.map(|_| entry.value)
    }

    pub(crate) fn set(&mut self, gc: Gc, key: NonNull<ObjString>, value: Value) -> bool {
        if (self.size + 1) as f64 > (self.cap() as f64) * TABLE_MAX_LOAD {
            let cap = grow_cap(self.cap());
            self.resize(gc, cap);
        }

        let slot = find_slot(&self.entries, key);
        let entry = &mut self.entries[slot];

        let is_new = entry.key.is_none();
        if is_new && entry.value.is_nil() {
            self.size += 1;
        }

        entry.key = Some(key);
        entry.value = value;

        is_new
    }

    pub(crate) fn delete(&mut self, key: NonNull<ObjString>) -> bool {
        if self.size == 0 {
            return false;
        }

        let slot = find_slot(&self.entries, key);
        self.delete_slot(slot)
    }

    pub(crate) fn delete_slot(&mut self, slot: usize) -> bool {
        let entry = &mut self.entries[slot];
        if entry.key.is_none() {
            // Already tombstoned, so there was no value here.
            return false;
        }

        // Put a tombstone in the entry so linear probing still considers this as part
        // of the chain.
        entry.key = None;
        entry.value = Value::bool(false);

        true
    }

    pub(crate) fn extend(&mut self, gc: Gc, src: &Table) {
        for entry in &src.entries {
            if let Some(key) = entry.key {
                self.set(gc, key, entry.value);
            }
        }
    }

    pub(crate) fn find_string(&self, chars: &str, hash: u32) -> Option<NonNull<ObjString>> {
        if self.size == 0 {
            return None;
        }

        let cap = self.cap() as u32;

        let mut idx = hash % cap;
        loop {
            let entry = &self.entries[idx as usize];
            match entry.key {
                None => {
                    if entry.value.is_nil() {
                        // Truly empty.
                        return None;
                    }
                    // This was a tombstone, so keep searching.
                }
                Some(key) => {
                    let key = unsafe { key.as_ref() };
                    if key.hash == hash && key.chars == chars {
                        // Found it!
                        return entry.key;
                    }
                }
            }

            idx = (idx + 1) % cap;
        }
    }

    pub(crate) fn remove_unreachable(&mut self) {
        for i in 0..self.cap() {
            let entry = &self.entries[i];
            let Some(mut key) = entry.key else {
                continue;
            };

            if !unsafe { key.as_mut() }.obj.is_marked {
                self.delete(key);
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
