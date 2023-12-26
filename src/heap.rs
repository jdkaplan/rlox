use std::mem;
use std::ptr::NonNull;

use crate::object::{
    Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjType, ObjUpvalue,
};
use crate::table::Table;
use crate::value::Value;

pub(crate) struct Heap {
    pub(crate) strings: Table,
    pub(crate) objects: Option<NonNull<Obj>>,

    pub(crate) gc_pending: Vec<NonNull<Obj>>,

    pub(crate) bytes_allocated: usize,
    pub(crate) next_gc: usize,
}

/// # Safety
///
/// Types that implement HeapObj must be referenceable as an Obj pointer.
/// Specifically, they must be repr(C) and contain an Obj value as their first field.
pub(crate) unsafe trait HeapObj {}

impl Heap {
    pub(crate) fn new() -> Self {
        Self {
            strings: Table::new(),
            objects: None,

            gc_pending: Vec::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
        }
    }

    pub(crate) fn claim<T: HeapObj>(&mut self, obj: Box<T>) -> NonNull<T> {
        self.bytes_allocated += mem::size_of::<T>();

        // Safety: The Box pointer is always non-null.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(obj)) };

        // Push the (type-erased) pointer onto the list.
        {
            let mut obj = ptr.cast::<Obj>();
            unsafe { obj.as_mut() }.next = self.objects;
            self.objects = Some(obj);
        }

        ptr
    }
}

impl Heap {
    pub(crate) fn mark_value(&mut self, value: Value) {
        if value.is_obj() {
            self.mark_obj(unsafe { value.as_obj::<Obj>() });
        }
    }

    pub(crate) fn mark_obj(&mut self, mut obj: NonNull<Obj>) {
        debug_log_gc!("mark_obj: {:?}", obj);

        let obj_ref = unsafe { obj.as_mut() };

        if obj_ref.is_marked {
            return;
        }
        obj_ref.is_marked = true;

        self.gc_pending.push(obj);
    }

    pub(crate) fn mark_table(&mut self, table: &mut Table) {
        for entry in &mut table.entries {
            if let Some(key) = entry.key {
                self.mark_obj(key.cast::<Obj>());
            }
            self.mark_value(entry.value);
        }
    }
}

impl Heap {
    pub(crate) fn trace_references(&mut self) {
        debug_log_gc!("--- trace references");
        while let Some(obj) = self.gc_pending.pop() {
            self.expand_obj(obj);
        }
    }

    fn expand_obj(&mut self, obj: NonNull<Obj>) {
        debug_log_gc!("expand obj: {:?}", obj);
        match unsafe { obj.as_ref().ty } {
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

impl Heap {
    pub(crate) fn sweep(&mut self) {
        // The keys in the interned strings table are weak references to string objects. Remove
        // the unreachable ones before freeing the data behind those pointers.
        self.strings.remove_unreachable();

        debug_log_gc!("--- sweep");

        let mut prev = None;
        let mut obj = self.objects;

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
                // The first object was the unreachable one, so replace the head of the list.
                self.objects = obj;
            }

            // ... and free it.
            self.free(unreachable)
        }
    }

    fn free(&mut self, obj: NonNull<Obj>) {
        let bytes = Obj::free(obj);
        self.bytes_allocated -= bytes;
    }
}
