use std::ffi::c_uint;

use crate::{Obj, ObjClosure, ObjString, ObjUpvalue, Table, Value, FRAMES_MAX, STACK_MAX};

#[repr(C)]
pub struct CallFrame {
    closure: *mut ObjClosure,
    ip: *mut u8,
    slots: *mut Value,
}

#[repr(C)]
pub struct Vm {
    // TODO: This is a Vec
    pub(crate) frames: [CallFrame; FRAMES_MAX],
    pub(crate) frame_count: c_uint,

    // TODO: This is a Vec
    pub(crate) stack: [Value; STACK_MAX],
    pub(crate) stack_top: *mut Value,

    pub(crate) globals: Table,

    pub(crate) strings: Table,
    pub(crate) init_string: *mut ObjString,

    pub(crate) open_upvalues: *mut ObjUpvalue,

    pub(crate) objects: *mut Obj,

    // TODO: This is a Vec
    pub(crate) gc_pending_len: usize,
    pub(crate) gc_pending_cap: usize,
    pub(crate) gc_pending_stack: *mut *mut Obj,

    pub(crate) bytes_allocated: usize,
    pub(crate) next_gc: usize,
}

impl Vm {
    pub(crate) fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }

    pub(crate) fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            *self.stack_top
        }
    }
}
