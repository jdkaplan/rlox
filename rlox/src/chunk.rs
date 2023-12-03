use std::ffi::c_int;

use crate::alloc::Gc;
use crate::value::Value;
use crate::{Bytecode, Lines, Values};

#[repr(C)]
pub struct Chunk {
    code: Bytecode,
    constants: Values,
    lines: Lines,
}

impl Chunk {
    pub fn init(&mut self) {
        self.code.0.init();
        self.constants.0.init();
        self.lines.0.init();
    }

    pub fn free(&mut self, gc: Gc) {
        self.code.0.free(gc);
        self.constants.0.free(gc);
        self.lines.0.free(gc);
    }

    pub fn write(&mut self, gc: Gc, byte: u8, line: c_int) {
        self.code.0.push(gc, byte);
        self.lines.0.push(gc, line);
    }

    pub fn add_constant(&mut self, gc: Gc, value: Value) -> u8 {
        let idx = self.constants.0.len();

        // GC: Ensure `value` is reachable temporarily in case resizing the constants
        // array triggers garbage collection.
        //
        // TODO: Use Alloc stash
        unsafe { gc.vm.as_mut().unwrap() }.push(value);
        self.constants.0.push(gc, value);
        unsafe { gc.vm.as_mut().unwrap() }.pop();

        idx as u8
    }
}
