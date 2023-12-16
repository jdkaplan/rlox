mod alloc;
mod chunk;
mod compiler;
mod object;
mod scanner;
mod table;
mod value;
mod vm;

pub use vm::{InterpretError, InterpretResult, Vm};

pub const U8_COUNT: usize = (u8::MAX as usize) + 1;
pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;
