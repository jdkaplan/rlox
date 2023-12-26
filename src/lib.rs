macro_rules! debug_log_gc {
    ($($arg:tt)*) => {{
        #[cfg(feature = "log_gc")]
        println!($($arg)*);
    }};
}

mod chunk;
mod compiler;
mod gc;
mod heap;
mod object;
mod scanner;
mod table;
mod value;
mod vm;

pub use vm::{InterpretError, InterpretResult, Vm};

pub const U8_COUNT: usize = (u8::MAX as usize) + 1;
pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;
