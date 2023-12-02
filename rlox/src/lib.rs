use std::alloc::{dealloc, realloc, Layout};
use std::ffi::c_void;
use std::ptr;

#[no_mangle]
pub extern "C" fn hello() {
    println!("Hello from Rust!");
}

#[no_mangle]
pub extern "C" fn _reallocate(ptr: *mut c_void, new: usize) -> *mut c_void {
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
