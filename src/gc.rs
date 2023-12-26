use std::ptr::NonNull;

use crate::compiler::Compiler;
use crate::heap::HeapObj;
use crate::vm::Vm;

const GC_GROWTH_FACTOR: usize = 2;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Gc<'compiler> {
    pub(crate) vm: NonNull<Vm>,
    pub(crate) compiler: Option<NonNull<Compiler<'compiler>>>,

    can_gc: bool,
}

impl<'compiler> Gc<'compiler> {
    pub(crate) fn comptime(compiler: NonNull<Compiler<'compiler>>, vm: &mut Vm) -> Self {
        Self {
            compiler: Some(compiler),
            vm: NonNull::from(vm),
            can_gc: true,
        }
    }

    pub(crate) fn runtime(vm: &mut Vm) -> Self {
        Self {
            compiler: None,
            vm: NonNull::from(vm),
            can_gc: true,
        }
    }

    pub(crate) fn boot(vm: &mut Vm) -> Self {
        Self {
            compiler: None,
            vm: NonNull::from(vm),
            can_gc: false,
        }
    }
}

impl Gc<'_> {
    pub(crate) fn _run_collection(&mut self) {
        if !self.can_gc {
            return;
        }

        #[cfg(feature = "stress_gc")]
        {
            debug_log_gc!("-- gc stress");
            self.collect_garbage();
        }

        let heap = &mut unsafe { self.vm.as_mut() }.heap;

        if heap.bytes_allocated > heap.next_gc {
            debug_log_gc!("-- gc now: {} > {}", heap.bytes_allocated, heap.next_gc);
            self.collect_garbage();
        }
    }

    pub(crate) fn _claim<T: HeapObj>(&mut self, obj: Box<T>) -> NonNull<T> {
        let heap = &mut unsafe { self.vm.as_mut() }.heap;
        heap.claim(obj)
    }
}

impl Gc<'_> {
    pub(crate) fn collect_garbage(&mut self) {
        self.mark_roots();

        let heap = &mut unsafe { self.vm.as_mut() }.heap;
        heap.trace_references();

        #[cfg(feature = "log_gc")]
        let before = heap.bytes_allocated;

        heap.sweep();

        heap.next_gc = heap.bytes_allocated * GC_GROWTH_FACTOR;

        #[cfg(feature = "log_gc")]
        let after = heap.bytes_allocated;
        debug_log_gc!(
            "   gc collected {} bytes ({} => {}) next at {}",
            before - after,
            before,
            after,
            heap.next_gc
        );
    }
}

// Mark
impl Gc<'_> {
    fn mark_roots(&mut self) {
        let vm = unsafe { self.vm.as_mut() };

        debug_log_gc!("--- mark vm");
        vm.mark_roots();

        debug_log_gc!("--- mark compilers");
        Compiler::mark_roots(self.compiler, &mut vm.heap);
    }
}
