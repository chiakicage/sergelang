
pub fn serge_gc_collect() {
    extern "C" {
        fn __serge_gc_collect();
    }
    unsafe {
        __serge_gc_collect();
    }
}