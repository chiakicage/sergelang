

#include <cstdlib>
#include "Utility.h"
#include "GCObject.h"
#include "Allocator.h"

extern "C"
void *__serge_runtime_alloc(size_t size) {
    void *ptr = calloc(1, size);
    if (unlikely(ptr == nullptr)) {
        __serge_runtime_panic("allocator failed");
    }    
    return ptr;
}

extern "C"
void __serge_runtime_free(void *ptr) {
    free(ptr);
}


void *GCMalloc(size_t) alias("__serge_runtime_alloc");
void GCFree(void *ptr) alias("__serge_runtime_free");

/// \todo: we expect allocator API with GC has an internal generation optimization in the future,
/// raw allocator API manages raw data buffers. This alias is a work-around now.
void *RawMalloc(size_t) alias("__serge_runtime_alloc");
void RawFree(void *ptr) alias("__serge_runtime_free");


