

#include <cstddef>
#include <cstdlib>
#include <vector>
#include "Utility.h"
#include "GCObject.h"
#include "Allocator.h"



// mark array type object
static void markArray(GCObjectHandle);

// main iteration of mark alive objects
static void markReachableObject(GCObjectHandle);

/// \todo: trace free block list
union FreeBlockList {
    GCObjectHandle  Object; 
    FreeBlockList   *Next;
};


namespace {

struct AllocatorImpl {
    // GC roots, the start end of GC process.
    std::vector<GCObjectHandle>  GlobalVariable;
    // Trace all objects allocated by the heap
    std::vector<GCObjectHandle> Heaps;
    // Free lists.
    FreeBlockList *FreeListHead = nullptr;

    // GC main process.
    void mark();
    void sweep();

    // process GC object allocation and deallocation    
    void *allocate(size_t n);
    void deallocate(void *);

    // preallocated pools
    enum PersistentPool : size_t {
      Unit = 0,
      SIZE_OF_PERSISTENTPOOL,  
    };

    void initialize_all();
};

} // end namespace

void markArray(GCObjectHandle Handle) {
    SergeArray *Array = static_cast<SergeArray *>(Handle);
    for (uint32_t i = 0; i < Array->Length; ++i) {
        markReachableObject(static_cast<GCObjectHandle *>(Array->DataPtr)[i]);
    }
}


void markReachableObject(GCObjectHandle Handle) {
    GCMetaData &MetaData = getMetaData(Handle);
    if (unlikely(MetaData.Mark)) {
       return; 
    }

    MetaData.Mark = 1;
    switch (MetaData.Kind) {
    case GCMetaData::Int:
        break;
    case GCMetaData::Float:
        break;
    case GCMetaData::Array:
        markArray(Handle);
        break;
    default:
        break;
    }
}

static AllocatorImpl serge_allocator;

void AllocatorImpl::mark() {
    for (auto Root : GlobalVariable) {
        GCMetaData &MetaData = getMetaData(Root);
        markReachableObject(Root);
    }
}

void AllocatorImpl::sweep() {
    for (auto Obj : Heaps) {
        GCMetaData &MetaData = getMetaData(Obj);
        if (unlikely(MetaData.Mark != 1)) {
            deallocate(Obj);
        }
    }
}

void *AllocatorImpl::allocate(size_t n) {
    void *ptr = calloc(1, n);
    if (unlikely(ptr == nullptr)) {
        __serge_panic("allocator failed");
    }    
    return ptr;
}


void AllocatorImpl::deallocate(void *ptr) {
    if (auto array = dyn_cast<SergeArray *>(ptr)) {
        // handle object deallocation only, dispatch
        RawFree(array->DataPtr);
    }
    free(ptr);
}

void AllocatorImpl::initialize_all() {
    // Unit Object
    GlobalVariable.reserve(AllocatorImpl::SIZE_OF_PERSISTENTPOOL);
    SergeUnit *Unit = (SergeUnit *)RawMalloc(sizeof(SergeUnit));
    Unit->MetaData.Kind = GCMetaData::Unit;

}

extern "C"
const SergeUnit *__serge_alloc_unit() {
    return static_cast<SergeUnit *>
        (serge_allocator.GlobalVariable[AllocatorImpl::Unit]);
}

// exposed allocator api implementation.

extern "C" 
void __serge_gc_init(void) {
    /// \todo do some initialization works.
    serge_allocator.initialize_all();
    return;
} 

extern "C"
void *__serge_alloc(size_t size) {
    return serge_allocator.allocate(size);
}

extern "C"
void __serge_free(void *ptr) {
    serge_allocator.deallocate(ptr);
}

/// runtime C ffi, triger GC manually
/// we expect do full gc (all generation) by default.
/// \todo different generation GC algorithm.
extern "C"
void __serge_gc_collect(void) {
    serge_allocator.mark();
}

void *GCMalloc(size_t) alias("__serge_alloc");
void GCFree(void *ptr) alias("__serge_free");

/// \todo: we expect allocator API with GC has an internal generation optimization in the future,
/// raw allocator API manages raw data buffers. This alias is a work-around now.
void *RawMalloc(size_t n) {
    return malloc(n);
}

void RawFree(void *ptr) {
    return free(ptr);
}


