

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <csetjmp>
#include <vector>
#include <list>
#include <unordered_set>

#include "Utility.h"
#include "GCObject.h"
#include "Allocator.h"
#include "io.h"

#ifdef __x86_64
#define read_frame_pointer(fp) __asm__ volatile("movq %%rbp, %0": "=r"(fp))
#endif

#ifdef __x86_64
#define read_stack_pointer(sp) __asm__ volatile("movq %%rsp, %0": "=r"(sp))
#endif


#ifdef __riscv
// use s0 as frame pointer, in calling convention, s0 also serves as saved register
#define read_frame_pointer(fp) __asm__ volatile("mv %0, x8": "=r"(fp))
#endif

#ifdef __riscv
#define read_stack_pointer(sp) __asm__ volatile("mv %0, x2": "=r"(sp))
#endif



// main iteration of mark alive objects
static void markReachableObject(GCObjectHandle);

/// \todo: trace free block list
union FreeBlockList {
    GCObjectHandle  Object; 
    FreeBlockList   *Next;
};


namespace {

struct AllocatorImpl {
    // Trace all objects allocated by the heap
    std::unordered_set<GCObjectHandle> Heaps;
    // Free lists.
    FreeBlockList *FreeListHead = nullptr;
    // stack 
    char *stack_top;

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

// mark array type object
static void markArray(GCObjectHandle Handle) {
    SergeArray *Array = static_cast<SergeArray *>(Handle);
    GCObjectHandle *Data = (GCObjectHandle *)Array->DataPtr;
    int Length = Array->Length;
    for (int i = 0; i < Length; ++i) {
        markReachableObject(Data[i]);
    }
}

// mark tuple type object
void markTuple(GCObjectHandle Handle) {
    SergeTuple *Tuple = static_cast<SergeTuple *>(Handle);
    int Length = Tuple->Length;
    for (int i = 0; i < Length; ++i) {
        markReachableObject(Tuple->Fields[i]);
    }
}

void markReachableObject(GCObjectHandle Handle) {
    GCMetaData &MetaData = getMetaData(Handle);
    if (unlikely(MetaData.Mark)) {
       return; 
    }

    MetaData.Mark = 1;
    switch (MetaData.Kind) {
    // plain of data type, do nothing.
    case GCMetaData::Int:
    case GCMetaData::Float:
    case GCMetaData::Unit:
        break;
    case GCMetaData::Array: {
        markArray(Handle);
        break;
    }
    case GCMetaData::Tuple: {
        markTuple(Handle);
        break;
    }
    default:
        break;
    }
}

static AllocatorImpl serge_allocator;

void AllocatorImpl::mark() {
    // search the stack and find the root
    jmp_buf context;
    setjmp(context);

    // x86, riscv requires 16 aligned
    char *stack_pointer = stack_top;
    read_stack_pointer(stack_pointer);

    std::vector<GCObjectHandle> potential_root;

    for (; stack_pointer < stack_top; stack_pointer += sizeof(GCObjectHandle)) {
        GCObjectHandle ptr = *(GCObjectHandle *)stack_pointer;
        fprintf(stderr, "object at %p: \n", ptr);
        if (Heaps.count(ptr)) {
            potential_root.push_back(ptr);
            SERGE_DEBUG({ 
                fprintf(stderr, "potential root object at %p: ", ptr);
                serge_debug_dump_object(ptr);
            });
        }
    }
    // Do mark
    for (auto root : potential_root) {
        getMetaData(root).Mark = 1;
        markReachableObject(root);        
    }
}

void AllocatorImpl::sweep() {
    std::vector<GCObjectHandle> Worklist;
    for (auto OB : Heaps) {
        GCMetaData &MetaData = getMetaData(OB);
        if (unlikely(MetaData.Mark != 1)) {
            SERGE_DEBUG({
                fprintf(stderr, "gc collect object at %p: ", OB);
                serge_debug_dump_object(OB);
            });
            
            Worklist.push_back(OB);
        }
        // clear GC mark.
        MetaData.Mark = 0;
    }

    // clean
    for (auto Obj : Worklist) {
        deallocate(Obj);
        Heaps.erase(Obj);
    }
}

void *AllocatorImpl::allocate(size_t n) {
    void *ptr = calloc(1, n);
    if (unlikely(ptr == nullptr)) {
        __serge_panic("allocator failed");
    }
    SERGE_DEBUG({
        fprintf(stderr, "allocate object at %p, size = %ld\n", ptr, n);
    });
    Heaps.insert(ptr);
    return ptr;
}


void AllocatorImpl::deallocate(void *ptr) {
    if (auto array = dyn_cast<SergeArray *>(ptr)) {
        // handle object deallocation only, dispatch
        RawFree(array->DataPtr);
    }
    RawFree(ptr);
}

void AllocatorImpl::initialize_all() {

}

// exposed allocator api implementation.
extern "C"
SergeUnit *__serge_user_main(void);

extern "C"
int main(int argc, char **argv) {
    // we must set the stack top at the very first of the main function.
    // otherwise some stack area will overlap with potential root.
    read_frame_pointer(serge_allocator.stack_top);
    serge_allocator.initialize_all();
    __serge_user_main();
    return 0;
}



extern "C"
void *__serge_alloc(size_t size) {
    return serge_allocator.allocate(size);
}


/// runtime C ffi, triger GC manually
/// we expect do full gc (all generation) by default.
/// \todo different generation GC algorithm.
extern "C"
void __serge_gc_collect(void) {
    serge_allocator.mark();
    serge_allocator.sweep();
}

void *GCMalloc(size_t) alias("__serge_alloc");

/// \todo: we expect allocator API with GC has an internal generation optimization in the future,
/// raw allocator API manages raw data buffers. This alias is a work-around now.
void *RawMalloc(size_t n) {
    return malloc(n);
}

void RawFree(void *ptr) {
    return free(ptr);
}


