#include "GCObject.h"
#include "Utility.h"
#include <cstdint>
#include <vector>

struct GCAllocator {
    GCObjectHandle Root;
    std::vector<GCObjectHandle> Heaps;

    inline static GCObjectHandle GCMalloc();
    inline static void           GCFree(GCObjectHandle);
};

GCAllocator Allocator;

// mark array type object
static void doMarkArray(GCObjectHandle);

// main iteration of mark alive objects
static void doMarkChildren(GCObjectHandle);

void doMarkArray(GCObjectHandle Handle) {
    SergeArray *Array = static_cast<SergeArray *>(Handle);
    for (uint32_t i = 0; i < Array->Length; ++i) {
        doMarkChildren(static_cast<GCObjectHandle *>(Array->DataPtr)[i]);
    }
}

void doMarkChildren(GCObjectHandle Handle) {
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
        doMarkArray(Handle);
        break;
    default:
        break;
    }
}

void markAliveObject() {
    for (auto Handle : Allocator.Heaps) {
        GCMetaData &MetaData = getMetaData(Handle);
        // mark all object unreachable
        MetaData.Mark = 0;
    } 
    // mark all reachablel obejct
    auto Root = static_cast<SergeObjectUnit *>(Allocator.Root);
    doMarkChildren(Root);
}

void sweepDeadObject() {
    // clear all unreablable objects.
    for (auto Handle : Allocator.Heaps) {
        GCMetaData &MetaData = getMetaData(Handle);
        if (unlikely(!MetaData.Mark)) {
            // do free work.
        }
    }
}




void __serge_gc_collect() {
    markAliveObject();
    sweepDeadObject();
}