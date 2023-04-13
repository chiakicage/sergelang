#pragma once

#include <cstdlib>
#include <cstdint>

/// \file GCObject.h
/// \brief Define the metadata class of runtime GCObject




struct GCMetaData {
    enum GCObjectKind : uint8_t {
        Object = 0,
        Int,
        Float,
        String,
        Enum,
        Tuple,
        Array,
        Clousure,
        NumOfKind
    };

    // (reserved) pointer to object metadata
    void                    *GCPointer;
    
    // GCObject Type
    enum GCObjectKind       Kind;

    // (reserved) GC Tags
    uint8_t                 Tag;

    // mark and sweep GC object marks.
    uint16_t                Mark;
};


// placeholder GC object metadata
struct SergeObjectUnit {
    GCMetaData  MetaData;
    void        *Ptr;
};

struct SergeInt32 {
    GCMetaData  MetaData;
    int32_t     Data;
};


struct SergeFLoat64 {
    GCMetaData  MetaData;
    double      Data;
};


struct SeregeString {
    GCMetaData  MetaData;
    uint32_t    Length;
    // Pod string
    char        Data[0];
};


struct SergeArray {
    GCMetaData  MetaData;
    uint32_t    Length;
    void        *DataPtr;
    uint32_t    Capacity;
};

typedef void *GCObjectHandle;

#define getMetaData(Handle) (static_cast<SergeObjectUnit *>(Handle)->MetaData)
#define getKind(Handle) (getMetaData(Handle).Kind)
#define getMark(Handle) (getMetaData(Handle).Mark)


/// runtime C ffi, triger GC manually
/// we expect do full gc (all generation) by default.
/// \todo different generation GC algorithm.
extern "C"
void __serge_gc_collect();