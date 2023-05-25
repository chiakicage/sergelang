#pragma once

#include <cstdlib>
#include <cstdint>
#include <type_traits>

/// \file GCObject.h
/// \brief Define the metadata class of runtime GCObject




struct GCMetaData {
    enum GCObjectKind : uint8_t {
        Object = 0,
        Int,
        Float,
        Unit,
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
} __attribute__((packed));


// placeholder GC object metadata
struct SergeObject {
    GCMetaData  MetaData;
    void        *Ptr;

    static enum GCMetaData::GCObjectKind 
    getKind() { return GCMetaData::Object; }
};

struct SergeUnit {
    GCMetaData MetaData;

    static enum GCMetaData::GCObjectKind
    getKind() { return GCMetaData::Unit; }
};

struct SergeInt32 {
    GCMetaData  MetaData;
    int32_t     Data;

    static enum GCMetaData::GCObjectKind 
    getKind() { return GCMetaData::Int; }
};


struct SergeFloat64 {
    GCMetaData  MetaData;
    double      Data;

    static enum GCMetaData::GCObjectKind 
    getKind() { return GCMetaData::Float; }
};


struct SergeString {
    GCMetaData  MetaData;
    uint32_t    Length;
    // Pod string
    char        Data[0];

    static enum GCMetaData::GCObjectKind 
    getKind() { return GCMetaData::String; }
};


struct SergeArray {
    GCMetaData  MetaData;
    uint32_t    Length;
    void        *DataPtr;
    uint32_t    Capacity;

    static enum GCMetaData::GCObjectKind 
    getKind() { return GCMetaData::Array; }
};

typedef void *GCObjectHandle;

#define getMetaData(Handle) (static_cast<SergeObject *>(Handle)->MetaData)


// exposed allocaate API
extern "C" const SergeUnit *__serge_alloc_unit(); 
extern "C" SergeInt32 *__serge_alloc_i32();
extern "C" SergeInt32 *__serge_alloc_i32_literal(const int);
extern "C" SergeFloat64 *__serge_alloc_f64();
extern "C" SergeFloat64 *__serge_alloc_f64_literal(const double);


template <typename T, typename K>
bool isa(K value) {
    static_assert(std::is_pointer<K>::value || std::is_reference<K>::value,
        "require a pointer or a reference type!");
    if (value->MetaData.Kind == std::remove_pointer<T>::type::getKind()) 
        return true;
    return false;
}

template <typename T>
bool isa(void *ptr) {
    return isa<T>(static_cast<SergeObject *>(ptr));
}


template <typename T, typename K>
T dyn_cast(K value) {
    if (isa<T>(value))
        return static_cast<T>(value);
    return nullptr;
}
