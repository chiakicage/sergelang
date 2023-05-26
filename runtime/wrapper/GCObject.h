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
        Bool,
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

struct SergeBool {
    GCMetaData  MetaData;
    bool        Data;
<<<<<<< HEAD

    static enum GCMetaData::GCObjectKind
    getKind() { return GCMetaData::Bool; }
};

=======

    static enum GCMetaData::GCObjectKind
    getKind() { return GCMetaData::Bool; }
};

>>>>>>> c214281e4abdb8933fc830aadb1d8fca2f5e346f
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

struct SergeTuple {
    GCMetaData  MetaData;
    uint32_t    Length;
    void        *Fields[0];

    static enum GCMetaData::GCObjectKind
    getKind() { return GCMetaData::Tuple; }
};

typedef void *GCObjectHandle;

#define getMetaData(Handle) (static_cast<SergeObject *>(Handle)->MetaData)

<<<<<<< HEAD

/// Runtime API. When we enter a new domain, e.g. a new function,
/// this will create a new root for the current program space.
/// @TODO
extern "C"
void __serge_create_gc_root(void);
=======
>>>>>>> c214281e4abdb8933fc830aadb1d8fca2f5e346f

//// Runtime API, should be paired with `__serge_drop_gc_root`,
/// collect unused object created between create and drop gc root.
/// @TODO
extern "C"
void __serge_drop_gc_root(void);

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
