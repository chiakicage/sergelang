#include "GCObject.h"
#include "Allocator.h"

/// \brief Runtime exposed Int object manipulate API

extern "C"
SergeInt32 *__serge_alloc_i32() {
    auto ptr = static_cast<SergeInt32 *>(GCMalloc(sizeof(SergeInt32)));
    ptr->MetaData.Kind = GCMetaData::Int;
    ptr->MetaData.Mark = 0;
    return ptr;
}

extern "C"
SergeInt32 *__serge_alloc_i32_literal(const int value) {
    auto ptr = __serge_alloc_i32();
    ptr->Data = value;
    return ptr;
}

extern "C"
int __serge_extract_i32(const SergeInt32 *obj) {
    return obj->Data;
} 


extern "C"
SergeInt32 *__serge_f64cvtf32(const SergeFloat64 *obj) {
    double value = obj->Data;
    return __serge_alloc_i32_literal(static_cast<int>(value));
}
