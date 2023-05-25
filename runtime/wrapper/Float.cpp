#include "GCObject.h"
#include "Allocator.h"

/// \brief Runtime exposed Float object manipulate API

extern "C"
SergeFloat64 *__serge_alloc_f64() {
    auto ptr = static_cast<SergeFloat64 *>(GCMalloc(sizeof(SergeFloat64)));
    ptr->MetaData.Kind = GCMetaData::Float;
    ptr->MetaData.Mark = 0;
    return ptr;
}

extern "C"
SergeFloat64 *__serge_alloc_f64_literal(const double value) {
    auto ptr = __serge_alloc_f64();
    ptr->Data = value;
    return ptr;
}

extern "C"
double __serge_extract_f64(const SergeFloat64 *obj) {
    return obj->Data;
} 

extern "C"
SergeFloat64 *__serge_i32cvtf64(const SergeInt32 *obj) {
    int value = obj->Data;
    return __serge_alloc_f64_literal(static_cast<double>(value));
}