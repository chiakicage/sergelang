#include "GCObject.h"
#include "Allocator.h"



extern "C"
SergeFloat64 *__serge_runtime_alloc_f64() {
    auto ptr = static_cast<SergeFloat64 *>(GCMalloc(sizeof(SergeFloat64)));
    ptr->MetaData.Kind = GCMetaData::Float;
    return ptr;
}

extern "C"
SergeFloat64 *__serge_runtime_alloc_f64_literal(double value) {
    auto ptr = __serge_runtime_alloc_f64();
    ptr->Data = value;
    return ptr;
}

extern "C"
double __serge_runtime_extract_f64(SergeFloat64 *obj) {
    return obj->Data;
} 

extern "C"
SergeFloat64 *__serge_runtime_i32cvtf64(SergeInt32 *obj) {
    int value = obj->Data;
    return __serge_runtime_alloc_f64_literal(static_cast<double>(value));
}