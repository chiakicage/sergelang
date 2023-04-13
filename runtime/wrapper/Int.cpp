#include "GCObject.h"
#include "Allocator.h"



extern "C"
SergeInt32 *__serge_runtime_alloc_i32() {
    auto ptr = static_cast<SergeInt32 *>(GCMalloc(sizeof(SergeInt32)));
    ptr->MetaData.Kind = GCMetaData::Int;
    return ptr;
}

extern "C"
SergeInt32 *__serge_runtime_alloc_i32_literal(int value) {
    auto ptr = __serge_runtime_alloc_i32();
    ptr->Data = value;
    return ptr;
}

extern "C"
int __serge_runtime_extract_i32(SergeInt32 *obj) {
    return obj->Data;
} 


extern "C"
SergeInt32 *__serge_runtime_i32cvtf64(SergeFloat64 *obj) {
    double value = obj->Data;
    return __serge_runtime_alloc_i32_literal(static_cast<int>(value));
}
