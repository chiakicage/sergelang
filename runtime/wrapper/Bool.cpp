#include "Allocator.h"
#include "GCObject.h"

extern "C"
SergeBool *__serge_alloc_bool() {
    SergeBool *obj = (SergeBool *)GCMalloc(sizeof(SergeBool));
    obj->MetaData.Kind = GCMetaData::Bool;
    obj->MetaData.Mark = 0;
    return obj;
}

extern "C"
SergeBool *__serge_alloc_bool_literal(const bool value) {
    SergeBool *obj = __serge_alloc_bool();
    obj->Data = value;
    return obj;
}

extern "C"
bool __serge_extract_bool(const SergeBool *Handle) {
    return Handle->Data;
}