#include "Unit.h"
#include "Allocator.h"

extern "C"
const SergeUnit *__serge_alloc_unit() {
    auto ptr = static_cast<SergeUnit *>(GCMalloc(sizeof(SergeUnit)));
    ptr->MetaData.Kind = GCMetaData::Unit;
    ptr->MetaData.Mark = 0;
    return ptr;
}