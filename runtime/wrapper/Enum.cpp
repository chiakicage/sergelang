#include "Enum.h"
#include "Tuple.h"
#include "Allocator.h"
#include "Utility.h"
#include <cstdint>

SergeEnum *__serge_make_enum(const uint32_t tag, const GCObjectHandle data) {
    SergeEnum *obj = (SergeEnum *)GCMalloc(sizeof(SergeEnum));
    obj->MetaData.Kind = GCMetaData::Enum;
    obj->MetaData.Mark = 0;
    
    obj->CtorTag = tag;
    obj->Data = data;
    return obj;
}

uint32_t __serge_extract_enum_tag(const SergeEnum *obj) {
    return obj->CtorTag;
}

GCObjectHandle __serge_extract_enum_field(const SergeEnum *obj, 
                                            uint32_t expected_tag, 
                                            uint32_t field_index)
{
    uint32_t tag = __serge_extract_enum_tag(obj);
    if (tag != expected_tag)
        __serge_panic("unmatched enumeration ctor!");
    
    if (auto tuple = dyn_cast<SergeTuple *>(obj->Data)) {
        return __serge_extract_tuple_field(tuple, field_index);
    }
    __serge_panic("enumeration field is not a tuple!");
}