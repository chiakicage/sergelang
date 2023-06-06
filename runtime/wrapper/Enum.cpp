#include "Enum.h"
#include "Tuple.h"
#include "Int.h"
#include "Allocator.h"
#include "Utility.h"
#include <cstdint>

extern "C"
SergeEnum *__serge_make_enum(const uint32_t tag, const GCObjectHandle data) {
    SergeEnum *obj = (SergeEnum *)GCMalloc(sizeof(SergeEnum));
    obj->MetaData.Kind = GCMetaData::Enum;
    obj->MetaData.Mark = 0;
    
    obj->CtorTag = tag;
    obj->Data = data;
    return obj;
}

extern "C"
SergeInt32 *__serge_extract_enum_tag(const SergeEnum *obj) {
    return __serge_alloc_i32_literal(obj->CtorTag);
}

extern "C"
GCObjectHandle __serge_extract_enum_field(const SergeEnum *obj, 
                                            // SergeInt32 *expected_tag, 
                                            uint32_t field_index)
{
    // SergeInt32 *tag = __serge_extract_enum_tag(obj);
    // if (tag->Data != expected_tag->Data)
    //     __serge_panic("unmatched enumeration ctor!");
    
    if (auto tuple = dyn_cast<SergeTuple *>(obj->Data)) {
        return __serge_extract_tuple_field(tuple, field_index);
    }
    __serge_panic("enumeration field is not a tuple!");
}
