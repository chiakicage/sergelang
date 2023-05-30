#include "GCObject.h"
#include "Allocator.h"
#include "Utility.h"
#include <cstring>
#include <algorithm>

/// \brief Runtime exposed Array object manipulate API

extern "C"
SergeArray *__serge_alloc_array() {
    int init_capacity = 4;
    auto ptr = static_cast<SergeArray *>(GCMalloc(sizeof(SergeArray)));

    ptr->MetaData.Kind = GCMetaData::Array;
    ptr->MetaData.Mark = 0;
    ptr->Length = 0;
    ptr->Capacity = init_capacity;
    ptr->DataPtr = RawMalloc(init_capacity * sizeof(GCObjectHandle));
    return ptr;
}

extern "C"
int __serge_array_length(const SergeArray *array) {
    return array->Length;
}

extern "C"
GCObjectHandle __serge_array_index(const SergeArray *array, const uint32_t index) {
    if (array->Length < index)
        __serge_panic("array index exceeded!");
    return static_cast<GCObjectHandle *>(array->DataPtr)[index];
}

extern "C"
void __serge_array_write_index(SergeArray *array, const SergeInt32 *index, const GCObjectHandle value) {
    auto raw_index = index->Data;
    if (array->Length < raw_index)
        __serge_panic("array index exceeded!");
    static_cast<GCObjectHandle *>(array->DataPtr)[raw_index] = value;
}


extern "C"
void __serge_array_push_back(SergeArray *array, GCObjectHandle value) {
    if (array->Length == array->Capacity) {
        // grow up array
        size_t old_capacity = array->Capacity;
        size_t new_capacity = old_capacity * 2;
        auto new_buffer = RawMalloc(new_capacity * sizeof(GCObjectHandle));
        std::memcpy(new_buffer, array->DataPtr, old_capacity * sizeof(GCObjectHandle));
        RawFree(array->DataPtr);
        array->DataPtr = new_buffer;
        array->Capacity = new_capacity;
    }
    auto length = array->Length;
    static_cast<GCObjectHandle *>(array->DataPtr)[length] = value;
    array->Length++;
}