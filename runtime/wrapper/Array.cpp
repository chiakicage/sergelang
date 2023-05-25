#include "GCObject.h"
#include "Allocator.h"
#include <cstring>
#include <algorithm>

/// \brief Runtime exposed Array object manipulate API

extern "C"
SergeArray *__serge_alloc_array(const size_t capacity) {
    size_t init_capacity = std::max(capacity, (size_t)4);
    auto ptr = static_cast<SergeArray *>(GCMalloc(sizeof(SergeArray)));

    ptr->MetaData.Kind = GCMetaData::Array;
    ptr->MetaData.Mark = 0;
    ptr->Length = 0;
    ptr->Capacity = init_capacity;
    ptr->DataPtr = RawMalloc(init_capacity * sizeof(GCObjectHandle));
    return ptr;
}

extern "C"
size_t __serge_array_length(const SergeArray *array) {
    return array->Length;
}

extern "C"
GCObjectHandle __serge_array_index(const SergeArray *array, const SergeInt32 *index) {
    auto raw_index = index->Data;
    return static_cast<GCObjectHandle *>(array->DataPtr)[raw_index];
}

extern "C"
void __serge_array_push_back(SergeArray *array, GCObjectHandle value) {
    if (array->Length == array->Capacity) {
        // grow up array
        size_t old_capacity = array->Capacity;
        size_t new_capacity = old_capacity * 2;
        auto new_buffer = RawMalloc(new_capacity);
        std::memcpy(new_buffer, array->DataPtr, old_capacity * sizeof(GCObjectHandle));
        RawFree(array->DataPtr);
        array->DataPtr = new_buffer;
    }
    auto length = array->Length;
    static_cast<GCObjectHandle *>(array->DataPtr)[length] = value;
    array->Length++;
}