#pragma once

#include "GCObject.h"

/// As we use Int32 as Serge Language's default integer type, 
// we use 'int' a.k.a. int32 to represent size

extern "C"
SergeArray *__serge_alloc_array(const int capacity);

extern "C"
int __serge_array_length(const SergeArray *array);

extern "C"
GCObjectHandle __serge_array_index(const SergeArray *array, const SergeInt32 *index);

extern "C"
void __serge_array_push_back(SergeArray *array, GCObjectHandle value);

