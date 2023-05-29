#pragma once

#include "GCObject.h"

extern "C"
SergeInt32 *__serge_alloc_i32();

extern "C"
SergeInt32 *__serge_alloc_i32_literal(const int value);

extern "C"
int __serge_extract_i32(const SergeInt32 *obj);

extern "C"
SergeInt32 *__serge_f64cvtf32(const SergeFloat64 *obj);


/// Designed for loop iteration
extern "C"
SergeInt32 *__serge_increase_i32(SergeInt32 *obj);