#pragma once

#include "GCObject.h"

extern "C"
void __serge_print(const GCObjectHandle Handle);

extern "C"
void __serge_println(const GCObjectHandle Handle);

extern "C"
SergeInt32 *__serge_read_i32();

extern "C"
SergeFloat64 *__serge_read_f64();


// Debug
void serge_debug_dump_object(const GCObjectHandle);