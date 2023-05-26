#pragma once

#include "GCObject.h"

extern "C"
const SergeUnit *__serge_print(const GCObjectHandle Handle);

extern "C"
const SergeUnit *__serge_println(const GCObjectHandle Handle);

extern "C"
const SergeInt32 *__serge_read_i32();

extern "C"
const SergeFloat64 *__serge_read_f64();