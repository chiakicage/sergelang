#pragma once

#include "GCObject.h"

extern "C"
SergeFloat64 *__serge_alloc_f64();

extern "C"
SergeFloat64 *__serge_alloc_f64_literal(const double value);

extern "C"
double __serge_extract_f64(const SergeFloat64 *obj);

extern "C"
SergeFloat64 *__serge_i32cvtf64(const SergeInt32 *obj);