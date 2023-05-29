#pragma once

#include "GCObject.h"
#include "Allocator.h"
#include "Utility.h"
#include <cstdarg>
#include <cstdint>

extern "C"
SergeTuple *__serge_make_tuple(int n, ...);


extern "C"
int __serge_tuple_length(const SergeTuple *Tuple);

extern "C"
GCObjectHandle __serge_extract_tuple_field(const SergeTuple *Tuple, const uint32_t Index);