#pragma once

#include "GCObject.h"
#include <cstdint>


// a single object like int/float etc. or a tuple for mutilpe fields.
extern "C"
SergeEnum *__serge_make_enum(const uint32_t tag, const GCObjectHandle data);

extern "C"
SergeInt32 *__serge_extract_enum_tag(const SergeEnum *obj);

extern "C"
GCObjectHandle __serge_extract_enum_field(const SergeEnum *obj,/* uint32_t expected_tag, */ uint32_t field_index);
