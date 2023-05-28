#pragma once
#include "GCObject.h"


extern "C"
SergeBool *__serge_alloc_bool();

extern "C"
SergeBool *__serge_alloc_bool_literal(const bool value);

extern "C"
bool __serge_extract_bool(const SergeBool *Handle);