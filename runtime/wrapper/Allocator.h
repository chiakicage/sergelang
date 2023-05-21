#pragma once

#include <cstdint>
#include <cstddef>

/// \brief internal GC manager function, allocate runtime object.
void *GCMalloc(size_t size);

/// \brief internal GC manager function, free runtime object.
void GCFree(void *ptr);

/// \brief internal function, allocate raw memory.
void *RawMalloc(size_t size);

/// \brief internal function, free raw memory.
void RawFree(void *ptr);