#include "GCObject.h"
#include "Allocator.h"
#include "Int.h"
#include "Float.h"
#include "Unit.h"
#include "Array.h"
#include "io.h"


SergeArray *dummy_alloc() {
    SergeArray *Array1 = __serge_alloc_array(1);
    for (int i = 0; i < 5; ++i) {
        // dead alloc
        __serge_alloc_f64_literal(i);
        SergeInt32 *Elem = __serge_alloc_i32_literal(i);
        __serge_array_push_back(Array1, Elem);
    }

    SergeArray *Array2 = __serge_alloc_array(1);
    for (int i = 0; i < 5; ++i) {
        // dead alloc
        __serge_alloc_f64_literal(i);
        SergeInt32 *Elem = __serge_alloc_i32_literal(i * 2);
        __serge_array_push_back(Array2, Elem);
    }
    return Array1;
}

extern "C"
SergeUnit *__serge_user_main() {
    SergeArray *out = dummy_alloc();

    __serge_println(out);
    SergeUnit *unit = (SergeUnit *)__serge_alloc_unit();
    __serge_gc_collect();
    // check, `out` value should not be sweeped.
    __serge_println(out);
    return unit;
}

/// Example output:
// allocate object at 0x55d0e58b6eb0, size = 32
// allocate object at 0x55d0e58b6fa0, size = 24
// allocate object at 0x55d0e58b6fe0, size = 16
// allocate object at 0x55d0e58b7020, size = 24
// allocate object at 0x55d0e58b7060, size = 16
// allocate object at 0x55d0e58b70a0, size = 24
// allocate object at 0x55d0e58b70e0, size = 16
// allocate object at 0x55d0e58b7120, size = 24
// allocate object at 0x55d0e58b7160, size = 16
// allocate object at 0x55d0e58b71a0, size = 24
// allocate object at 0x55d0e58b71e0, size = 16
// allocate object at 0x55d0e58b7270, size = 32
// allocate object at 0x55d0e58b72c0, size = 24
// allocate object at 0x55d0e58b7300, size = 16
// allocate object at 0x55d0e58b7430, size = 24
// allocate object at 0x55d0e58b7470, size = 16
// allocate object at 0x55d0e58b74b0, size = 24
// allocate object at 0x55d0e58b74f0, size = 16
// allocate object at 0x55d0e58b7530, size = 24
// allocate object at 0x55d0e58b7570, size = 16
// allocate object at 0x55d0e58b75b0, size = 24
// allocate object at 0x55d0e58b75f0, size = 16
// [0, 1, 2, 3, 4, ]
// allocate object at 0x55d0e58b7a90, size = 12
// object at 0x55d0e3b350e0:
// object at 0x55d0e3b350e0:
// object at 0x7ffc75d9a5e0:
// object at 0xdc28b5196dc14800:
// object at 0x2:
// object at 0x2:
// object at (nil):
// object at (nil):
// object at (nil):
// object at 0x1007f241ae0fbe0:
// object at (nil):
// object at 0x9987f14158a9bfb7:
// object at 0x55d0e3b2d170:
// object at (nil):
// object at (nil):
// object at (nil):
// object at 0x9987f1415ec9bfb7:
// object at 0xcddedd97a34fbfb7:
// object at 0x7ffc00000000:
// object at 0x55d0e3b350e0:
// object at 0x55d0e3b350e0:
// object at 0x55d0e3b350e0:
// object at 0x7ffc75d9a690:
// object at 0x55d0e3b2e31f:
// object at 0x7ffc75d9a6b8:
// object at 0x55d0e3b350e0:
// object at 0x7ffc75d9a6c0:
// object at 0x55d0e3b2de7c:
// object at 0xc:
// object at 0x55d0e3b350e0:
// object at 0x7f241ae10788:
// object at 0x55d0e58b7a90:
// potential root object at 0x55d0e58b7a90: Unit ()
// object at 0x7ffc75d9a6e0:
// object at 0x55d0e3b2df23:
// object at 0x7ffc75d9a700:
// object at 0xc:
// object at 0x7ffc75d9a700:
// object at 0x7ffc75d9a6e8:
// object at 0x7ffc75d9a700:
// object at 0x55d0e3b2df35:
// object at 0x7ffc75d9a720:
// object at 0x55d0e3b2d344:
// object at 0x55d0e58b7a90:
// potential root object at 0x55d0e58b7a90: Unit ()
// object at 0x55d0e58b6eb0:
// potential root object at 0x55d0e58b6eb0: (array) {size = 5, capacity = 8, ptr = 0x55d0e58b7220}
// object at 0x7ffc75d9a740:
// object at 0x55d0e3b2defd:
// object at 0x7ffc75d9a838:
// object at 0x100000000:
// gc collect object at 0x55d0e58b75b0: (float) {data = 4.000000}
// gc collect object at 0x55d0e58b7570: (int) {data = 6}
// gc collect object at 0x55d0e58b7530: (float) {data = 3.000000}
// gc collect object at 0x55d0e58b74f0: (int) {data = 4}
// gc collect object at 0x55d0e58b74b0: (float) {data = 2.000000}
// gc collect object at 0x55d0e58b7470: (int) {data = 2}
// gc collect object at 0x55d0e58b7430: (float) {data = 1.000000}
// gc collect object at 0x55d0e58b7300: (int) {data = 0}
// gc collect object at 0x55d0e58b75f0: (int) {data = 8}
// gc collect object at 0x55d0e58b7120: (float) {data = 3.000000}
// gc collect object at 0x55d0e58b72c0: (float) {data = 0.000000}
// gc collect object at 0x55d0e58b6fa0: (float) {data = 0.000000}
// gc collect object at 0x55d0e58b6fe0: (int) {data = 0}
// gc collect object at 0x55d0e58b7020: (float) {data = 1.000000}
// gc collect object at 0x55d0e58b7060: (int) {data = 1}
// gc collect object at 0x55d0e58b70e0: (int) {data = 2}
// gc collect object at 0x55d0e58b7160: (int) {data = 3}
// gc collect object at 0x55d0e58b71a0: (float) {data = 4.000000}
// gc collect object at 0x55d0e58b70a0: (float) {data = 2.000000}
// gc collect object at 0x55d0e58b7270: (array) {size = 5, capacity = 8, ptr = 0x55d0e58b7630}
// gc collect object at 0x55d0e58b71e0: (int) {data = 4}
// [0, 1, 2, 3, 4, ]