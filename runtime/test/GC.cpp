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
    return unit;
}

/// Expected output:
// [0, 1, 2, 3, 4, ]
// gc collect object at 0x5608e7be9a90: gc collect object at 0x5608e7be95b0: (float) {data = 4.000000}
// gc collect object at 0x5608e7be9570: (int) {data = 6}
// gc collect object at 0x5608e7be9530: (float) {data = 3.000000}
// gc collect object at 0x5608e7be94f0: (int) {data = 4}
// gc collect object at 0x5608e7be94b0: (float) {data = 2.000000}
// gc collect object at 0x5608e7be9470: (int) {data = 2}
// gc collect object at 0x5608e7be9430: (float) {data = 1.000000}
// gc collect object at 0x5608e7be9300: (int) {data = 0}
// gc collect object at 0x5608e7be95f0: (int) {data = 8}
// gc collect object at 0x5608e7be8eb0: (array) {size = 5, capacity = 4, ptr = 0x5608e7be9220}
// gc collect object at 0x5608e7be9120: (float) {data = 3.000000}
// gc collect object at 0x5608e7be92c0: (float) {data = 0.000000}
// gc collect object at 0x5608e7be8fa0: (float) {data = 0.000000}
// gc collect object at 0x5608e7be8fe0: (int) {data = 0}
// gc collect object at 0x5608e7be9020: (float) {data = 1.000000}
// gc collect object at 0x5608e7be9060: (int) {data = 1}
// gc collect object at 0x5608e7be90e0: (int) {data = 2}
// gc collect object at 0x5608e7be9160: (int) {data = 3}
// gc collect object at 0x5608e7be91a0: (float) {data = 4.000000}
// gc collect object at 0x5608e7be90a0: (float) {data = 2.000000}
// gc collect object at 0x5608e7be9270: (array) {size = 5, capacity = 4, ptr = 0x5608e7be9630}
// gc collect object at 0x5608e7be91e0: (int) {data = 4}