#include "GCObject.h"
#include "Int.h"
#include "Unit.h"
#include "Array.h"
#include "io.h"

extern "C"
SergeUnit *__serge_user_main() {
    SergeInt32 *n = __serge_read_i32();
    int _n = __serge_extract_i32(n);
    SergeArray *array = __serge_alloc_array(1);
    for (int i = 0; i < _n; ++i) {
        SergeInt32 *x = __serge_read_i32();
        __serge_array_push_back(array, x);
    }

    __serge_println(array);
    __serge_println(__serge_array_index(array, __serge_alloc_i32_literal(1)));
    SergeUnit *unit = (SergeUnit *)__serge_alloc_unit();
    return unit;
}