#include "GCObject.h"
#include "Int.h"
#include "Tuple.h"
#include "Unit.h"
#include "Array.h"
#include "io.h"

extern "C"
SergeUnit *__serge_user_main() {
    SergeInt32 *n = __serge_read_i32();
    SergeFloat64 *m = __serge_read_f64();
    int _n = __serge_extract_i32(n);
    SergeArray *array = __serge_alloc_array(1);
    for (int i = 0; i < _n; ++i) {
        SergeInt32 *x = __serge_read_i32();
        __serge_array_push_back(array, x);
    }

    SergeTuple *tuple = __serge_make_tuple(3, n, m, array);
    __serge_println(tuple);
    SergeUnit *unit = (SergeUnit *)__serge_alloc_unit();
    return unit;
}