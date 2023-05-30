
#include "GCObject.h"
#include "Int.h"
#include "Unit.h"
#include "io.h"

extern "C"
SergeUnit *__serge_user_main() {
    SergeInt32 *a = (SergeInt32 *)__serge_read_i32();
    SergeInt32 *b = (SergeInt32 *)__serge_read_i32();
    int _a = __serge_extract_i32(a);
    int _b = __serge_extract_i32(b);
    int _sum = _a + _b;
    SergeInt32 *sum = __serge_alloc_i32_literal(_sum);

    __serge_println(sum);
    SergeUnit *unit = (SergeUnit *)__serge_alloc_unit();
    return unit;
}
