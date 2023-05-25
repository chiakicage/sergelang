
#include "GCObject.h"

extern "C"
int __serge_extract_i32(const SergeInt32 *obj);
extern "C"
double __serge_extract_f64(const SergeFloat64 *obj);
extern "C"
const SergeInt32 *__serge_read_i32();
extern "C"
const SergeFloat64 *__serge_read_f64();
extern "C"
const SergeUnit *println(const GCObjectHandle Handle);

extern "C"
SergeUnit *__serge_user_main() {
    SergeInt32 *a = (SergeInt32 *)__serge_read_i32();
    SergeInt32 *b = (SergeInt32 *)__serge_read_i32();
    int _a = __serge_extract_i32(a);
    int _b = __serge_extract_i32(b);
    int _sum = _a + _b;
    SergeInt32 *sum = __serge_alloc_i32_literal(_sum);

    println(sum);
    SergeUnit *unit = (SergeUnit *)__serge_alloc_unit();
    return unit;
}
