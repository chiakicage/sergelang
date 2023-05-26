#include "Tuple.h"

// Internal allocation.
extern "C"
SergeTuple *__serge_alloc_tuple(int n) {
    auto Tuple = static_cast<SergeTuple *>
        (GCMalloc(sizeof(SergeTuple) + sizeof(GCObjectHandle) * n));

    Tuple->MetaData.Kind = GCMetaData::Tuple;
    Tuple->Length = n;
    return Tuple;
}

extern "C"
const SergeTuple *__serge_make_tuple(int n, ...) {
    GCObjectHandle Handle;
    va_list args;
    va_start(args, n);

    auto Tuple = __serge_alloc_tuple(n);
    for (int i = 0; i < n; ++i) {
        Handle = va_arg(args, GCObjectHandle);
        Tuple->Fields[i] = Handle;
    }
    va_end(args);
    return Tuple;
}

extern "C"
int __serge_tuple_length(const SergeTuple *Tuple) {
    return Tuple->Length;
}

extern "C"
GCObjectHandle __serge_extract_tuple_field(const SergeTuple *Tuple, int Index) {
    if (Tuple->Length < Index) 
        __serge_panic("tuple index exceeded!");
    return Tuple->Fields[Index];
}