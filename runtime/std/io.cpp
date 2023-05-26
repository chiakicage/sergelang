#include <cstdio>
#include <cstdlib>
#include "GCObject.h"
#include "Utility.h"
#include "Unit.h"
#include "Int.h"
#include "Float.h"


namespace {
void print_object_internal(GCObjectHandle);
}


extern "C" void 
__serge_panic(const char *msg) {
    printf("[panic]: %s\n", msg);
    exit(1);
}

extern "C"
const SergeUnit *__serge_print(const GCObjectHandle Handle) {
    print_object_internal(Handle);
    return __serge_alloc_unit();
}

extern "C"
const SergeUnit *__serge_println(const GCObjectHandle Handle) {
    print_object_internal(Handle);
    printf("\n");
    return __serge_alloc_unit();
}


extern "C"
SergeInt32 *__serge_read_i32() {
    int value = 0;
    scanf("%d", &value);
    return __serge_alloc_i32_literal(value);
}

extern "C"
SergeFloat64 *__serge_read_f64() {
    double value = 0.0;
    scanf("%lf", &value);
    return __serge_alloc_f64_literal(value);
}


namespace {
void print_object_internal(GCObjectHandle Handle) {
    switch (((SergeObject *)Handle)->MetaData.Kind) {
        case GCMetaData::Unit: {
            printf("()");
            break;
        }
        case GCMetaData::String: {
            auto Str = (SergeString *)Handle;
            // TODO
            break;
        }
        case GCMetaData::Int: {
            auto Int = (SergeInt32 *)Handle;
            printf("%d", Int->Data);
            break;
        }
        case GCMetaData::Float: {
            auto Float = (SergeFloat64 *)Handle;
            printf("%f", Float->Data);
            break;
        }
        case GCMetaData::Array: {
            auto Array = (SergeArray *)Handle;
            int size = Array->Length;
            GCObjectHandle *data = (GCObjectHandle *)Array->DataPtr;
            printf("[");
            for (int index = 0; index < size; ++index) {
                print_object_internal(data[index]);
                printf(",");
            }
            printf("]\n");
            break;
        }
        default:
            __serge_panic("Unhandled print object!");
    }
}

}


