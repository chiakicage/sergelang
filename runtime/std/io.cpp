#include <cstdio>
#include <cstdlib>
#include "GCObject.h"
#include "Utility.h"
#include "Unit.h"
#include "Int.h"
#include "Float.h"
#include "io.h"


namespace {
void print_object_internal(GCObjectHandle Handle);
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


void serge_debug_dump_object(GCObjectHandle Handle) {
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
            fprintf(stderr, "(int) {data = %d}\n", Int->Data);
            break;
        }
        case GCMetaData::Float: {
            auto Float = (SergeFloat64 *)Handle;
            fprintf(stderr, "(float) {data = %f}\n", Float->Data);
            break;
        }
        case GCMetaData::Array: {
            auto Array = (SergeArray *)Handle;
            int size = Array->Length;
            int capacity = Array->Capacity;
            GCObjectHandle *data = (GCObjectHandle *)Array->DataPtr;
            fprintf(stderr, 
                        "(array) {size = %d, capacity = %d, ptr = %p}\n", size, capacity, data);
            break;
        }
        case GCMetaData::Tuple: {
            auto Tuple = (SergeTuple *)Handle;
            int size = Tuple->Length;
            GCObjectHandle *data = Tuple->Fields;
            fprintf(stderr, 
                "(tuple) {size = %d, field = %p}\n", size, data);
            break;
        }
        default:
            __serge_panic("Unhandled print object!");
    }    
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
                printf(", ");
            }
            printf("]");
            break;
        }
        case GCMetaData::Tuple: {
            auto Tuple = (SergeTuple *)Handle;
            int size = Tuple->Length;
            printf("(");
            for (int index = 0; index < size; ++index) {
                print_object_internal(Tuple->Fields[index]);
                printf(", ");
            }
            printf(")");
            break;
        }
        default:
            __serge_panic("Unhandled print object!");
    }
}

}


