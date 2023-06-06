use llvm_sys::core::*;
use llvm_sys::prelude::*;

use libc::*;
use std::ptr::null_mut;
use crate::backend::codegen::CodeGen;
use crate::utils::to_c_str;
use paste::paste;


// get runtime function symbol name in c library given function name
macro_rules! runtime_function_name {
    ($fn_name: ident) => {
        concat!("__serge_", stringify!($fn_name))
    };
}

macro_rules! runtime_function {
    ($fn_name: ident) => {
        paste! {
            fn [<get_runtime_$fn_name>](&self) -> LLVMValueRef;
        }
    };
}

/// get runtime function in LLVM module
macro_rules! get_runtime_function {
    ($fn_name: ident) => {
        paste! {
            fn [<get_runtime_$fn_name>](&self) -> LLVMValueRef {
                unsafe {
                    LLVMGetNamedFunction(self.module, to_c_str(runtime_function_name!($fn_name)).as_ptr())
                }
            }
        }
    };
}


pub trait RuntimeLibrary<'a> {
    // panic
    runtime_function!(panic);
    // IO
    runtime_function!(read_i32);
    runtime_function!(read_i64);
    runtime_function!(print);
    runtime_function!(println);
    // bool
    runtime_function!(alloc_bool);
    runtime_function!(alloc_bool_literal);
    runtime_function!(extract_bool);
    // int32
    runtime_function!(alloc_i32);
    runtime_function!(alloc_i32_literal);
    runtime_function!(extract_i32);
    // float64
    runtime_function!(alloc_f64);
    runtime_function!(alloc_f64_literal);
    runtime_function!(extract_f64);
    // unit
    runtime_function!(alloc_unit);
    // array
    runtime_function!(alloc_array);
    runtime_function!(array_length);
    runtime_function!(array_read_index);
    runtime_function!(array_write_index);
    runtime_function!(array_push_back);
    // tuple
    runtime_function!(make_tuple);
    runtime_function!(tuple_length);
    runtime_function!(extract_tuple_field);   
    // enum
    runtime_function!(make_enum);
    runtime_function!(extract_enum_tag);
    runtime_function!(extract_enum_field);

    fn insert_runtime_function_declaration(&mut self); 
}



impl<'a> RuntimeLibrary<'a> for CodeGen<'a> {
    // panic
    get_runtime_function!(panic);
    // IO
    get_runtime_function!(read_i32);
    get_runtime_function!(read_i64);
    get_runtime_function!(print);
    get_runtime_function!(println);
    // bool
    get_runtime_function!(alloc_bool);
    get_runtime_function!(alloc_bool_literal);
    get_runtime_function!(extract_bool);
    // int32
    get_runtime_function!(alloc_i32);
    get_runtime_function!(alloc_i32_literal);
    get_runtime_function!(extract_i32);
    // float64
    get_runtime_function!(alloc_f64);
    get_runtime_function!(alloc_f64_literal);
    get_runtime_function!(extract_f64);
    // unit
    get_runtime_function!(alloc_unit);
    // array
    get_runtime_function!(alloc_array);
    get_runtime_function!(array_length);
    get_runtime_function!(array_read_index);
    get_runtime_function!(array_write_index);
    get_runtime_function!(array_push_back);
    // tuple
    get_runtime_function!(make_tuple);
    get_runtime_function!(tuple_length);
    get_runtime_function!(extract_tuple_field);
    // enum
    get_runtime_function!(make_enum);
    get_runtime_function!(extract_enum_tag);
    get_runtime_function!(extract_enum_field);

    fn insert_runtime_function_declaration(&mut self) {
        macro_rules! insert_runtime_function {
            ($va_arg: literal; $fn_name: ident : | $($param_tys: ident),* | => $ret_ty: ident) => {
                let mut params = [$($param_tys), *];
                let fn_type = LLVMFunctionType($ret_ty, 
                                                params.as_mut_ptr(), 
                                                params.len() as u32, 
                                                $va_arg as LLVMBool);
                let func = LLVMAddFunction(self.module, 
                                    to_c_str(runtime_function_name!($fn_name)).as_ptr(), 
                                    fn_type);
                self.function_type_map.insert(func, fn_type);
            };
            ($va_arg: literal; $fn_name: ident : | | => $ret_ty: ident) => {
                let fn_type = LLVMFunctionType($ret_ty, 
                                                null_mut::<LLVMTypeRef>::(),
                                                0, 
                                                $va_arg as LLVMBool);
                let func = LLVMAddFunction(self.module, 
                                    to_c_str(runtime_function_name!($fn_name)).as_ptr(), 
                                    fn_type);
                self.function_type_map.insert(func, fn_type);
            };
        }
        unsafe {
            let void_type = LLVMVoidTypeInContext(self.context);
            let bool_type = LLVMInt1TypeInContext(self.context);
            let int_type = LLVMInt32TypeInContext(self.context);
            let float_type = LLVMDoubleTypeInContext(self.context);
            let ptr_type = LLVMPointerTypeInContext(self.context, 0);
            // panic
            insert_runtime_function!(false; panic: |ptr_type| => void_type);
            // IO
            insert_runtime_function!(false; read_i32: | | => ptr_type);
            insert_runtime_function!(false; read_i64: | | =>  ptr_type);
            insert_runtime_function!(false; print: | ptr_type | => void_type);
            insert_runtime_function!(false; println: | ptr_type | => void_type);
            // bool
            insert_runtime_function!(false; alloc_bool: | | => ptr_type);
            insert_runtime_function!(false; alloc_bool_literal: | bool_type | => ptr_type);
            insert_runtime_function!(false; extract_bool: | ptr_type | => bool_type);
            // int32
            insert_runtime_function!(false; alloc_i32: | | => ptr_type);
            insert_runtime_function!(false; alloc_i32_literal: | int_type | => ptr_type);
            insert_runtime_function!(false; extract_i32: | ptr_type | => int_type);
            // float64
            insert_runtime_function!(false; alloc_f64: |  | => ptr_type);
            insert_runtime_function!(false; alloc_f64_literal: | float_type | => ptr_type);
            insert_runtime_function!(false; extract_f64: | ptr_type | => float_type);
            // unit
            insert_runtime_function!(false; alloc_unit: |  | => ptr_type);
            // array
            insert_runtime_function!(false; alloc_array: | | => ptr_type);
            insert_runtime_function!(false; array_length: |  | => ptr_type);
            insert_runtime_function!(false; array_read_index: | ptr_type, int_type | => ptr_type);
            insert_runtime_function!(false; array_write_index: | ptr_type, ptr_type, ptr_type| => void_type);
            insert_runtime_function!(false; array_push_back: | ptr_type, ptr_type | => void_type);
            // tuple
            insert_runtime_function!(true; make_tuple: | int_type | => ptr_type);
            insert_runtime_function!(false; tuple_length: | ptr_type | => int_type);
            insert_runtime_function!(false; extract_tuple_field: | ptr_type, int_type | => ptr_type);
            // enum
            insert_runtime_function!(false; make_enum: | int_type, ptr_type | => ptr_type);
            insert_runtime_function!(false; extract_enum_tag: | ptr_type | => ptr_type);
            insert_runtime_function!(false; extract_enum_field: | ptr_type, int_type | => ptr_type);
        }
    }
}
