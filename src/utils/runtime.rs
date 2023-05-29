

use inkwell::context::Context; 
use inkwell::module::{Module, Linkage};
use inkwell::AddressSpace;
use inkwell::values::FunctionValue;

use paste::paste;


// get runtime function symbol name in c library given function name
macro_rules! runtime_function_name {
    ($fn_name: ident) => {
        concat!("__serge_", stringify!($fn_name))
    };
}

macro_rules! runtime_function {
    ($fn_name: ident, $ctx: lifetime) => {
        paste! {
            fn [<get_runtime_$fn_name>](&self) -> FunctionValue<$ctx>;
        }
    };
}

/// get runtime function in LLVM module
macro_rules! get_runtime_function {
    ($fn_name: ident, $ctx: lifetime) => {
        paste! {
            fn [<get_runtime_$fn_name>](&self) -> FunctionValue<$ctx> {
                self.get_function(runtime_function_name!($fn_name)).expect("runtime function not found!")
            }
        }
    };
}



pub trait RuntimeLibrary<'ctx> {
    // panic
    runtime_function!(panic, 'ctx);
    // IO
    runtime_function!(read_i32, 'ctx);
    runtime_function!(read_i64, 'ctx);
    runtime_function!(print, 'ctx);
    runtime_function!(println, 'ctx);
    // int32
    runtime_function!(alloc_i32, 'ctx);
    runtime_function!(alloc_i32_literal, 'ctx);
    runtime_function!(extract_i32, 'ctx);
    // float64
    runtime_function!(alloc_f64, 'ctx);
    runtime_function!(alloc_f64_literal, 'ctx);
    runtime_function!(extract_f64, 'ctx);
    // unit
    runtime_function!(alloc_unit, 'ctx);
    // array
    runtime_function!(alloc_array, 'ctx);
    runtime_function!(array_length, 'ctx);
    runtime_function!(array_read_index, 'ctx);
    runtime_function!(array_write_index, 'ctx);
    runtime_function!(array_push_back, 'ctx);
    // tuple
    runtime_function!(make_tuple, 'ctx);
    runtime_function!(tuple_length, 'ctx);
    runtime_function!(extract_tuple_field, 'ctx);   

    fn insert_runtime_function_declaration(&self);
}

impl<'ctx> RuntimeLibrary<'ctx> for Module<'ctx> {
    // panic
    get_runtime_function!(panic, 'ctx);
    // IO
    get_runtime_function!(read_i32, 'ctx);
    get_runtime_function!(read_i64, 'ctx);
    get_runtime_function!(print, 'ctx);
    get_runtime_function!(println, 'ctx);
    // int32
    get_runtime_function!(alloc_i32, 'ctx);
    get_runtime_function!(alloc_i32_literal, 'ctx);
    get_runtime_function!(extract_i32, 'ctx);
    // float64
    get_runtime_function!(alloc_f64, 'ctx);
    get_runtime_function!(alloc_f64_literal, 'ctx);
    get_runtime_function!(extract_f64, 'ctx);
    // unit
    get_runtime_function!(alloc_unit, 'ctx);
    // array
    get_runtime_function!(alloc_array, 'ctx);
    get_runtime_function!(array_length, 'ctx);
    get_runtime_function!(array_read_index, 'ctx);
    get_runtime_function!(array_write_index, 'ctx);
    get_runtime_function!(array_push_back, 'ctx);
    // tuple
    get_runtime_function!(make_tuple, 'ctx);
    get_runtime_function!(tuple_length, 'ctx);
    get_runtime_function!(extract_tuple_field, 'ctx);

    fn insert_runtime_function_declaration(&self) {
        macro_rules! insert_runtime_function {
            ($va_arg: literal; $fn_name: ident : | $($param_tys: ident),* | => $ret_ty: ident) => {
                let fn_type = $ret_ty.fn_type(&[$($param_tys), *], $va_arg);
                let function = self.add_function(
                    runtime_function_name!($fn_name), 
                    fn_type, 
                    Some(Linkage::External));
            };
            ($va_arg: literal; $fn_name: ident : | | => $ret_ty: ident) => {
                let fn_type = $ret_ty.fn_type(&[], $va_arg);
                let function = self.add_function(
                    runtime_function_name!($fn_name), 
                    fn_type, 
                    Some(Linkage::External));
            };
        }
        let context = self.get_context();
        let i32_ty_llvm = context.i32_type();
        let f64_ty_llvm = context.f64_type();
        let ptr_ty_llvm = i32_ty_llvm.ptr_type(AddressSpace::default());
        let void_ty_llvm = context.void_type();

        let i32_ty = i32_ty_llvm.into();
        let f64_ty = f64_ty_llvm.into();
        let ptr_ty = ptr_ty_llvm.into();
        // panic
        insert_runtime_function!(false; panic: |ptr_ty| => void_ty_llvm);
        // IO
        insert_runtime_function!(false; read_i32: | | => ptr_ty_llvm);
        insert_runtime_function!(false; read_i64: | | => ptr_ty_llvm);
        insert_runtime_function!(false; print: |ptr_ty| => void_ty_llvm);
        insert_runtime_function!(false; println: |ptr_ty| => void_ty_llvm);
        // int32
        insert_runtime_function!(false; alloc_i32: | | => ptr_ty_llvm);
        insert_runtime_function!(false; alloc_i32_literal: |i32_ty| => ptr_ty_llvm);
        insert_runtime_function!(false; extract_f64: |ptr_ty| => f64_ty_llvm);
        // float64
        insert_runtime_function!(false; alloc_f64: | | => ptr_ty_llvm);
        insert_runtime_function!(false; alloc_f64_literal: |f64_ty| => ptr_ty_llvm);
        insert_runtime_function!(false; extract_f64: |ptr_ty| => f64_ty_llvm);
        // unit
        insert_runtime_function!(false; alloc_unit: | | => ptr_ty_llvm);
        // array
        insert_runtime_function!(false; alloca_array: |i32_ty| => ptr_ty_llvm);
        insert_runtime_function!(false; array_length: | | => i32_ty_llvm);
        insert_runtime_function!(false; array_read_index: |ptr_ty, i32_ty| => ptr_ty_llvm);
        insert_runtime_function!(false; array_write_index: |ptr_ty, i32_ty, ptr_ty| => void_ty_llvm);
        insert_runtime_function!(false; array_push_back: |ptr_ty, ptr_ty| => void_ty_llvm);
        // tuple
        insert_runtime_function!(true; make_tuple: |i32_ty| => void_ty_llvm);
        insert_runtime_function!(false; tuple_length: |ptr_ty| => i32_ty_llvm);
        insert_runtime_function!(false; extract_tuple_field: |ptr_ty, i32_ty| => i32_ty_llvm);
    }
}