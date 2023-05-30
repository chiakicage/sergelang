pub use crate::midend::typed_ast::*;
use inkwell::basic_block;
use inkwell::values::*;
use inkwell::types::BasicMetadataTypeEnum;
// use inkwell::basic_block;
pub use crate::ast::*;
pub use crate::utils::types::*;
pub use inkwell::IntPredicate;
pub use inkwell::FloatPredicate;
use chumsky::container::Seq;
use rpds::HashTrieMap;
use std::{collections::{HashMap, HashSet}, any::Any, sync::mpsc::SyncSender, env::temp_dir};

type SymTable<K, V> = HashTrieMap<K, V>;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module}, types::{BasicType, AnyType, AsTypeRef}, values::{PointerValue, AsValueRef, BasicValue, BasicMetadataValueEnum}, AddressSpace,
};
use crate::utils::runtime::RuntimeLibrary;

pub struct CodeGen<'ctx, 'a> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
}

pub struct TypedPointerValue<'a> {
    pub pointer_type : Type,
    pub ptr : inkwell::values::PointerValue<'a>,
}

impl <'a>TypedPointerValue<'a> {
    pub fn new(
        pointer_type : Type,
        ptr :  inkwell::values::PointerValue<'a>,
    ) -> Self {
        Self {
            pointer_type, 
            ptr,
        }
    }
}

pub struct TypedPointervalue_table<'a> {
    pub TypePointer : Option<TypedPointerValue<'a>>,
    pub sym_table : SymTable<String, Type>,
    pub sym_ptr_table : SymTable<String, PointerValue<'a>>,
}

impl <'a>TypedPointervalue_table<'a> {
    pub fn new(
        pointer_type : Type,
        ptr :  inkwell::values::PointerValue<'a>,
        sym_table : SymTable<String, Type>,
        sym_ptr_table : SymTable<String, PointerValue<'a>>,
    ) -> Self {
        Self {
            TypePointer : Some(TypedPointerValue::new(pointer_type, ptr)),
            sym_table,
            sym_ptr_table,
        }
    }
    pub fn new_None(
        sym_table : SymTable<String, Type>,
        sym_ptr_table : SymTable<String, PointerValue<'a>>,
    )-> Self {
        Self {
            TypePointer : None,
            sym_table,
            sym_ptr_table,
        }
    }
}

impl<'a, 'ctx : 'a > CodeGen<'ctx, 'a> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder,
        }
    }

    pub fn codegen(&self) {
        self.module.insert_runtime_function_declaration();
        let i32_type = self.context.i32_type();
        let void_type = self.context.void_type();
        let fn_type = i32_type.fn_type(&[], false);
        let fn_value = self.module.add_function("main", fn_type, None);
        let getint_fn_type = i32_type.fn_type(&[], false);
        let putint_fn_type = void_type.fn_type(&[i32_type.into()], false);
        let putch_fn_type = void_type.fn_type(&[i32_type.into()], false);
        let getint_fn_value =
            self.module
                .add_function("getint", getint_fn_type, Some(Linkage::External));
        let putint_fn_value =
            self.module
                .add_function("putint", putint_fn_type, Some(Linkage::External));
        let putch_fn_value =
            self.module
                .add_function("putch", putch_fn_type, Some(Linkage::External));
        let basic_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(basic_block);
        let const_int = i32_type.const_int(12123, false);
        self.builder
            .build_call(putint_fn_value, &[const_int.into()], "");
        let enter_int = i32_type.const_int(48, false);
        self.builder
            .build_call(putch_fn_value, &[enter_int.into()], "");
        self.builder.build_return(Some(&const_int));
    }


    pub fn codegen_module(&self, typedast : & TypedModule) {
        let void_type = self.context.void_type();
        let i32_type = self.context.i32_type();
        let putint_fn_type = void_type.fn_type(&[i32_type.into()], false);
        let putint_fn_value =
            self.module
                .add_function("putint", putint_fn_type, Some(Linkage::External));

        let putch_fn_type = void_type.fn_type(&[i32_type.into()], false);
        let putch_fn_value =
            self.module
                .add_function("putch", putch_fn_type, Some(Linkage::External));

        let mut func_name_table : SymTable<String, FunctionValue> = SymTable::new();
        let mut new_table : SymTable<String, FunctionValue> = SymTable::new();
        for func in &typedast.func_defs {
            let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
            func_name_table.clone_into(&mut tmp_table);
            new_table = self.codegen_func(& func, tmp_table, putint_fn_value.clone(), putch_fn_value.clone());
            new_table.clone_into(&mut func_name_table);
        }

    }

    pub fn type2struct_type(& self, ser_type : Type) -> inkwell::types::StructType<'ctx>{
        match &ser_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Int => {
                        return self.context.struct_type(
                            &[self.context.i32_type().into()], 
                            false);
                    }
                    PrimitiveType::Float => {
                        return self.context.struct_type(
                            &[self.context.f64_type().into()], 
                            false);
                    }
                    PrimitiveType::Bool => {
                        return self.context.struct_type(
                            &[self.context.bool_type().into()], 
                            false);
                    }
                    PrimitiveType::Char => {
                        return self.context.struct_type(
                            &[self.context.bool_type().into()], 
                            false);
                    }
                    PrimitiveType::String => {
                        // {length, char * payload}
                        return self.context.struct_type(
                            &[self.context.i32_type().into(), 
                            self.context.i8_type().ptr_type(AddressSpace::default()).into()], 
                            false);
                    }
                    PrimitiveType::Unit => {
                        return self.context.struct_type(
                            &[], 
                            false);
                    }
                    _ => {
                        return self.context.struct_type(
                        &[self.context.i8_type().into()], 
                        false);
                    }
                }
            }
            _ => {
                return self.context.struct_type(
                    &[self.context.i8_type().into()], 
                    false);
            }
        }
    }

    
    pub fn literal_create_wrapper(&self, typedLiteral : & TypedLiteral) -> PointerValue<'a> {
        
        match typedLiteral {
            TypedLiteral::Bool(bool) => {
                let bool_struct = self.context.struct_type(
                    &[self.context.bool_type().into()], 
                    false
                );
                let ret = self.builder.build_alloca(
                    bool_struct, 
                    ""
                );
                let tmp_bool = self.context.bool_type().const_int(bool.to_owned() as u64, false);
                let tmp_bool_struct = bool_struct.const_named_struct(&[tmp_bool.into()]);
                self.builder.build_store(ret, tmp_bool_struct);
                return ret;
            }
            TypedLiteral::Char(char) => {
                let char_struct = self.context.struct_type(
                    &[self.context.i8_type().into()], 
                    false
                );
                let ret = self.builder.build_alloca(
                    char_struct, 
                    ""
                );
                let tmp_char = self.context.i8_type().const_int(char.to_owned() as u64, false);
                let tmp_char_struct = char_struct.const_named_struct(&[tmp_char.into()]);
                self.builder.build_store(ret, tmp_char_struct);
                return ret;
            }
            TypedLiteral::Float(float) => {
                let float_struct = self.context.struct_type(
                    &[self.context.f64_type().into()], 
                    false
                );
                let ret = self.builder.build_alloca(
                    float_struct, 
                    ""
                );
                let tmp_float = self.context.f64_type().const_float(float.to_owned());
                let tmp_float_struct = float_struct.const_named_struct(&[tmp_float.into()]);
                self.builder.build_store(ret, tmp_float_struct);
                return ret;
            }
            TypedLiteral::Int(int) => {
                let int_struct = self.context.struct_type(
                    &[self.context.i32_type().into()], 
                    false
                );
                let ret = self.builder.build_alloca(
                    int_struct, 
                    ""
                );
                let tmp_int = self.context.i32_type().const_int(int.to_owned() as u64, false);
                let tmp_int_struct = int_struct.const_named_struct(&[tmp_int.into()]);
                self.builder.build_store(ret, tmp_int_struct);
                return ret;
            }
            TypedLiteral::Str(str) => {
                let str_struct = self.context.struct_type(
                    &[self.context.i32_type().into(), 
                        self.context.i8_type().ptr_type(AddressSpace::default()).into()
                    ], 
                    false
                );
                let ret = self.builder.build_alloca(
                    str_struct, 
                    ""
                );
                

                let tmp_length = str.len();
                let tmp_length_value = self.context.i32_type().const_int(tmp_length.try_into().unwrap(), false);

                let payload_type = self.context.i8_type().ptr_type(AddressSpace::default());
                let payload = self.builder.build_array_malloc(self.context.i8_type(), tmp_length_value, "");

                
                match payload {
                    Ok(payload_ptr) => {
                        let tmp_str_struct = str_struct.const_named_struct(&[tmp_length_value.into(), payload_ptr.into()]);
                        self.builder.build_store(ret, tmp_str_struct);

                        let mut cur_addr = payload_ptr.clone().const_to_int(self.context.i64_type().into());
                        let next_offset = self.context.i64_type().const_int(1, false);
                        for char in str.chars() {
                            
                            
                            let tmp_char_value = self.context.i8_type().const_int(char.to_owned() as u64, false);
                            cur_addr = self.builder.build_int_add(cur_addr, next_offset, "");

                            self.builder.build_store(cur_addr.const_to_pointer(self.context.i8_type().ptr_type(AddressSpace::default())), tmp_char_value);
                        }
                    }
                    _ => {
                        let tmp_str_struct = str_struct.const_named_struct(&[tmp_length_value.into(), payload_type.const_zero().into()]);
                        self.builder.build_store(ret, tmp_str_struct);
                    }
                }

                
                return ret;
            }
            _ => {return unsafe {<inkwell::values::PointerValue as inkwell::values::IntMathValue>::new(std::ptr::null_mut())};}
        }
    }

    pub fn literal_unwrap_bool(&self, lit_type : & Type, lit_ptr :  PointerValue<'a>) -> Option<IntValue<'a>> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Bool => {
                        let bool_ptr = &lit_ptr.const_cast(self.context.bool_type().ptr_type(AddressSpace::default()).into());
                        let tmp_bool = self.builder.build_load(self.context.bool_type(), bool_ptr.to_owned(), "");
                        return Some(tmp_bool.into_int_value());
                    }
                    _ => {return None;}
                }
            }
            _ => {return None;}
        }
    }
    pub fn literal_unwrap_char(&self, lit_type : & Type, lit_ptr :  PointerValue<'a>) -> Option<IntValue<'a>> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Char => {
                        let char_ptr = &lit_ptr.const_cast(self.context.i8_type().ptr_type(AddressSpace::default()).into());
                        let tmp_char = self.builder.build_load(self.context.i8_type(), char_ptr.to_owned(), "");
                        return Some(tmp_char.into_int_value());
                    }
                    _ => {return None;}
                }
            }
            _ => {return None;}
        }
    }
    pub fn literal_unwrap_int(&self, lit_type : & Type, lit_ptr :  PointerValue<'a>) -> Option<IntValue<'a>> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Int => {
                        let int_ptr = &lit_ptr.const_cast(self.context.i32_type().ptr_type(AddressSpace::default()).into());
                        let tmp_int = self.builder.build_load(self.context.i32_type(), int_ptr.to_owned(), "");
                        return Some(tmp_int.into_int_value());
                    }
                    _ => {return None;}
                }
            }
            _ => {return None;}
        }
    }
    pub fn literal_unwrap_float(&self, lit_type : & Type, lit_ptr : PointerValue<'a>) -> Option<FloatValue<'a>> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Float => {
                        let float_ptr = &lit_ptr.const_cast(self.context.f64_type().ptr_type(AddressSpace::default()).into());
                        let tmp_float = self.builder.build_load(self.context.f64_type(), float_ptr.to_owned(), "");
                        return Some(tmp_float.into_float_value());
                    }
                    _ => {return None;}
                }
            }
            _ => {return None;}
        }
    }

    

    pub fn codegen_func(&self, typedFunc : & TypedFunc, mut func_name_table :  SymTable<String, FunctionValue<'a>>, putint_fn_value : FunctionValue, putch_fn_value : FunctionValue) 
    -> SymTable<String, FunctionValue<'a>>
    {
        // set up local symbol table for every level of block
        let return_ty = self.type2struct_type(typedFunc.return_ty.clone());
        let mut param_types :Vec<BasicMetadataTypeEnum> = Vec::new();
        for single_type in &typedFunc.params {
            param_types.push(inkwell::types::BasicMetadataTypeEnum::PointerType(
                    self.type2struct_type(single_type.1.clone()).ptr_type(AddressSpace::default())
                )
            );
        }

        let fn_type = return_ty.fn_type(&param_types, false);
        let fn_value = self.module.add_function(&typedFunc.name, fn_type, None);
        
        func_name_table.insert(typedFunc.name.clone(), fn_value.clone()).clone_into(&mut func_name_table);

        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        let mut local_sym_ptr_table : SymTable<String, inkwell::values::PointerValue> = SymTable::new();
        
        let mut param_counter : u32 = 0;
        for param in &typedFunc.params {
            local_sym_table.insert(
                param.0.to_string(),
                param.1.clone()
            ).clone_into(&mut local_sym_table);
            local_sym_ptr_table.insert(
                param.0.to_string(),
                fn_value.get_nth_param(param_counter).unwrap().into_pointer_value()
                // symbol table generation, ptr for all
            ).clone_into(&mut local_sym_ptr_table);
            param_counter = param_counter + 1;
        }
        let entry_name = format!("{:?}entry", (&typedFunc.name).to_string());
        let basic_block = self.context.append_basic_block(fn_value, &entry_name);
        self.builder.position_at_end(basic_block);
        
        let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
        func_name_table.clone_into(&mut tmp_table);

        let res = self.codegen_block(& typedFunc.body, 
                fn_value.clone(), 
                None,
                None,
                local_sym_table, 
                local_sym_ptr_table, 
                tmp_table,

            );

        if let Some(type_ptr) = res.TypePointer {
            let mut pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
            let mut pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
            let mut float_flag : i32 = 0;
            if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                float_flag = 1;
                pointee_type_int = self.context.struct_type(&[self.context.f64_type().into()], false);
            }
            else {
                if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                    pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                }
                else if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char){
                    pointee_type_int = self.context.struct_type(&[self.context.i8_type().into()], false);
                }
                else {
                    pointee_type_int = self.context.struct_type(&[self.context.bool_type().into()], false);
                }
            }
            let mut return_value;
            if float_flag == 1{
                return_value = self.builder.build_load(pointee_type_float, type_ptr.ptr.to_owned().const_cast(pointee_type_float.ptr_type(AddressSpace::default())), "").into_struct_value();
            }
            else {
                self.builder.build_call(putint_fn_value, &[self.literal_unwrap_int(&Type::Primitive(PrimitiveType::Int), type_ptr.ptr).unwrap().into()], "");
                // print result
                self.builder.build_call(putch_fn_value, &[self.context.i32_type().const_int(13, false).into()], "");
                self.builder.build_call(putch_fn_value, &[self.context.i32_type().const_int(10, false).into()], "");
                // print '\r\n'
                return_value = self.builder.build_load(pointee_type_int, type_ptr.ptr.to_owned().const_cast(pointee_type_int.ptr_type(AddressSpace::default())), "").into_struct_value();
            }
            

            self.builder.build_return(Some(&return_value));
        }
        else {
            self.builder.build_return(None);
        }
        
        let mut ret_table :SymTable<String, FunctionValue> = SymTable::new();
        func_name_table.clone_into(&mut ret_table);
        return ret_table;
        
    }

    pub fn codegen_block(&self, typedBlock : & TypedBlock, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut parent_sym_table :  SymTable<String, Type>, 
        mut parent_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>,
        func_name_table : SymTable<String, FunctionValue<'a>>) 
        -> TypedPointervalue_table<'a>{
        
        let mut local_sym_table : SymTable<String, Type> = parent_sym_table.clone();
        let mut local_sym_ptr_table : SymTable<String, inkwell::values::PointerValue> = parent_sym_ptr_table.clone();
        
        let mut tmp_res : TypedPointervalue_table = TypedPointervalue_table::new_None(parent_sym_table.clone(), parent_sym_ptr_table.clone());
        for expr in &typedBlock.exprs {
            let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
            func_name_table.clone_into(&mut tmp_table);
            tmp_res = self.codegen_expr(expr,
                fn_value.clone(),
                block_to_break.clone(), block_to_con.clone(),
                local_sym_table, 
                local_sym_ptr_table,
                tmp_table,
            );
            local_sym_table = tmp_res.sym_table.clone();
            local_sym_ptr_table = tmp_res.sym_ptr_table.clone();
        }
        let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
        func_name_table.clone_into(&mut tmp_table);

        return tmp_res;
        

    }

    pub fn codegen_if(&self, typedIf : & TypedIf, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>, 
        func_name_table : SymTable<String, FunctionValue<'a>>) -> TypedPointervalue_table<'a> {
        
        let cond_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.build_unconditional_branch(cond_basic_block);
        self.builder.position_at_end(cond_basic_block);

        let then_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.position_at_end(then_basic_block);
        self.codegen_block(&typedIf.then, 
            fn_value.clone(), 
            block_to_break.clone(),
            block_to_con.clone(),
            block_sym_table.clone(),
            block_sym_ptr_table.clone(), 
            func_name_table.clone()
            );
        
        
        let maybe_else_basic_block = self.context.append_basic_block(fn_value.clone(), "");

        self.builder.build_unconditional_branch(maybe_else_basic_block);

        
        
        self.builder.position_at_end(cond_basic_block);
        let _compare_res = self.codegen_expr(typedIf.cond.as_ref(), fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
            block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
        
        if let Some(compare_res) = _compare_res.TypePointer {
            let _real_res = self.literal_unwrap_int(&Type::Primitive(PrimitiveType::Int), compare_res.ptr);
            if let Some(real_res) = _real_res {
                self.builder.build_conditional_branch(real_res, then_basic_block, maybe_else_basic_block);
            }
        }

        self.builder.position_at_end(maybe_else_basic_block);
        self.codegen_else(&typedIf.els, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
            block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());

        

        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());

    }

    pub fn codegen_else(&self, typedElse : & TypedElse, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>, 
        func_name_table : SymTable<String, FunctionValue<'a>>) -> TypedPointervalue_table<'a> {

        match &typedElse {
            TypedElse::Else(else_only) => {
                self.codegen_block(else_only, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
            }
            TypedElse::ElseIf(else_if) => {
                self.codegen_if(else_if.as_ref(), fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
            }
            TypedElse::None => {
                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
            }
            _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
        }
        
    }

    pub fn codegen_while(&self, typedWhile : & TypedWhile, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>, 
        func_name_table : SymTable<String, FunctionValue<'a>>) -> TypedPointervalue_table<'a>{
            
        let cond_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.build_unconditional_branch(cond_basic_block);
        self.builder.position_at_end(cond_basic_block);

        let then_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.position_at_end(then_basic_block);
        
        
        let while_block_to_con = cond_basic_block.clone();
        let while_block_to_break = cond_basic_block.clone();    // break has not been implemented yet for some reason

        self.codegen_block(&typedWhile.body, fn_value.clone(), Some(while_block_to_break), Some(while_block_to_con),
            block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
        self.builder.build_unconditional_branch(cond_basic_block);

        
        let end_while_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.position_at_end(cond_basic_block.clone());

        let _compare_res = self.codegen_expr(&typedWhile.cond, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
            block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
        if let Some(compare_res) = _compare_res.TypePointer {
            let _real_res = self.literal_unwrap_bool(&Type::Primitive(PrimitiveType::Bool), compare_res.ptr);
            if let Some(real_res) = _real_res {
                self.builder.build_conditional_branch(real_res, then_basic_block, end_while_basic_block);
            }
        }

        self.builder.position_at_end(end_while_basic_block);
        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }

    pub fn codegen_for(&self, typedFor : & TypedFor, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>, 
        func_name_table : SymTable<String, FunctionValue<'a>>) -> TypedPointervalue_table<'a> {

        // let expr_let : TypedLet = TypedLet { name: (typedFor.var.clone()), 
        //                                     ty: (Type::Primitive(PrimitiveType::Int)), 
        //                                     rhs: (typedFor.start) };
        // // let expr_var : TypedVariable = TypedVariable { name: (typedFor.var.clone()), 
        // //                                     ty: (Type::Primitive(PrimitiveType::Int)) };

        // let _let_ret = self.codegen_let(&expr_let, fn_value.clone(), block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());

        let var_name = &typedFor.var.clone();
        if block_sym_table.get(var_name) == None {
            block_sym_table.insert(
                typedFor.var.clone(),
                Type::Primitive(PrimitiveType::Int)
            ).clone_into(&mut block_sym_table);
            let typeLet_ty_copy = &Type::Primitive(PrimitiveType::Int);
            let var_ptr_type = self.type2struct_type(typeLet_ty_copy.to_owned());
            let var_ptr = self.builder.build_alloca(var_ptr_type, "");
            block_sym_ptr_table.insert(
                typedFor.var.clone(),
                var_ptr,
                // symbol table generation, ptr for all
            ).clone_into(&mut block_sym_ptr_table);
        }
        let mut _var_ptr = block_sym_ptr_table.get(var_name);


        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        block_sym_table.clone_into(&mut local_sym_table);
        let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

        let _res_start = self.codegen_expr(typedFor.start.as_ref(), fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
            local_sym_table.clone(), local_sym_ptr_table.clone(), func_name_table.clone());
        self.builder.build_store(_var_ptr.unwrap().to_owned(), 
                                self.literal_unwrap_int(&Type::Primitive(PrimitiveType::Int), 
                                    _res_start.TypePointer.unwrap().ptr).unwrap());
        // "for i in 0...10", let i = 0 here
        
        let mut tmp_local_sym_table = block_sym_table.clone();
        let mut tmp_local_sym_ptr_table = block_sym_ptr_table.clone();




        let for_cond_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.build_unconditional_branch(for_cond_basic_block);

        let for_body_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        
        self.builder.position_at_end(for_cond_basic_block.clone());

        self.builder.position_at_end(for_body_basic_block.clone());

        
        let for_block_to_break = for_cond_basic_block.clone();  // break has not been implemented yet
        let for_block_to_con =for_cond_basic_block.clone();

        self.codegen_block(&typedFor.body, fn_value.clone(), Some(for_block_to_break), Some(for_block_to_con),
            tmp_local_sym_table.clone(), tmp_local_sym_ptr_table.clone(), func_name_table.clone());
        
        self.builder.build_unconditional_branch(for_cond_basic_block.clone());
        let for_end_basic_block = self.context.append_basic_block(fn_value.clone(), "");
        self.builder.position_at_end(for_cond_basic_block);
        
        if let Some(var_ptr) = tmp_local_sym_ptr_table.get(&typedFor.var) {
            // let binop_gt : TypedBinOp = TypedBinOp { op: (BinOp::Gt), lhs: (Box::new(TypedExpr::Variable(TypedVariable { name: (typedFor.var.clone()), ty: (Type::Primitive(PrimitiveType::Int)) }))), rhs: (Box::new(*typedFor.end)), ty: (Type::Primitive(PrimitiveType::Bool)) };
            // let binop_res = self.codegen_binOP(&binop_gt, fn_value.clone(), block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
            let _res_end = self.codegen_expr(typedFor.end.as_ref(), fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                local_sym_table.clone(), local_sym_ptr_table.clone(), func_name_table.clone());
            let binop_res = 
                self.builder.build_int_compare(IntPredicate::SLT, 
                                                self.literal_unwrap_int(&Type::Primitive(PrimitiveType::Int), 
                                                    var_ptr.to_owned().clone()).unwrap(), 
                                                self.literal_unwrap_int(&Type::Primitive(PrimitiveType::Int), 
                                                    _res_end.TypePointer.unwrap().ptr).unwrap(), 
                                                "");
            // "for i in 0...10" binop_res = ?(i > 10)

            let binop_add : TypedBinOp = TypedBinOp { 
                op: (BinOp::Add), 
                lhs: (Box::new(TypedExpr::Variable(TypedVariable { name: (typedFor.var.clone()), ty: (Type::Primitive(PrimitiveType::Int)) }))), 
                rhs: (Box::new(TypedExpr::Literal(TypedLiteral::Int(1)))), 
                ty: (Type::Primitive(PrimitiveType::Int)) 
            };
            let next_var = self.codegen_binOP(&binop_add, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                tmp_local_sym_table.clone(), tmp_local_sym_ptr_table.clone(), func_name_table.clone());
            self.builder.build_store(*block_sym_ptr_table.get(var_name).unwrap(), self.literal_unwrap_int(&Type::Primitive(PrimitiveType::Int), next_var.TypePointer.unwrap().ptr).unwrap());

            self.builder.build_conditional_branch(
                binop_res, 
                for_body_basic_block.clone(), 
                for_end_basic_block.clone()
            );

            
        }
        
        
        self.builder.position_at_end(for_end_basic_block.clone());

        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());

        
    }

    pub fn codegen_expr(&self, typedExpr : & TypedExpr, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>, 
        func_name_table : SymTable<String, FunctionValue<'a>>) -> TypedPointervalue_table<'a> {
        
        match typedExpr {
            // TypedExpr::Array() => {

            // }
            TypedExpr::Assign(typeAssign) => {
                let tmp_res = self.codegen_assign(typeAssign, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                    block_sym_table, block_sym_ptr_table, func_name_table);
                block_sym_table = tmp_res.sym_table.clone();
                block_sym_ptr_table = tmp_res.sym_ptr_table.clone();

                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);    
            }
            TypedExpr::BinOp(typeBinop) => {
                let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);

                let mut tmp_sym_table : SymTable<String, Type> = SymTable::new();
                block_sym_table.clone_into(&mut tmp_sym_table);
                let mut tmp_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
                block_sym_ptr_table.clone_into(&mut tmp_sym_ptr_table);

                let mut ret =  self.codegen_binOP(&typeBinop, 
                    fn_value.clone(),
                    block_to_break.clone(),
                    block_to_con.clone(),
                    tmp_sym_table, 
                    tmp_sym_ptr_table,
                    tmp_table);

                ret.sym_table = block_sym_table.clone();
                ret.sym_ptr_table = block_sym_ptr_table.clone();
                return ret;
            }
            TypedExpr::Block(typedBlock) => {
                return self.codegen_block(&typedBlock, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());

            }
            // TypedExpr::Break => {
            //     if let Some(break_to_jump) = block_to_break {
            //         self.builder.build_unconditional_branch(break_to_jump);
            //     }
            //     return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            // }
            TypedExpr::Call(typedCall) => {
                let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);

                let mut tmp_sym_table : SymTable<String, Type> = SymTable::new();
                block_sym_table.clone_into(&mut tmp_sym_table);
                let mut tmp_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
                block_sym_ptr_table.clone_into(&mut tmp_sym_ptr_table);

                let mut ret =  self.codegen_call(&typedCall, 
                                                                      fn_value.clone(),
                                                                      block_to_break.clone(), block_to_con.clone(),
                                                                      tmp_sym_table, 
                                                                      tmp_sym_ptr_table,
                                                                      tmp_table);

                ret.sym_table = block_sym_table.clone();
                ret.sym_ptr_table = block_sym_ptr_table.clone();
                return ret;
            }
            // TypedExpr::Closure() => {
                
            // }
            TypedExpr::Continue => {
                if let Some(con_to_jump) = block_to_con {
                    self.builder.build_unconditional_branch(con_to_jump);
                }
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            // TypedExpr::Ctor() => {
                
            // }
            TypedExpr::For(typedFor) => {
                self.codegen_for(&typedFor, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                    block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            TypedExpr::If(typedIf) => {
                self.codegen_if(&typedIf, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                    block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            // TypedExpr::Index() => {
                
            // }
            TypedExpr::Let(typelet) => {
                let tmp_res = self.codegen_let(&typelet, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                     block_sym_table, block_sym_ptr_table, func_name_table);
                block_sym_table = tmp_res.sym_table.clone();
                block_sym_ptr_table = tmp_res.sym_ptr_table.clone();
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            TypedExpr::Literal(literal) => {
                return TypedPointervalue_table::new(
                    literal.ty(), 
                    self.literal_create_wrapper(literal), 
                    block_sym_table, 
                    block_sym_ptr_table
                );

            }
            // TypedExpr::Match() => {

            // }
            TypedExpr::Return(typeReturn) => {
                let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);

                let mut tmp_sym_table : SymTable<String, Type> = SymTable::new();
                block_sym_table.clone_into(&mut tmp_sym_table);
                let mut tmp_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
                block_sym_ptr_table.clone_into(&mut tmp_sym_ptr_table);

                self.codegen_return(&typeReturn, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                    tmp_sym_table, tmp_sym_ptr_table, tmp_table);
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            // TypedExpr::Tuple() => {
                
            // }
            TypedExpr::UnOp(typedUnOp) => {
                let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);

                let mut tmp_sym_table : SymTable<String, Type> = SymTable::new();
                block_sym_table.clone_into(&mut tmp_sym_table);
                let mut tmp_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
                block_sym_ptr_table.clone_into(&mut tmp_sym_ptr_table);

                let mut ret =  self.codegen_unOp(&typedUnOp, 
                                                                      fn_value.clone(),
                                                                      block_to_break.clone(), block_to_con.clone(),
                                                                      tmp_sym_table, 
                                                                      tmp_sym_ptr_table,
                                                                      tmp_table);

                ret.sym_table = block_sym_table.clone();
                ret.sym_ptr_table = block_sym_ptr_table.clone();
                return ret;
            }
            TypedExpr::Variable(var) => {
                if block_sym_table.get(&var.name) == None {
                    block_sym_table.insert(
                        var.name.clone(),
                        var.ty.clone()
                    ).clone_into(&mut block_sym_table);
                    let var_ptr_type = self.type2struct_type(var.ty.clone());
                    let var_ptr = self.builder.build_alloca(var_ptr_type, "");
                    block_sym_ptr_table.insert(
                        var.name.clone(),
                        var_ptr.clone(),
                        // symbol table generation, ptr for all
                    ).clone_into(&mut block_sym_ptr_table);
                }
                if let Some(var_ptr) = block_sym_ptr_table.get(&var.name) {
                    return TypedPointervalue_table::new(
                        var.ty.clone(),
                        var_ptr.to_owned(), 
                        block_sym_table, 
                        block_sym_ptr_table
                    );
                }
                else {
                    return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
                }
                
            }
            TypedExpr::While(typedWhile) => {
                self.codegen_while(&typedWhile, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                    block_sym_table.clone(), block_sym_ptr_table.clone(), func_name_table.clone());
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            _ => { return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table); }
        }
    }
    
    pub fn codegen_binOP(&self, typedBinOp : & TypedBinOp, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table : SymTable<String, Type>, 
        mut block_sym_ptr_table : SymTable<String, inkwell::values::PointerValue<'a>>,
        func_name_table : SymTable<String, FunctionValue<'a>>) 
        -> TypedPointervalue_table<'a>{
        
            let mut tmp_func_table : SymTable<String, FunctionValue> = SymTable::new();
            func_name_table.clone_into(&mut tmp_func_table);
            let mut local_sym_table : SymTable<String, Type> = SymTable::new();
            block_sym_table.clone_into(&mut local_sym_table);
            let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
            block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

            let mut tmp_func_table_2 : SymTable<String, FunctionValue> = SymTable::new();
            func_name_table.clone_into(&mut tmp_func_table_2);
            let mut local_sym_table_2 : SymTable<String, Type> = SymTable::new();
            block_sym_table.clone_into(&mut local_sym_table_2);
            let mut local_sym_ptr_table_2 : SymTable<String, PointerValue> = SymTable::new();
            block_sym_ptr_table.clone_into(&mut local_sym_ptr_table_2);

            let _rhs_res = self.codegen_expr(&typedBinOp.rhs, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                local_sym_table, local_sym_ptr_table, tmp_func_table);
            let _lhs_res = self.codegen_expr(&typedBinOp.lhs, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                local_sym_table_2, local_sym_ptr_table_2, tmp_func_table_2);
            let lhs_res : &TypedPointerValue;
            let rhs_res : &TypedPointerValue;

            if let (Some(lhs_res), Some(rhs_res)) = (_lhs_res.TypePointer, _rhs_res.TypePointer) {
                // let mut rhs_value = self.literal_unwrap(&typedBinOp.ty, &rhs_res.ptr);
                // let mut lhs_value = self.literal_unwrap(&typedBinOp.ty, &lhs_res.ptr);

                // let mut tmp_result : BasicValue;

                let bool_struct = self.context.bool_type();
                let int_struct = self.context.i32_type();
                let float_struct = self.context.f64_type();
                let char_struct = self.context.i8_type();

                let mut cal_type = lhs_res.pointer_type.clone();
                if rhs_res.pointer_type == Type::Primitive(PrimitiveType::Float) {
                    cal_type = Type::Primitive(PrimitiveType::Float);
                }


                match cal_type {
                    Type::Primitive(PrimitiveType::Int) => {
                        let _rhs_value = self.literal_unwrap_int(&cal_type, rhs_res.ptr);
                        let _lhs_value = self.literal_unwrap_int(&cal_type, lhs_res.ptr);

                        if let (Some(rhs_value), Some(lhs_value)) = (_rhs_value, _lhs_value) {
                            match &typedBinOp.op {
                                BinOp::Add => {
                                    let tmp_result = self.builder.build_int_add(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Sub => {
                                    let tmp_result = self.builder.build_int_sub(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Mul => {
                                    let tmp_result = self.builder.build_int_mul(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Div => {
                                    let tmp_result = self.builder.build_int_signed_div(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Mod => {
                                    let tmp_result = self.builder.build_int_signed_rem(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Eq => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Neq => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Lt => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::SLT, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Gt => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::SGT, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Lte => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::SLE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Gte => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::SGE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::And => {
                                    let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Or => {
                                    let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Int), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                            }
                        }
                        else {
                            return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                        }
                    }
                    Type::Primitive(PrimitiveType::Float) => {
                        let _rhs_value = self.literal_unwrap_float(&cal_type, rhs_res.ptr);
                        let _lhs_value = self.literal_unwrap_float(&cal_type, lhs_res.ptr);

                        if let (Some(rhs_value), Some(lhs_value)) = (_rhs_value, _lhs_value){
                            match &typedBinOp.op {
                                BinOp::Add => {
                                    let tmp_result = self.builder.build_float_add(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Float), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Sub => {
                                    let tmp_result = self.builder.build_float_sub(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Float), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Mul => {
                                    let tmp_result = self.builder.build_float_mul(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Float), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Div => {
                                    let tmp_result = self.builder.build_float_div(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Float), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Mod => {
                                    let tmp_result = self.builder.build_float_rem(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Float), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Eq => {
                                    let tmp_result = self.builder.build_float_compare(FloatPredicate::OEQ, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Neq => {
                                    let tmp_result = self.builder.build_float_compare(FloatPredicate::ONE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Lt => {
                                    let tmp_result = self.builder.build_float_compare(FloatPredicate::OLT, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Gt => {
                                    let tmp_result = self.builder.build_float_compare(FloatPredicate::OGT, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Lte => {
                                    let tmp_result = self.builder.build_float_compare(FloatPredicate::OLE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Gte => {
                                    let tmp_result = self.builder.build_float_compare(FloatPredicate::OGE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                            }
                            
                        }
                        else {
                            return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                        }
                        
                    }
                    Type::Primitive(PrimitiveType::Char) => {
                        let _rhs_value = self.literal_unwrap_char(&cal_type, rhs_res.ptr);
                        let _lhs_value = self.literal_unwrap_char(&cal_type, lhs_res.ptr);

                        if let (Some(rhs_value), Some(lhs_value)) = (_rhs_value, _lhs_value){
                            match &typedBinOp.op {
                                BinOp::Add => {
                                    let tmp_result = self.builder.build_int_add(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Sub => {
                                    let tmp_result = self.builder.build_int_sub(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Mul => {
                                    let tmp_result = self.builder.build_int_mul(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Div => {
                                    let tmp_result = self.builder.build_int_signed_div(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Mod => {
                                    let tmp_result = self.builder.build_int_signed_rem(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Eq => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Neq => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Lt => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::ULT, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Gt => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::UGT, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Lte => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::ULE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Gte => {
                                    let tmp_result = self.builder.build_int_compare(IntPredicate::UGE, lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::And => {
                                    let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Or => {
                                    let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Char), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                            }
                        }
                        else {
                            return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                        }
                    }
                    Type::Primitive(PrimitiveType::Bool) => {
                        let _rhs_value = self.literal_unwrap_bool(&typedBinOp.ty, rhs_res.ptr);
                        let _lhs_value = self.literal_unwrap_bool(&typedBinOp.ty, lhs_res.ptr);

                        if let (Some(rhs_value), Some(lhs_value)) = (_rhs_value, _lhs_value){
                            match &typedBinOp.op {
                                BinOp::And => {
                                    let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                BinOp::Or => {
                                    let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
                                    let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                    self.builder.build_store(tmp_result_ptr, tmp_result);
                                    return TypedPointervalue_table::new(
                                        Type::Primitive(PrimitiveType::Bool), 
                                        tmp_result_ptr, 
                                        block_sym_table, 
                                        block_sym_ptr_table
                                    );
                                }
                                _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                            }
                        }
                        else {
                            return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                        }
                        
                    }
                    _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                }
            }
            else {
                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
            }

    }

    pub fn codegen_unOp(&self, typedUnOp : & TypedUnOp, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table : SymTable<String, Type>, 
        mut block_sym_ptr_table : SymTable<String, inkwell::values::PointerValue<'a>>,
        func_name_table : SymTable<String, FunctionValue<'a>>) 
        -> TypedPointervalue_table<'a> {
        
        let mut tmp_func_table : SymTable<String, FunctionValue> = SymTable::new();
        func_name_table.clone_into(&mut tmp_func_table);
        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        block_sym_table.clone_into(&mut local_sym_table);
        let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

        let _rhs_res = self.codegen_expr(
            &typedUnOp.rhs, 
            fn_value.clone(),
            block_to_break.clone(), block_to_con.clone(),
            local_sym_table, 
            local_sym_ptr_table,
            tmp_func_table
        );
            
        if let Some(rhs_res) = _rhs_res.TypePointer {
            match typedUnOp.op {
                UnOp::Neg => {
                    
                    match &typedUnOp.ty {
                        Type::Primitive(PrimitiveType::Int) => {
                            let _tmp_value = self.literal_unwrap_int(&typedUnOp.ty, rhs_res.ptr);
                            if let Some(tmp_value) = _tmp_value { 
                                let zero_value = self.context.i32_type().const_zero();
                                let tmp_result = self.builder.build_int_sub(zero_value, tmp_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(
                                    self.context.i32_type(), 
                                    "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return TypedPointervalue_table::new(
                                    typedUnOp.ty.clone(), 
                                    tmp_result_ptr, 
                                    block_sym_table, 
                                    block_sym_ptr_table
                                );
                            }
                            else {
                                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                            }
                        }
                        Type::Primitive(PrimitiveType::Float) => {
                            let _tmp_value = self.literal_unwrap_float(&typedUnOp.ty, rhs_res.ptr);
                            if let Some(tmp_value) = _tmp_value {
                                let zero_value = self.context.f64_type().const_float(0.0);
                                let tmp_result = self.builder.build_float_sub(zero_value, tmp_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(
                                    self.context.f64_type(), 
                                    "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return TypedPointervalue_table::new(
                                    typedUnOp.ty.clone(), 
                                    tmp_result_ptr, 
                                    block_sym_table, 
                                    block_sym_ptr_table
                                );
                            }
                            else {
                                return return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                            }
                        }
                        _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                    }
                    
                }
                UnOp::Not => {
                    match &typedUnOp.ty {
                        Type::Primitive(PrimitiveType::Bool) => {
                            let _tmp_value = self.literal_unwrap_bool(&typedUnOp.ty, rhs_res.ptr);
                            if let  Some(tmp_value) = _tmp_value {
                           
                                let tmp_result = self.builder.build_not(tmp_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(
                                    self.context.bool_type(), 
                                    "",
                                );
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return TypedPointervalue_table::new(
                                    typedUnOp.ty.clone(), 
                                    tmp_result_ptr, 
                                    block_sym_table, 
                                    block_sym_ptr_table
                                );
                            }
                            else {
                                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                            }
                        }
                        _ =>{return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
                    }
                }
                _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
            }
        }
        else {
            return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
        }
        
    }
    pub fn codegen_call(&self, typedCall : & TypedCall, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>,
        func_name_table : SymTable<String, FunctionValue<'a>>) 
        -> TypedPointervalue_table<'a>{

        // println!("{:?}", typedCall.ty);
        match typedCall.func.as_ref().clone() {
            TypedExpr::Variable(variable) => {
                let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);
                // println!("\n{:?}\nNo!!\n", fn_value);

                if let Some(_fn_value) = tmp_table.get(&variable.name){
                    let fn_value = _fn_value.to_owned().clone();
                    let mut params :Vec<BasicMetadataValueEnum> = Vec::new();

                    // println!("\n{:?}\nYes!!\n", fn_value);
        
                    for arg in &typedCall.args {
                        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
                        block_sym_table.clone_into(&mut local_sym_table);
                        let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
                        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);
                        let mut tmp_table : SymTable<String, FunctionValue> = SymTable::new();
                        func_name_table.clone_into(&mut tmp_table);
                        let tmp_arg =  self.codegen_expr(&arg, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                            local_sym_table, local_sym_ptr_table, tmp_table);

                        if let Some(type_ptr) = tmp_arg.TypePointer {
                            params.push(BasicMetadataValueEnum::PointerValue(type_ptr.ptr.to_owned()));
                        }
                    }

                    let call_result = &self.builder.build_call(fn_value, &params, "");
                    let real_result = call_result.try_as_basic_value();
                    if real_result.is_right(){
                        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                    }
                    else  {
                        let real_real_result = real_result.left().unwrap();

                        let result_pointer_type = self.type2struct_type(typedCall.ty.clone());
                        let real_real_result_in_struct = real_real_result.into_struct_value();

                        let real_real_result_ptr = self.builder.build_alloca(result_pointer_type, "");
                        self.builder.build_store(real_real_result_ptr, real_real_result_in_struct);
                        
                        return TypedPointervalue_table::new(
                            typedCall.ty.clone(), 
                            real_real_result_ptr, 
                            block_sym_table, 
                            block_sym_ptr_table
                        );
                    }
                }
                else {
                    return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
                }
            }
            _ => {return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());}
        }
        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }
    pub fn codegen_return(&self, typeReturn : & TypedReturn, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table : SymTable<String, Type>, 
        mut block_sym_ptr_table : SymTable<String, inkwell::values::PointerValue<'a>>, 
        func_name_table : SymTable<String, FunctionValue<'a>>) 
        -> TypedPointervalue_table<'a>{
        
        if let Some(expr) = &typeReturn.expr.as_ref() {
            let mut local_sym_table : SymTable<String, Type> = SymTable::new();
            block_sym_table.clone_into(&mut local_sym_table);
            let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
            block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

            let _type_ptr = self.codegen_expr(&expr, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
                local_sym_table, local_sym_ptr_table, func_name_table);
            if let Some(type_ptr) = _type_ptr.TypePointer {
                let mut pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                    let mut pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
                let mut float_flag : i32 = 0;
                if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                    float_flag = 1;
                    pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
                }
                else {
                    if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                        pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                    }
                    else if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char){
                        pointee_type_int = self.context.struct_type(&[self.context.i8_type().into()], false);
                    }
                    else {
                        pointee_type_int = self.context.struct_type(&[self.context.bool_type().into()], false);
                    }
                }
                let mut return_value;
                if float_flag == 1{
                    return_value = self.builder.build_load(pointee_type_float, type_ptr.ptr.to_owned().const_cast(pointee_type_float.ptr_type(AddressSpace::default())), "").into_struct_value();
                }
                else {
                    return_value = self.builder.build_load(pointee_type_int, type_ptr.ptr.to_owned().const_cast(pointee_type_int.ptr_type(AddressSpace::default())), "").into_struct_value();
                }
                
                self.builder.build_return(Some(&return_value));
            }
            else {
                self.builder.build_return(None);
            }
        }
        else {
            self.builder.build_return(None);
        }
        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());

    }
    pub fn codegen_let(&self, typeLet : & TypedLet, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>,
        func_name_table : SymTable<String, FunctionValue<'a>>
    ) 
        -> TypedPointervalue_table<'a>{
        
        let var_name = &typeLet.name.clone();
        if block_sym_table.get(var_name) == None {
            block_sym_table.insert(
                typeLet.name.clone(),
                typeLet.ty.clone()
            ).clone_into(&mut block_sym_table);
            let typeLet_ty_copy = &typeLet.ty;
            let var_ptr_type = self.type2struct_type(typeLet_ty_copy.to_owned());
            let var_ptr = self.builder.build_alloca(var_ptr_type, "");
            block_sym_ptr_table.insert(
                typeLet.name.clone(),
                var_ptr,
                // symbol table generation, ptr for all
            ).clone_into(&mut block_sym_ptr_table);
        }

        let mut _var_type = block_sym_table.get(var_name);

        let mut _var_ptr = block_sym_ptr_table.get(var_name);


        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        block_sym_table.clone_into(&mut local_sym_table);
        let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

        let _rhs = self.codegen_expr(&typeLet.rhs, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
            local_sym_table, local_sym_ptr_table, func_name_table);

        if let Some(var_type) = _var_type {
            if let Some(var_ptr) = _var_ptr {
                if let Some(rhs_result) = _rhs.TypePointer {
                    let mut pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                    let mut pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
                    let mut float_flag : i32 = 0;
                    if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                        float_flag = 1;
                        pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
                    }
                    else {
                        if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                            pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                        }
                        else if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char ){
                            pointee_type_int = self.context.struct_type(&[self.context.i8_type().into()], false);
                        }
                        else {
                            pointee_type_int = self.context.struct_type(&[self.context.bool_type().into()], false);
                        }
                    }
                    let mut return_value;
                    if float_flag == 1{
                        return_value = self.builder.build_load(pointee_type_float, rhs_result.ptr.to_owned().const_cast(pointee_type_float.ptr_type(AddressSpace::default())), "").into_struct_value();
                    }
                    else {
                        return_value = self.builder.build_load(pointee_type_int, rhs_result.ptr.to_owned().const_cast(pointee_type_int.ptr_type(AddressSpace::default())), "").into_struct_value();
                    }
                    
                
                    self.builder.build_store(var_ptr.to_owned(), return_value);
                }
            }
        }
        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }

    pub fn codegen_assign(&self, typeAssign : & TypedAssign, fn_value : FunctionValue, block_to_break : Option<BasicBlock>, block_to_con : Option<BasicBlock>,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'a>>,
        func_name_table : SymTable<String, FunctionValue<'a>>
    ) 
        -> TypedPointervalue_table<'a>{
        
        let var_name = &typeAssign.name;
        let _var_ptr = block_sym_ptr_table.get(var_name);

        
        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        block_sym_table.clone_into(&mut local_sym_table);
        let mut local_sym_ptr_table : SymTable<String, PointerValue> = SymTable::new();
        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

        let _rhs = self.codegen_expr(&typeAssign.rhs, fn_value.clone(), block_to_break.clone(), block_to_con.clone(),
            local_sym_table, local_sym_ptr_table, func_name_table);


        if let Some(var_ptr) = _var_ptr {
            if let Some(rhs_result) = _rhs.TypePointer {
                
                let mut pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                let mut pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
                let mut float_flag : i32 = 0;
                if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                    float_flag = 1;
                    pointee_type_float = self.context.struct_type(&[self.context.f64_type().into()], false);
                }
                else {
                    if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                        pointee_type_int = self.context.struct_type(&[self.context.i32_type().into()], false);
                    }
                    else if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char ){
                        pointee_type_int = self.context.struct_type(&[self.context.i8_type().into()], false);
                    }
                    else {
                        pointee_type_int = self.context.struct_type(&[self.context.bool_type().into()], false);
                    }
                }
                let mut return_value;
                if float_flag == 1{
                    return_value = self.builder.build_load(pointee_type_float, rhs_result.ptr.to_owned().const_cast(pointee_type_float.ptr_type(AddressSpace::default())), "").into_struct_value();
                }
                else {
                    return_value = self.builder.build_load(pointee_type_int, rhs_result.ptr.to_owned().const_cast(pointee_type_int.ptr_type(AddressSpace::default())), "").into_struct_value();
                }
                
                self.builder.build_store(var_ptr.to_owned(), return_value);
            }
        }

        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }
    
    

}






