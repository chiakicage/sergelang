pub use crate::midend::typed_ast::*;
use inkwell::values::*;
use inkwell::types::BasicMetadataTypeEnum;
pub use crate::ast::*;
pub use crate::utils::types::*;
pub use inkwell::IntPredicate;
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

pub struct CodeGen<'ctx, 'a> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
}

pub struct TypedPointervalue<'ctx> {
    pub pointer_type : Type,
    pub ptr : inkwell::values::PointerValue<'ctx>,
}

impl <'ctx, 'a>TypedPointervalue<'ctx> {
    pub fn new(
        pointer_type : Type,
        ptr :  inkwell::values::PointerValue<'ctx>,
    ) -> Self {
        Self {
            pointer_type, 
            ptr,
        }
    }
}

pub struct TypedPointervalue_table<'ctx> {
    pub TypePointer : Option<TypedPointervalue<'ctx>>,
    pub sym_table : SymTable<String, Type>,
    pub sym_ptr_table : SymTable<String, PointerValue<'ctx>>,
}

impl <'ctx, 'a>TypedPointervalue_table<'ctx> {
    pub fn new(
        pointer_type : Type,
        ptr :  inkwell::values::PointerValue<'ctx>,
        sym_table : SymTable<String, Type>,
        sym_ptr_table : SymTable<String, PointerValue<'ctx>>,
    ) -> Self {
        Self {
            TypePointer : Some(TypedPointervalue::new(pointer_type, ptr)),
            sym_table,
            sym_ptr_table,
        }
    }
    pub fn new_None(
        sym_table : SymTable<String, Type>,
        sym_ptr_table : SymTable<String, PointerValue<'ctx>>,
    )-> Self {
        Self {
            TypePointer : None,
            sym_table,
            sym_ptr_table,
        }
    }
}

impl<'ctx, 'a : 'ctx> CodeGen<'ctx, 'a> {
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


    pub fn codegen_module(&'ctx self, typedast : &'ctx TypedModule) {

        let mut func_name_table : SymTable<String, FunctionValue<'ctx>> = SymTable::new();
        let mut new_table : SymTable<String, FunctionValue<'ctx>> = SymTable::new();
        for func in &typedast.func_defs {
            let mut tmp_table : SymTable<String, FunctionValue<'ctx>> = SymTable::new();
            func_name_table.clone_into(&mut tmp_table);
            new_table = self.codegen_func(& func, tmp_table);
            new_table.clone_into(&mut func_name_table);
        }

    }

    pub fn type2struct_type<'c>(&'c self, ser_type : &'c Type) -> inkwell::types::StructType<'c>{
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

    
    pub fn literal_create_wrapper(&self, typedLiteral : &TypedLiteral) -> PointerValue {
        
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
            _ => {return unsafe {<inkwell::values::PointerValue<'_> as inkwell::values::IntMathValue>::new(std::ptr::null_mut())};}
        }
    }

    pub fn literal_unwrap_bool<'c>(&'c self, lit_type : &'c Type, lit_ptr : &'c PointerValue) -> IntValue<'c> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Bool => {
                        let bool_ptr = &lit_ptr.const_cast(self.context.bool_type().ptr_type(AddressSpace::default()).into());
                        let tmp_bool = self.builder.build_load(self.context.bool_type(), bool_ptr.to_owned(), "");
                        return tmp_bool.into_int_value();
                    }
                    _ => {return self.context.bool_type().const_zero();}
                }
            }
            _ => {return self.context.bool_type().const_zero();}
        }
    }
    pub fn literal_unwrap_char<'c>(&'c self, lit_type : &'c Type, lit_ptr : &'c PointerValue) -> IntValue<'c> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Char => {
                        let char_ptr = &lit_ptr.const_cast(self.context.i8_type().ptr_type(AddressSpace::default()).into());
                        let tmp_char = self.builder.build_load(self.context.i8_type(), char_ptr.to_owned(), "");
                        return tmp_char.into_int_value();
                    }
                    _ => {return self.context.i8_type().const_zero();}
                }
            }
            _ => {return self.context.i8_type().const_zero();}
        }
    }
    pub fn literal_unwrap_int<'c>(&'c self, lit_type : &'c Type, lit_ptr : &'c PointerValue) -> IntValue<'c> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Int => {
                        let int_ptr = &lit_ptr.const_cast(self.context.i32_type().ptr_type(AddressSpace::default()).into());
                        let tmp_int = self.builder.build_load(self.context.i32_type(), int_ptr.to_owned(), "");
                        return tmp_int.into_int_value();
                    }
                    _ => {return self.context.i32_type().const_zero();}
                }
            }
            _ => {return self.context.i32_type().const_zero();}
        }
    }
    pub fn literal_unwrap_float<'c>(&'c self, lit_type : &'c Type, lit_ptr : &'c PointerValue) -> FloatValue<'c> {
        match lit_type {
            Type::Primitive(primitive) => {
                match primitive {
                    PrimitiveType::Float => {
                        let float_ptr = &lit_ptr.const_cast(self.context.f64_type().ptr_type(AddressSpace::default()).into());
                        let tmp_float = self.builder.build_load(self.context.f64_type().ptr_type(AddressSpace::default()), float_ptr.to_owned(), "");
                        return tmp_float.into_float_value();
                    }
                    _ => {return self.context.f64_type().const_float(0.0);}
                }
            }
            _ => {return self.context.f64_type().const_float(0.0);}
        }
    }

    // pub fn literal_unwrap(&self, lit_type : &Type, lit_ptr : &PointerValue) -> BasicValue {
    //     match lit_type {
    //         Type::Primitive(primitive) => {
                
    //             match primitive {
    //                 PrimitiveType::Bool => {
    //                     let bool_ptr = &lit_ptr.const_cast(self.context.bool_type().into());
    //                     let tmp_bool = self.builder.build_load(bool_ptr, "");
    //                     return tmp_bool;
    //                     // return tmp_bool.into_int_value();
    //                 }
    //                 PrimitiveType::Char => {
    //                     let char_ptr = &lit_ptr.const_cast(self.context.i8_type().into());
    //                     let tmp_char = self.builder.build_load(char_ptr, "");
    //                     return tmp_char;
    //                     // return tmp_char.into_int_value();
    //                 }
    //                 PrimitiveType::Float => {
    //                     let float_ptr = &lit_ptr.const_cast(self.context.f64_type().into());
    //                     let tmp_float = self.builder.build_load(float_ptr, "");
    //                     return tmp_float;
    //                     // return tmp_float.into_float_value();
    //                 }
    //                 PrimitiveType::Int => {
    //                     let int_ptr = &lit_ptr.const_cast(self.context.i32_type().into());
    //                     let tmp_int = self.builder.build_load(int_ptr, "");
    //                     return tmp_int;
    //                     // return tmp_int.into_int_value();
    //                 }
    //                 PrimitiveType::String => {
    //                     return self.builder.build_load(lit_ptr, "").into_struct_value();
    //                 }
    //                 _ => {}
    //             }
    //         }
    //         _ => {}
    //     }
    // }

    pub fn codegen_func<'c : 'ctx>(&'c self, typedFunc : &'c TypedFunc, mut func_name_table :  SymTable<String, FunctionValue<'c>>) 
    -> SymTable<String, FunctionValue<'c>>
    {
        // set up local symbol table for every level of block
        let return_ty = self.type2struct_type(&typedFunc.return_ty);
        let mut param_types :Vec<BasicMetadataTypeEnum> = Vec::new();
        for single_type in &typedFunc.params {
            param_types.push(inkwell::types::BasicMetadataTypeEnum::PointerType(
                    self.type2struct_type(&single_type.1).ptr_type(AddressSpace::default())
                )
            );
        }

        let fn_type = return_ty.fn_type(&param_types, false);
        let fn_value = self.module.add_function(&typedFunc.name, fn_type, None);
        
        func_name_table.insert(typedFunc.name.clone(), fn_value.clone()).clone_into(&mut func_name_table);

        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        let mut local_sym_ptr_table : SymTable<String, inkwell::values::PointerValue<'c>> = SymTable::new();
        
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
        
        let mut tmp_table : SymTable<String, FunctionValue<'c>> = SymTable::new();
        func_name_table.clone_into(&mut tmp_table);

        self.codegen_block(& typedFunc.body, local_sym_table, local_sym_ptr_table, tmp_table);
        let mut ret_table :SymTable<String, FunctionValue<'c>> = SymTable::new();
        func_name_table.clone_into(&mut ret_table);
        return ret_table;
        
    }

    pub fn codegen_block<'c>(&'c self, typedBlock : &'c TypedBlock, 
        mut parent_sym_table :  SymTable<String, Type>, 
        mut parent_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'c>>,
        func_name_table : SymTable<String, FunctionValue<'c>>) {
        
        let mut local_sym_table : SymTable<String, Type> = parent_sym_table.clone();
        let mut local_sym_ptr_table : SymTable<String, inkwell::values::PointerValue<'c>> = parent_sym_ptr_table.clone();
        
        for expr in &typedBlock.exprs {
            let mut tmp_table : SymTable<String, FunctionValue<'c>> = SymTable::new();
            func_name_table.clone_into(&mut tmp_table);
            let tmp_res = self.codegen_expr(expr,
                local_sym_table, 
                local_sym_ptr_table,
                tmp_table,
            );
            local_sym_table = tmp_res.sym_table.clone();
            local_sym_ptr_table = tmp_res.sym_ptr_table.clone();
        }
    }

    

    pub fn codegen_expr<'c>(&'c self, typedExpr : &'c TypedExpr,
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'c>>, 
        func_name_table : SymTable<String, FunctionValue<'c>>) -> TypedPointervalue_table<'c> {
        
        match typedExpr {
            // TypedExpr::Array() => {

            // }
            TypedExpr::Assign(typeAssign) => {
                let tmp_res = self.codegen_assign(typeAssign, block_sym_table, block_sym_ptr_table, func_name_table);
                block_sym_table = tmp_res.sym_table.clone();
                block_sym_ptr_table = tmp_res.sym_ptr_table.clone();

                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);    
            }
            // TypedExpr::BinOp(typeBinop) => {
                
            // }
            // TypedExpr::Block() => {
                
            // }
            // TypedExpr::Break => {
                
            // }
            TypedExpr::Call(typedCall) => {
                let mut tmp_table : SymTable<String, FunctionValue<'c>> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);

                let mut tmp_sym_table : SymTable<String, Type> = SymTable::new();
                block_sym_table.clone_into(&mut tmp_sym_table);
                let mut tmp_sym_ptr_table : SymTable<String, PointerValue<'c>> = SymTable::new();
                block_sym_ptr_table.clone_into(&mut tmp_sym_ptr_table);

                let mut ret =  self.codegen_call(&typedCall, 
                                                                      tmp_sym_table, 
                                                                      tmp_sym_ptr_table,
                                                                      tmp_table);

                ret.sym_table = block_sym_table.clone();
                ret.sym_ptr_table = block_sym_ptr_table.clone();
                return ret;
            }
            // TypedExpr::Closure() => {
                
            // }
            // TypedExpr::Continue => {
                
            // }
            // TypedExpr::Ctor() => {
                
            // }
            // TypedExpr::For() => {
                
            // }
            // TypedExpr::If() => {
                
            // }
            // TypedExpr::Index() => {
                
            // }
            TypedExpr::Let(typelet) => {
                let tmp_res = self.codegen_let(&typelet, block_sym_table, block_sym_ptr_table, func_name_table);
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
                let mut tmp_table : SymTable<String, FunctionValue<'c>> = SymTable::new();
                func_name_table.clone_into(&mut tmp_table);

                let mut tmp_sym_table : SymTable<String, Type> = SymTable::new();
                block_sym_table.clone_into(&mut tmp_sym_table);
                let mut tmp_sym_ptr_table : SymTable<String, PointerValue<'c>> = SymTable::new();
                block_sym_ptr_table.clone_into(&mut tmp_sym_ptr_table);

                self.codegen_return(&typeReturn, tmp_sym_table, tmp_sym_ptr_table, tmp_table);
                return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table);
            }
            // TypedExpr::Tuple() => {
                
            // }
            // TypedExpr::UnOp(typedUnOp) => {
            //     return self.codegen_unOp(&typedUnOp, &mut block_sym_table, &mut block_sym_ptr_table);
            // }
            TypedExpr::Variable(var) => {
                if block_sym_table.get(&var.name) == None {
                    block_sym_table.insert(
                        var.name.clone(),
                        var.ty.clone()
                    ).clone_into(&mut block_sym_table);
                    let var_ptr_type = self.type2struct_type(&var.ty);
                    let var_ptr = self.builder.build_alloca(var_ptr_type, &var.name);
                    block_sym_ptr_table.insert(
                        var.name.clone(),
                        var_ptr.clone(),
                        // symbol table generation, ptr for all
                    ).clone_into(&mut block_sym_ptr_table);
                }
                if let Some(var_ptr) = block_sym_ptr_table.get(&var.name) {
                    // let return_value = Some(
                    //     TypedPointervalue::new(
                    //         var.ty.clone(), 
                    //         var_ptr.to_owned(),
                    //     )
                    // );
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
            // TypedExpr::While() => {
                
            // }
            _ => { return TypedPointervalue_table::new_None(block_sym_table, block_sym_ptr_table); }
        }
    }
    
    // pub fn codegen_binOP(&self, typedBinOp : &TypedBinOp, 
    //     block_sym_table : &mut SymTable<String, Type>, 
    //     block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) 
    //     -> Option<TypedPointervalue> {
        
    //         let _rhs_res = self.codegen_expr(&typedBinOp.rhs, &mut block_sym_table, &mut block_sym_ptr_table);
    //         let _lhs_res = self.codegen_expr(&typedBinOp.lhs, &mut block_sym_table, &mut block_sym_ptr_table);
            
    //         if let (Some(lhs_res), Some(rhs_res)) = (_lhs_res, _rhs_res) {
    //             let mut rhs_value = self.literal_unwrap(&typedBinOp.ty, &rhs_res.ptr);
    //             let mut lhs_value = self.literal_unwrap(&typedBinOp.ty, &lhs_res.ptr);

    //             let mut tmp_result : BasicValue;

    //             let bool_struct = self.context.struct_type(&[self.context.bool_type().into()], false);
    //             let int_struct = self.context.struct_type(&[self.context.i32_type().into()], false);
    //             let float_struct = self.context.struct_type(&[self.context.f64_type().into()], false);
    //             let char_struct = self.context.struct_type(&[self.context.i8_type().into()], false);
    //             match &typedBinOp.ty {
    //                 PrimitiveType::Int => {
    //                     match &typedBinOp.op {
    //                         BinOp::Add => {
    //                             let tmp_result = self.builder.build_int_add(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Sub => {
    //                             let tmp_result = self.builder.build_int_sub(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Mul => {
    //                             let tmp_result = self.builder.build_int_mul(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Div => {
    //                             let tmp_result = self.builder.build_int_signed_div(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Mod => {
    //                             let tmp_result = self.builder.build_int_signed_rem(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Eq => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Neq => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Lt => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::SLT, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Gt => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::SGT, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Lte => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::SLE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Gte => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::SGE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::And => {
    //                             let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Or => {
    //                             let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Int,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         _ => {return None;}
    //                     }
    //                 }
    //                 PrimitiveType::Float => {
    //                     rhs_value = self.builder.build_float_cast(&rhs_value, self.context.f64_type().into(), "");
    //                     lhs_value = self.builder.build_float_cast(&lhs_value, self.context.f64_type().into(), "");
    //                     match &typedBinOp.op {
    //                         BinOp::Add => {
    //                             let tmp_result = self.builder.build_float_add(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Float,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Sub => {
    //                             let tmp_result = self.builder.build_float_sub(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Float,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Mul => {
    //                             let tmp_result = self.builder.build_float_mul(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Float,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Div => {
    //                             let tmp_result = self.builder.build_float_div(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Float,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Mod => {
    //                             let tmp_result = self.builder.build_float_rem(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Float,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Eq => {
    //                             let tmp_result = self.builder.build_float_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Neq => {
    //                             let tmp_result = self.builder.build_float_compare(IntPredicate::NE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Lt => {
    //                             let tmp_result = self.builder.build_float_compare(IntPredicate::SLT, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Gt => {
    //                             let tmp_result = self.builder.build_float_compare(IntPredicate::SGT, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Lte => {
    //                             let tmp_result = self.builder.build_float_compare(IntPredicate::SLE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Gte => {
    //                             let tmp_result = self.builder.build_float_compare(IntPredicate::SGE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         _ => {return None;}
    //                     }
    //                 }
    //                 PrimitiveType::Char => {
    //                     match &typedBinOp.op {
    //                         BinOp::Add => {
    //                             let tmp_result = self.builder.build_int_add(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Sub => {
    //                             let tmp_result = self.builder.build_int_sub(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Mul => {
    //                             let tmp_result = self.builder.build_int_mul(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Div => {
    //                             let tmp_result = self.builder.build_int_signed_div(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Mod => {
    //                             let tmp_result = self.builder.build_int_signed_rem(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Eq => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Neq => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Lt => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::ULT, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Gt => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::UGT, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Lte => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::ULE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Gte => {
    //                             let tmp_result = self.builder.build_int_compare(IntPredicate::UGE, lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::And => {
    //                             let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Or => {
    //                             let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Char,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         _ => {return None;}
    //                     }
    //                 }
    //                 PrimitiveType::Bool => {
    //                     match &typedBinOp.op {
    //                         BinOp::And => {
    //                             let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         BinOp::Or => {
    //                             let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
    //                             let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
    //                             self.builder.build_store(tmp_result_ptr, tmp_result);
    //                             return Some(TypedPointervalue::new(
    //                                     &PrimitiveType::Bool,
    //                                     &tmp_result_ptr,
    //                                 )
    //                             );
    //                         }
    //                         _ => {return None;}
    //                     }
                        
    //                 }
    //                 _ => {return None;}
    //             }
    //         }
    //         else {
    //             return None;
    //         }

    // }
    // pub fn codegen_unOp(&self, typedUnOp : &TypedUnOp, 
    //     block_sym_table : &mut SymTable<String, Type>, 
    //     block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) 
    //     -> Option<TypedPointervalue> {
    
    //     let _rhs_res = self.codegen_expr(&typedUnOp.rhs, &mut block_sym_table, &mut block_sym_ptr_table);
        
    //     if let Some(rhs_res) = _rhs_res {
    //         match typedUnOp.op {
    //             UnOp::Neg => {
    //                 let tmp_value = self.literal_unwrap(&typedUnOp.ty, &rhs_res.ptr);
    //                 match &typedUnOp.ty {
    //                     PrimitiveType::Int => {
    //                         let zero_value = self.context.i32_type().const_zero();
    //                         let tmp_result = self.builder.build_int_sub(zero_value, tmp_value, "");
    //                         let tmp_result_ptr = self.builder.build_alloca(
    //                             self.context.struct_type(&[self.context.i32_type().into()], false), 
    //                             "");
    //                         self.builder.build_store(tmp_result_ptr, tmp_result);
    //                         return Some(TypedPointervalue::new(
    //                                 &typedUnOp.ty,
    //                                 &tmp_result_ptr,
    //                             )
    //                         );
    //                     }
    //                     PrimitiveType::Float => {
    //                         let zero_value = self.context.f64_type().const_float(0);
    //                         let tmp_result = self.builder.build_float_sub(zero_value, tmp_value, "");
    //                         let tmp_result_ptr = self.builder.build_alloca(
    //                             self.context.struct_type(&[self.context.f64_type().into()], false), 
    //                             "");
    //                         self.builder.build_store(tmp_result_ptr, tmp_result);
    //                         return Some(TypedPointervalue::new(
    //                                 &typedUnOp.ty,
    //                                 &tmp_result_ptr,
    //                             )
    //                         );
    //                     }
    //                     _ => {return None;}
    //                 }
                    
    //             }
    //             UnOp::Not => {
    //                 let tmp_value = self.literal_unwrap(&typedUnOp.ty, &rhs_res.ptr);
    //                 match &typedUnOp.ty {
    //                     PrimitiveType::Bool => {
    //                         let tmp_result = self.builder.build_not(tmp_value, "");
    //                         let tmp_result_ptr = self.builder.build_alloca(
    //                             self.context.struct_type(&[self.context.bool_type().into()], false), 
    //                             "",
    //                         );
    //                         self.builder.build_store(tmp_result_ptr, tmp_result);
    //                         return Some(TypedPointervalue::new(
    //                                 &typedUnOp.ty,
    //                                 &tmp_result_ptr,
    //                             )
    //                         );
    //                     }
    //                     _ =>{return None;}
    //                 }
    //             }
    //             _ => {return None;}
    //         }
    //     }
    //     else {
    //         return None;
    //     }
        
    // }
    pub fn codegen_call<'c>(&'c self, typedCall : &'c TypedCall, 
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'c>>,
        func_name_table : SymTable<String, FunctionValue<'c>>) 
        -> TypedPointervalue_table<'c>{

        
        match &(typedCall.ty) {
            Type::Func(in_arg, out_arg) => {
                match typedCall.func.as_ref().clone() {
                    TypedExpr::Variable(variable) => {
                        let mut tmp_table : SymTable<String, FunctionValue<'c>> = SymTable::new();
                        func_name_table.clone_into(&mut tmp_table);

                        if let Some(_fn_value) = tmp_table.get(&variable.name){
                            let fn_value = _fn_value.clone();
                            let mut params :Vec<BasicMetadataValueEnum> = Vec::new();
                
                            for arg in &typedCall.args {
                                let mut local_sym_table : SymTable<String, Type> = SymTable::new();
                                block_sym_table.clone_into(&mut local_sym_table);
                                let mut local_sym_ptr_table : SymTable<String, PointerValue<'c>> = SymTable::new();
                                block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);
                                let mut tmp_table : SymTable<String, FunctionValue<'c>> = SymTable::new();
                                func_name_table.clone_into(&mut tmp_table);
                                let tmp_arg =  self.codegen_expr(&arg, local_sym_table, local_sym_ptr_table, tmp_table);

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
                                let real_real_result_ptr = self.builder.build_alloca(real_real_result.get_type(), "");
                                self.builder.build_store(real_real_result_ptr, real_real_result);
                                
                                return TypedPointervalue_table::new(
                                    out_arg.to_owned().as_ref().to_owned(), 
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
            }
            _ => {
                return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
            }
        }
        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }
    pub fn codegen_return<'c>(&self, typeReturn : &TypedReturn, 
        mut block_sym_table : SymTable<String, Type>, 
        mut block_sym_ptr_table : SymTable<String, inkwell::values::PointerValue<'c>>, 
        func_name_table : SymTable<String, FunctionValue<'c>>) 
        -> TypedPointervalue_table<'c>{
        
        if let Some(expr) = &typeReturn.expr.as_ref() {
            let mut local_sym_table : SymTable<String, Type> = SymTable::new();
            block_sym_table.clone_into(&mut local_sym_table);
            let mut local_sym_ptr_table : SymTable<String, PointerValue<'c>> = SymTable::new();
            block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

            let _type_ptr = self.codegen_expr(&expr, local_sym_table, local_sym_ptr_table, func_name_table);
            if let Some(type_ptr) = _type_ptr.TypePointer {
                let mut pointee_type_int = self.context.i32_type();
                let mut pointee_type_float = self.context.f64_type();
                let mut float_flag : i32 = 0;
                if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                    float_flag = 1;
                    pointee_type_float = self.context.f64_type();
                }
                else {
                    if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                        pointee_type_int = self.context.i32_type();
                    }
                    else if type_ptr.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char){
                        pointee_type_int = self.context.i8_type();
                    }
                    else {
                        pointee_type_int = self.context.bool_type();
                    }
                }
                let mut return_value;
                if float_flag == 1{
                    return_value = self.builder.build_load(pointee_type_float, type_ptr.ptr.to_owned(), "").into_struct_value();
                }
                else {
                    return_value = self.builder.build_load(pointee_type_int, type_ptr.ptr.to_owned(), "").into_struct_value();
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
    pub fn codegen_let<'c>(&'c self, typeLet : &'c TypedLet, 
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'c>>,
        func_name_table : SymTable<String, FunctionValue<'c>>
    ) 
        -> TypedPointervalue_table<'c>{
        
        let var_name = &typeLet.name.clone();
        if block_sym_table.get(var_name) == None {
            block_sym_table.insert(
                typeLet.name.clone(),
                typeLet.ty.clone()
            ).clone_into(&mut block_sym_table);
            let typeLet_ty_copy = &typeLet.ty;
            let var_ptr_type = self.type2struct_type(typeLet_ty_copy);
            let var_ptr = self.builder.build_alloca(var_ptr_type, &typeLet.name.clone());
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
        let mut local_sym_ptr_table : SymTable<String, PointerValue<'c>> = SymTable::new();
        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

        let _rhs = self.codegen_expr(&typeLet.rhs, local_sym_table, local_sym_ptr_table, func_name_table);

        if let Some(var_type) = _var_type {
            if let Some(var_ptr) = _var_ptr {
                if let Some(rhs_result) = _rhs.TypePointer {
                    let mut pointee_type_int = self.context.i32_type();
                    let mut pointee_type_float = self.context.f64_type();
                    let mut float_flag : i32 = 0;
                    if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                        float_flag = 1;
                        pointee_type_float = self.context.f64_type();
                    }
                    else {
                        if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                            pointee_type_int = self.context.i32_type();
                        }
                        else if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char ){
                            pointee_type_int = self.context.i8_type();
                        }
                        else {
                            pointee_type_int = self.context.bool_type();
                        }
                    }
                    let mut return_value;
                    if float_flag == 1{
                        return_value = self.builder.build_load(pointee_type_float, rhs_result.ptr.to_owned(), "").into_struct_value();
                    }
                    else {
                        return_value = self.builder.build_load(pointee_type_int, rhs_result.ptr.to_owned(), "").into_struct_value();
                    }
                    
                
                    self.builder.build_store(var_ptr.to_owned(), return_value);
                }
            }
        }
        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }

    pub fn codegen_assign<'c>(&'c self, typeAssign : &'c TypedAssign, 
        mut block_sym_table :  SymTable<String, Type>, 
        mut block_sym_ptr_table :  SymTable<String, inkwell::values::PointerValue<'c>>,
        func_name_table : SymTable<String, FunctionValue<'c>>
    ) 
        -> TypedPointervalue_table<'c>{
        
        let var_name = &typeAssign.name;
        let _var_ptr = block_sym_ptr_table.get(var_name);

        
        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        block_sym_table.clone_into(&mut local_sym_table);
        let mut local_sym_ptr_table : SymTable<String, PointerValue<'c>> = SymTable::new();
        block_sym_ptr_table.clone_into(&mut local_sym_ptr_table);

        let _rhs = self.codegen_expr(&typeAssign.rhs, local_sym_table, local_sym_ptr_table, func_name_table);


        if let Some(var_ptr) = _var_ptr {
            if let Some(rhs_result) = _rhs.TypePointer {
                
                let mut pointee_type_int = self.context.i32_type();
                let mut pointee_type_float = self.context.f64_type();
                let mut float_flag : i32 = 0;
                if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Float) {
                    float_flag = 1;
                    pointee_type_float = self.context.f64_type();
                }
                else {
                    if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Int) {
                        pointee_type_int = self.context.i32_type();
                    }
                    else if rhs_result.pointer_type.to_owned() == Type::Primitive(PrimitiveType::Char ){
                        pointee_type_int = self.context.i8_type();
                    }
                    else {
                        pointee_type_int = self.context.bool_type();
                    }
                }
                let mut return_value;
                if float_flag == 1{
                    return_value = self.builder.build_load(pointee_type_float, rhs_result.ptr.to_owned(), "").into_struct_value();
                }
                else {
                    return_value = self.builder.build_load(pointee_type_int, rhs_result.ptr.to_owned(), "").into_struct_value();
                }
                
                self.builder.build_store(var_ptr.to_owned(), return_value);
            }
        }

        return TypedPointervalue_table::new_None(block_sym_table.clone(), block_sym_ptr_table.clone());
    }
    
    

}






