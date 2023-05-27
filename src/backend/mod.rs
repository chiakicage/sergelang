pub use crate::midend::typed_ast::*;
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
    pub unnameed_index : i32,
    pub func_name_table : SymTable<String, FunctionValue>,
}

pub struct TypedPointervalue {
    pub pointer_type : Type,
    pub ptr : inkwell::values::PointerValue,
}

impl TypedPointervalue {
    pub fn new(
        pointer_type : &Type,
        ptr : &inkwell::values::PointerValue,
    ) -> Self {
        Self {
            pointer_type, 
            ptr,
        }
    }
}

impl<'ctx, 'a> CodeGen<'ctx, 'a> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder,
            unnameed_index: (0),
            func_name_table: SymTable<String, FunctionValue>::new(),
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

    pub fn gen_new_label(&self, str : &String) -> String {
        self.unnameed_index = self.unnameed_index + 1;
        let new_label = format!("{}{}", str.to_owned(), self.unnameed_index.to_string());
        return new_label;
    }

    pub fn codegen_module(&self, typedast : &TypedModule) {

        for func in typedast.func_defs {
            self.codegen_func(&func);
        }

    }

    pub fn type2struct_type(&self, ser_type : &Type) -> inkwell::types::StructType{
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
                            &[self.context.void_type().into()], 
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
        self.unnameed_index = self.unnameed_index + 1;
        match typedLiteral {
            TypedLiteral::Bool(bool) => {
                let bool_struct = self.context.struct_type(
                    &[self.context.bool_type().into()], 
                    false
                );
                let ret = self.builder.build_alloca(
                    bool_struct, 
                    &self.unnameed_index.to_string()
                );
                let tmp_bool = self.context.bool_type().const_int(bool, false);
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
                    &self.unnameed_index.to_string()
                );
                let tmp_char = self.context.i8_type().const_int(char, false);
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
                    &self.unnameed_index.to_string()
                );
                let tmp_float = self.context.f64_type().const_int(float, false);
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
                    &self.unnameed_index.to_string()
                );
                let tmp_int = self.context.i32_type().const_int(int, false);
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
                    &self.unnameed_index.to_string()
                );
                self.unnameed_index = self.unnameed_index + 1;

                let tmp_length = str.len();
                let tmp_length_value = self.context.i32_type().const_int(tmp_length, false);

                let payload_type = self.context.i8_type().ptr_type(AddressSpace::default());
                let payload = self.builder.build_array_malloc(self.context.i8_type(), tmp_length_value, &self.unnameed_index.to_string());

                
                match payload {
                    Ok(payload_ptr, result_str) => {
                        let tmp_str_struct = str_struct.const_named_struct(&[tmp_length_value.into(), payload_ptr.into()]);
                        self.builder.build_store(ret, tmp_str_struct);

                        let mut cur_addr = payload_ptr.clone().const_to_int(self.context.i64_type().into());
                        let next_offset = self.context.i64_type().const_int(1, false);
                        for char in str.chars() {
                            self.unnameed_index = self.unnameed_index + 1;
                            
                            let tmp_char_value = self.context.i8_type().const_int(char, false);
                            cur_addr = self.builder.build_int_add(cur_addr, next_offset, &self.unnameed_index.to_string());

                            self.builder.build_store(cur_addr.const_to_pointer(self.context.i8_type()), tmp_char_value);
                        }
                    }
                    _ => {
                        let tmp_str_struct = str_struct.const_named_struct(&[tmp_length_value.into(), payload_type.const_zero().into()]);
                        self.builder.build_store(ret, tmp_str_struct);
                    }
                }

                
                return ret;
            }
            _ => {}
        }
    }

    pub fn literal_unwrap(&self, lit_type : &Type, lit_ptr : &PointerValue) -> BasicValue {
        match lit_type {
            Type::Primitive(primitive) => {
                self.unnameed_index = self.unnameed_index + 1;
                match primitive {
                    PrimitiveType::Bool => {
                        let bool_ptr = &lit_ptr.const_cast(self.context.bool_type().into());
                        let tmp_bool = self.builder.build_load(bool_ptr, &self.unnameed_index.to_string());
                        return tmp_bool.into_int_value();
                    }
                    PrimitiveType::Char => {
                        let char_ptr = &lit_ptr.const_cast(self.context.i8_type().into());
                        let tmp_char = self.builder.build_load(char_ptr, &self.unnameed_index.to_string());
                        return tmp_char.into_int_value();
                    }
                    PrimitiveType::Float => {
                        let float_ptr = &lit_ptr.const_cast(self.context.f64_type().into());
                        let tmp_float = self.builder.build_load(f64_ptr, &self.unnameed_index.to_string());
                        return tmp_float.into_float_value();
                    }
                    PrimitiveType::Int => {
                        let int_ptr = &lit_ptr.const_cast(self.context.i32_type().into());
                        let tmp_int = self.builder.build_load(int_ptr, &self.unnameed_index.to_string());
                        return tmp_int.into_int_value();
                    }
                    PrimitiveType::String => {
                        return self.builder.build_load(lit_ptr, &self.unnameed_index.to_string()).into_struct_value();
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    pub fn codegen_func(&self, typedFunc : &TypedFunc) {
        // set up local symbol table for every level of block
        let return_ty = self.type2struct_type(&typedFunc.return_ty);
        let param_types :Vec<BasicMetadataTypeEnum> = Vec::new();
        for single_type in typedFunc.params {
            param_types.push(self.type2struct_type(&single_type.1).ptr_type(AddressSpace::default()));
        }

        let fn_type = return_ty.fn_type(&param_types, false);
        let fn_value = self.module.add_function(&typedFunc.name, &fn_type, None);
        
        self.func_name_table.insert(&typedFunc.name, fn_value.clone());

        let mut local_sym_table : SymTable<String, Type> = SymTable::new();
        let mut local_sym_ptr_table : SymTable<String, inkwell::values::PointerValue> = SymTable::new();
        
        let mut param_counter : u32 = 0;
        for param in &typedFunc.params {
            local_sym_table = local_sym_table.insert(
                param.0.to_string(),
                param.1.clone()
            );
            local_sym_ptr_table = local_sym_ptr_table.insert(
                param.0.to_string(),
                fn_value.get_nth_param(param_counter).unwrap().into_pointer_value()
                // symbol table generation, ptr for all
            );
            param_counter = param_counter + 1;
        }
        let entry_name = format!("{:?}entry", (&typedFunc.name).to_string());
        let basic_block = self.context.append_basic_block(fn_value, &entry_name);
        self.builder.position_at_end(basic_block);
        
        self.codegen_block(&mut typedFunc.body, &mut local_sym_table, &mut local_sym_ptr_table);
        
    }

    pub fn codegen_block(&self, typedBlock : &TypedBlock, 
        mut parent_sym_table : &mut SymTable<String, Type>, 
        mut parent_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) {
        
        let mut local_sym_table : SymTable<String, Type> = parent_sym_table.clone();
        let mut local_sym_ptr_table : SymTable<String, inkwell::values::PointerValue> = parent_sym_ptr_table.clone();
        
        for expr in &typedBlock.exprs {
            self.codegen_expr(expr,
                &mut local_sym_table, 
                &mut local_sym_ptr_table
            );
        }
    }

    

    pub fn codegen_expr(&self, typedExpr : &TypedExpr,
        mut block_sym_table : &mut SymTable<String, Type>, 
        mut block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) -> Option<TypedPointervalue> {
        
        match typedExpr {
            // TypedExpr::Array() => {

            // }
            TypedExpr::Assign(typeAssign) => {
                self.codegen_assign(&typeAssign, &mut block_sym_table, &mut block_sym_ptr_table);
                return None;    
            }
            TypedExpr::BinOp(typeBinop) => {
                
            }
            // TypedExpr::Block() => {
                
            // }
            // TypedExpr::Break => {
                
            // }
            TypedExpr::Call(typedCall) => {
                return self.codegen_call(&typedCall, &mut block_sym_table, &mut block_sym_ptr_table);
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
                self.codegen_let(&typelet, &mut block_sym_table, &mut block_sym_ptr_table);
                return None;
            }
            TypedExpr::Literal(literal) => {
                return Some(
                    TypedPointervalue::new(
                        &literal.ty(), 
                        &self.literal_create_wrapper(literal)
                    )
                );
            }
            // TypedExpr::Match() => {

            // }
            TypedExpr::Return(typeReturn) => {
                self.codegen_return(&typeReturn, &mut block_sym_table, &mut block_sym_ptr_table);
                return None;
            }
            // TypedExpr::Tuple() => {
                
            // }
            TypedExpr::UnOp(typedUnOp) => {
                return self.codegen_unOp(&typedUnOp, &mut block_sym_table, &mut block_sym_ptr_table);
            }
            TypedExpr::Variable(var) => {
                if block_sym_table.get(&var.name) == None {
                    &block_sym_table = block_sym_table.insert(
                        &var.name,
                        &var.ty
                    );
                    let var_ptr_type = self.type2struct_type(&var.ty);
                    let var_ptr = self.builder.build_alloca(var_ptr_type, &var.name);
                    &block_sym_ptr_table = block_sym_ptr_table.insert(
                        &var.name,
                        var_ptr,
                        // symbol table generation, ptr for all
                    );
                }
                return Some(
                    TypedPointervalue::new(
                        &var.ty, 
                        block_sym_ptr_table.get(&var.name)
                    )
                );
            }
            // TypedExpr::While() => {
                
            // }
            _ => {}
        }
    }
    
    pub fn codegen_binOP(&self, typedBinOp : &TypedBinOp, 
        block_sym_table : &mut SymTable<String, Type>, 
        block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) 
        -> Option<Type, inkwell::values::PointerValue> {
        
            let _rhs_res = self.codegen_expr(&typedBinOp.rhs, &mut block_sym_table, &mut block_sym_ptr_table);
            let _lhs_res = self.codegen_expr(&typedBinOp.lhs, &mut block_sym_table, &mut block_sym_ptr_table);
            
            if let (Some(lhs_res), Some(rhs_res)) = (_lhs_res, _rhs_res) {
                let rhs_value = self.literal_unwrap(&typedBinOp.ty, &rhs_res.ptr);
                let lhs_value = self.literal_unwrap(&typedBinOp.ty, &lhs_res.ptr);

                let mut tmp_result : BasicValue;

                let bool_struct = self.context.struct_type(&[self.context.bool_type().into()], false);
                let int_struct = self.context.struct_type(&[self.context.i32_type().into()], false);
                let float_struct = self.context.struct_type(&[self.context.f64_type().into()], false);
                let char_struct = self.context.struct_type(&[self.context.i8_type().into()], false);
                match &typedBinOp.ty {
                    PrimitiveType::Int => {
                        match &typedBinOp.op {
                            BinOp::Add => {
                                let tmp_result = self.builder.build_int_add(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Sub => {
                                let tmp_result = self.builder.build_int_sub(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Mul => {
                                let tmp_result = self.builder.build_int_mul(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Div => {
                                let tmp_result = self.builder.build_int_signed_div(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Mod => {
                                let tmp_result = self.builder.build_int_signed_rem(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Eq => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Neq => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Lt => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::SLT, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Gt => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::SGT, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Lte => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::SLE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Gte => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::SGE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::And => {
                                let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Or => {
                                let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(int_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Int,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            _ => {return None;}
                        }
                    }
                    PrimitiveType::Float => {
                        match &typedBinOp.op {
                            BinOp::Add => {
                                let tmp_result = self.builder.build_float_add(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Float,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Sub => {
                                let tmp_result = self.builder.build_float_sub(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Float,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Mul => {
                                let tmp_result = self.builder.build_float_mul(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Float,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Div => {
                                let tmp_result = self.builder.build_float_div(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Float,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Mod => {
                                let tmp_result = self.builder.build_float_rem(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(float_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Float,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Eq => {
                                let tmp_result = self.builder.build_float_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Neq => {
                                let tmp_result = self.builder.build_float_compare(IntPredicate::NE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Lt => {
                                let tmp_result = self.builder.build_float_compare(IntPredicate::SLT, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Gt => {
                                let tmp_result = self.builder.build_float_compare(IntPredicate::SGT, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Lte => {
                                let tmp_result = self.builder.build_float_compare(IntPredicate::SLE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Gte => {
                                let tmp_result = self.builder.build_float_compare(IntPredicate::SGE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            _ => {return None;}
                        }
                    }
                    PrimitiveType::Char => {
                        match &typedBinOp.op {
                            BinOp::Add => {
                                let tmp_result = self.builder.build_int_add(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Sub => {
                                let tmp_result = self.builder.build_int_sub(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Mul => {
                                let tmp_result = self.builder.build_int_mul(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Div => {
                                let tmp_result = self.builder.build_int_signed_div(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Mod => {
                                let tmp_result = self.builder.build_int_signed_rem(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Eq => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::EQ, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Neq => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::NE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Lt => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::ULT, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Gt => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::UGT, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Lte => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::ULE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Gte => {
                                let tmp_result = self.builder.build_int_compare(IntPredicate::UGE, lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::And => {
                                let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Or => {
                                let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(char_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Char,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            _ => {return None;}
                        }
                    }
                    PrimitiveType::Bool => {
                        match &typedBinOp.op {
                            BinOp::And => {
                                let tmp_result = self.builder.build_and(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            BinOp::Or => {
                                let tmp_result = self.builder.build_or(lhs_value, rhs_value, "");
                                let tmp_result_ptr = self.builder.build_alloca(bool_struct, "");
                                self.builder.build_store(tmp_result_ptr, tmp_result);
                                return Some(TypedPointervalue::new(
                                        &PrimitiveType::Bool,
                                        &tmp_result_ptr,
                                    )
                                );
                            }
                            _ => {return None;}
                        }
                        
                    }
                    _ => {return None;}
                }
            }
            else {
                return None;
            }

    }
    pub fn codegen_unOp(&self, typedUnOp : &TypedUnOp, 
        block_sym_table : &mut SymTable<String, Type>, 
        block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) 
        -> Option<Type, inkwell::values::PointerValue> {
    
        let _rhs_res = self.codegen_expr(&typedUnOp.rhs, &mut block_sym_table, &mut block_sym_ptr_table);
        
        if let Some(rhs_res) = _rhs_res {
            match typedUnOp.op {
                UnOp::Neg => {
                    let tmp_value = self.literal_unwrap(&typedUnOp.ty, &rhs_res.ptr);
                    match &typedUnOp.ty {
                        PrimitiveType::Int => {
                            let zero_value = self.context.i32_type().const_zero();
                            let tmp_result = self.builder.build_int_sub(zero_value, tmp_value, "");
                            let tmp_result_ptr = self.builder.build_alloca(
                                self.context.struct_type(&[self.context.i32_type().into()], false), 
                                "");
                            self.builder.build_store(tmp_result_ptr, tmp_result);
                            return Some(TypedPointervalue::new(
                                    &typedUnOp.ty,
                                    &tmp_result_ptr,
                                )
                            );
                        }
                        PrimitiveType::Float => {
                            let zero_value = self.context.f64_type().const_float(0);
                            let tmp_result = self.builder.build_float_sub(zero_value, tmp_value, "");
                            let tmp_result_ptr = self.builder.build_alloca(
                                self.context.struct_type(&[self.context.f64_type().into()], false), 
                                "");
                            self.builder.build_store(tmp_result_ptr, tmp_result);
                            return Some(TypedPointervalue::new(
                                    &typedUnOp.ty,
                                    &tmp_result_ptr,
                                )
                            );
                        }
                        _ => {return None;}
                    }
                    
                }
                UnOp::Not => {
                    let tmp_value = self.literal_unwrap(&typedUnOp.ty, &rhs_res.ptr);
                    match &typedUnOp.ty {
                        PrimitiveType::Bool => {
                            let tmp_result = self.builder.build_not(tmp_value, "");
                            let tmp_result_ptr = self.builder.build_alloca(
                                self.context.struct_type(&[self.context.bool_type().into()], false), 
                                "",
                            );
                            self.builder.build_store(tmp_result_ptr, tmp_result);
                            return Some(TypedPointervalue::new(
                                    &typedUnOp.ty,
                                    &tmp_result_ptr,
                                )
                            );
                        }
                        _ =>{return None;}
                    }
                }
                _ => {return None;}
            }
        }
        else {
            return None;
        }
        
    }
    pub fn codegen_call(&self, typedCall : &TypedCall, 
        mut block_sym_table : &mut SymTable<String, Type>, 
        mut block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) 
        -> Option<Type, inkwell::values::PointerValue>{
        
        match (typedCall.func, typedCall.ty) {
            (TypedExpr::Variable(var), Type::Func(in_arg, out_arg)) => {
                let fn_value = self.func_name_table.get(&var.name);
                let params :Vec<BasicMetadataValueEnum> = Vec::new();
                
                for arg in typedCall.args {
                    if let Some(type_ptr) = self.codegen_expr(&arg, &mut block_sym_table, &mut block_sym_ptr_table) {
                        params.push(type_ptr.ptr);
                    }
                }

                let call_result = &self.builder.build_call(fn_value, &params, "");
                let real_result = call_result.try_as_basic_value();
                if(real_result.is_right()){
                    return None;
                }
                else if  real_result.is_left() {
                    let real_real_result = &real_result.left().unwrap().into_struct_value();
                    let real_real_result_ptr = self.builder.build_alloca(real_real_result.get_type(), "");
                    self.builder.build_store(real_real_result_ptr.clone(), real_real_result);
                    return Some(TypedPointervalue::new(
                            &out_arg,
                            &real_real_result_ptr.clone(),
                            )
                        );
                }

            }
            _ => {
                return None;
            }
        }
    }
    pub fn codegen_return(&self, typeReturn : &TypedReturn, 
        mut block_sym_table : &mut SymTable<String, Type>, 
        mut block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) {
        
        if let Some(expr) = &typeReturn.expr.as_ref() {
            let _type_ptr = self.codegen_expr(&expr, &mut block_sym_table, &mut block_sym_ptr_table);
            if let Some(type_ptr) = _type_ptr {
                let return_value = self.builder.build_load(&type_ptr.ptr, "").into_struct_value();
                self.builder.build_return(Some(&return_value));
            }
            else {
                self.builder.build_return(None);
            }
        }
        else {
            self.builder.build_return(None);
        }

    }
    pub fn codegen_let(&self, typeLet : &TypedLet, 
        mut block_sym_table : &mut SymTable<String, Type>, 
        mut block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) {
        
        let var_name = &typeLet.name;
        if block_sym_table.get(var_name) == None {
            &block_sym_table = block_sym_table.insert(
                &typeLet.name,
                &typeLet.ty
            );
            let var_ptr_type = self.type2struct_type(&typeLet.ty);
            let var_ptr = self.builder.build_alloca(var_ptr_type, &typeLet.name);
            &block_sym_ptr_table = block_sym_ptr_table.insert(
                &typeLet.name,
                var_ptr,
                // symbol table generation, ptr for all
            );
        }

        let _var_type = block_sym_table.get(var_name);

        let _var_ptr = block_sym_ptr_table.get(var_name);

        let _rhs = self.codegen_expr(&typeLet.rhs, &mut block_sym_table, &mut block_sym_ptr_table);

        if let Some(var_type) = _var_type {
            if let Some(var_ptr) = _var_ptr {
                if let Some(lhs_result) = _rhs {
                    self.unnameed_index = self.unnameed_index + 1;
                    let tmp_value = self.builder.build_load(&lhs_result.ptr, &self.unnameed_index.to_string());
                    self.builder.build_store(var_ptr, tmp_value.into_struct_value());
                }
            }
        }
    }

    pub fn codegen_assign(&self, typeAssign : &TypedLet, 
        mut block_sym_table : &mut SymTable<String, Type>, 
        mut block_sym_ptr_table : &mut SymTable<String, inkwell::values::PointerValue>) {
        
        let var_name = &typeAssign.name;
        let _var_ptr = block_sym_ptr_table.get(var_name);

        let _rhs = self.codegen_expr(&typeAssign.rhs, &mut block_sym_table, &mut block_sym_ptr_table);


        if let Some(var_ptr) = _var_ptr {
            if let Some(lhs_result) = _rhs {
                self.unnameed_index = self.unnameed_index + 1;
                let tmp_value = self.builder.build_load(&lhs_result.ptr, &self.unnameed_index.to_string());
                self.builder.build_store(var_ptr, tmp_value.into_struct_value());
            }
        }
    }
    
    

}






