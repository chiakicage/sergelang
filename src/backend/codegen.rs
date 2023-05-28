use std::collections::HashMap;
use std::ptr::null_mut;

use crate::midend::mir::*;
use crate::midend::typed_ast::LiteralKind;
use crate::utils::type_context::*;

use libc::*;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use rpds::HashTrieMap;
use slotmap::SlotMap;

use super::runtime::RuntimeLibrary;

type SymTable<K, V> = HashTrieMap<K, V>;
type BinOp = crate::ast::BinOp;
type UnOp = crate::ast::UnOp;

#[derive(Debug)]
pub struct FunctionEmissionState<'a> {
    var_map: &'a SlotMap<VarRef, Var>,
    symbol_value_map: HashMap<VarRef, LLVMValueRef>,
    block_map: HashMap<BlockRef, LLVMBasicBlockRef>,
    alloca_block: LLVMBasicBlockRef,
    ret_ptr: LLVMValueRef,
}

pub struct CodeGen<'a> {
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub function: Option<FunctionEmissionState<'a>>,

    pub name_ctx: SymTable<String, TypeRef>,
    pub ty_ctx: TypeContext
}

impl<'a> CodeGen<'a> {
    pub fn new(
        name_ctx: &SymTable<String, TypeRef>,
        ty_ctx: &TypeContext,
    ) -> Self 
    {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext("Serge".as_ptr() as *const i8, context);
            let builder = LLVMCreateBuilderInContext(context);
            CodeGen {
                context,
                module,
                builder,
                function: None,
                name_ctx: name_ctx.clone(),
                ty_ctx: ty_ctx.clone(),
            }
        }
    }
}

impl<'a> CodeGen<'a> {

    fn create_rvalue(&self, value: &Rvalue) -> LLVMValueRef {
        let Rvalue {typ, val} = value;
        match val.as_ref() {
            RvalueEnum::BinaryOperator(op, lhs , rhs) => {
                let lhs_raw = self.extract_raw_value(lhs);
                let rhs_raw = self.extract_raw_value(rhs);
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Div | BinOp::Mod => self.create_arithematic(op, lhs_raw, rhs_raw),

                    _ => panic!("not implemented!"),
                }
            }
            _ => { panic!("not implemented1") }
        }
    }

    fn create_stmts(&self, blocks: &SlotMap<BlockRef, Block>) {
        let block_map = &self.function
                        .as_ref()
                        .unwrap()
                        .block_map;
        for (bb_ref, bb) in blocks {
            let current_bb = block_map.get(&bb_ref).unwrap().clone();

            // set insert point
            self.set_insert_point_before_terminator(current_bb);
            for stmt in bb.stmts.iter() {
                let rhs = stmt.right.as_ref().unwrap();
                let rvalue_need_raw = matches!(rhs.val.as_ref(), 
                        RvalueEnum::BinaryOperator(_, _, _) 
                        | RvalueEnum::UnaryOperator(_, _, _));
            }
        }
    }

    fn create_terminators(&self, blocks: &SlotMap<BlockRef, Block>) {
        unsafe {
            let function_state = self.function.as_ref().unwrap();
            let ret_ptr = function_state.ret_ptr;
            let block_map = &function_state.block_map;

            for (block_ref, block) in blocks {
                let current_bb = block_map.get(&block_ref).unwrap().clone();
                // insert at the end of bb.
                self.set_insert_point_before_terminator(current_bb);
                match &block.terminator {
                    Terminator::Branch(cond, and, or) => {
                        panic!("not");
                    }
                    _ => { panic!("not implemented"); }
                }
            }
        }
    }

    fn create_function(&mut self, mfn: &'a Func) {
        unsafe {
            let mfn_name = mfn.name.as_ptr() as *const i8;
            let mfn_type = self.ty_ctx.get_type_by_typeref(mfn.typ);
            if let Type::Callable {params: params, ret: ret} = mfn_type {
                let ret_ty = self.object_ptr_type();
                let mut params_ty = params.iter().map(|&x| self.object_ptr_type()).collect::<Vec<_>>();
                let fn_type = self.create_fn_type(ret_ty, 
                                                                 params_ty.as_mut_ptr(),
                                                                params_ty.len() as u32, 
                                                                false);
                
                let func = LLVMAddFunction(self.module, mfn_name, fn_type);

                // start generate basicblocks
                // create alloca basic block
                let alloca_bb = LLVMAppendBasicBlock(func, "alloca".as_ptr() as *const c_char);
                // set insert point
                self.set_insert_point_before_terminator(alloca_bb);
                // create ret value basic block
                let ret_ptr = LLVMBuildAlloca(self.builder, self.object_ptr_type(), "ret_val.ptr".as_ptr() as *const c_char);

                // emit all BB
                let mut block_map = HashMap::new();
                for (blockref, block) in mfn.blocks.iter() {
                    let name = block.name.clone();
                    let bb = LLVMAppendBasicBlock(func, name.as_ptr() as *const c_char);   
                    block_map.insert(blockref, bb);
                }
                
                self.function = Some(FunctionEmissionState { 
                    var_map: & mfn.variables,
                    symbol_value_map: HashMap::new(), 
                    block_map: block_map,
                    alloca_block: alloca_bb, 
                    ret_ptr: ret_ptr, 
                });

                // store arguments, local variables
                self.store_fn_variables(&mfn.variables, &mfn.locals);

                // jump from alloca bb to the first code bb.
                let entry_bb = LLVMGetNextBasicBlock(alloca_bb);
                LLVMBuildBr(self.builder, entry_bb);

                self.create_stmts(&mfn.blocks);
                self.create_terminators(&mfn.blocks);
                
            }
        }
    }

    pub fn create_module(&mut self, mir: &MIR) {

    }

    // In some cases, e.g. do comparison, we must expose raw data.
    // We will call runtime function here.
    fn extract_raw_value(&self, operand: &Operand) -> LLVMValueRef {
        let Operand {typ, val} = operand;
        unsafe {
            match val.as_ref() {
                OperandEnum::Imm(number) => {
                    let int_type = LLVMInt32TypeInContext(self.context);
                    LLVMConstInt(int_type, *number as u64, 0)
                }
                OperandEnum::Var(var) => {
                    let var_value = self.load_stack_slot_variable(*var);
                    let var_def = self.function
                                        .as_ref()
                                        .unwrap()
                                        .var_map
                                        .get(*var)
                                        .unwrap();
                    let typ = var_def.typ;
                    if typ == self.ty_ctx.get_primitive("i32") {
                        let runtime_func = self.get_runtime_extract_i32();
                        return LLVMBuildCall2(self.builder, 
                            LLVMGetCalledFunctionType(runtime_func), 
                            runtime_func, 
                            [var_value].as_mut_ptr(), 
                            1, 
                            "call".as_ptr() as *const c_char)
                    }
                    if typ == self.ty_ctx.get_primitive("f64") {
                        let runtime_func = self.get_runtime_extract_f64();
                        return LLVMBuildCall2(self.builder, 
                            LLVMGetCalledFunctionType(runtime_func), 
                            runtime_func, 
                            [var_value].as_mut_ptr(), 
                            1, 
                            "call".as_ptr() as *const c_char)
                    }
                    if typ == self.ty_ctx.get_primitive("bool") {
                        let runtime_func = self.get_runtime_extract_bool();
                        return LLVMBuildCall2(self.builder, 
                            LLVMGetCalledFunctionType(runtime_func), 
                            runtime_func, 
                            [var_value].as_mut_ptr(), 
                            1, 
                            "call".as_ptr() as *const c_char)
                    }
                    panic!("can't get raw value!")
                }
                OperandEnum::Literal(literal) => self.create_liteal(literal, false)
            }
        }
    }

    fn create_liteal(&self, literal: &LiteralKind, is_object: bool) -> LLVMValueRef {
        unsafe {
            match literal {
               LiteralKind::Int(i) => {
                    let int_type = LLVMInt32TypeInContext(self.context);
                    let raw_value = LLVMConstInt(int_type, *i as u64, 0);
                    if is_object {
                        return self.box_raw_value_i32(raw_value);
                    }
                    return raw_value;
                }
                LiteralKind::Float(f) => {
                    let double_type = LLVMDoubleTypeInContext(self.context);
                    let raw_value = LLVMConstReal(double_type, *f);
                    if is_object {
                        return self.box_raw_value_f64(raw_value);
                    }
                    return raw_value;
                }
                LiteralKind::Bool(b) => {
                    let bool_type = LLVMInt1TypeInContext(self.context);
                    let raw_value = LLVMConstInt(bool_type,*b as u64, 0);
                    if is_object {
                        return self.box_raw_value_bool(raw_value);
                    }
                    return raw_value;
                }
                _ => panic!("not implemented!")
            }
        }
    }

    // After we compute the result with raw value, box it again
    fn box_raw_value_i32(&self, raw_value: LLVMValueRef) -> LLVMValueRef {
        let runtime_func = self.get_runtime_alloc_i32_literal();
        unsafe {
            LLVMBuildCall2(self.builder, 
                    LLVMGetCalledFunctionType(runtime_func), 
                      runtime_func, 
                    [raw_value].as_mut_ptr(), 
                 1, 
                    "box".as_ptr() as *const c_char)
        }
    }

    // After we compute the result with raw value, box it again
    fn box_raw_value_f64(&self, raw_value: LLVMValueRef) -> LLVMValueRef {
        let runtime_func = self.get_runtime_alloc_f64_literal();
        unsafe {
            LLVMBuildCall2(self.builder, 
                    LLVMGetCalledFunctionType(runtime_func), 
                        runtime_func, 
                    [raw_value].as_mut_ptr(), 
                    1, 
                    "box".as_ptr() as *const c_char)
        }
    }

    // After we compute the result with raw value, box it again
    fn box_raw_value_bool(&self, raw_value: LLVMValueRef) -> LLVMValueRef {
        let runtime_func = self.get_runtime_alloc_bool_literal();
        unsafe {
            LLVMBuildCall2(self.builder, 
                    LLVMGetCalledFunctionType(runtime_func), 
                      runtime_func, 
                    [raw_value].as_mut_ptr(), 
                 1, 
                    "box".as_ptr() as *const c_char)
        }
    }

    fn load_stack_slot_variable(&self, var: VarRef) -> LLVMValueRef {
        let alloca = self
            .function
            .as_ref()
            .unwrap()
            .symbol_value_map
            .get(&var)
            .unwrap()
            .clone();
        unsafe {
            LLVMBuildLoad2(self.builder, 
                            self.object_ptr_type(), 
                            alloca, 
                            "".as_ptr() as *const i8)
        }
    }

    fn store_fn_variables(&mut self, slots: &SlotMap<VarRef, Var>, variables: &[VarRef]) {
        unsafe {
            self.set_insert_point_before_terminator(self.function
                                                            .as_ref()
                                                            .unwrap()
                                                            .alloca_block);
            for var_ref in variables.iter().copied() {
                let var = slots.get(var_ref).unwrap();
                let object_ty = self.object_ptr_type();

                let stack_slot = LLVMBuildAlloca(self.builder, object_ty, var.name.as_ptr() as *const c_char);
                self.function
                        .as_mut()
                        .unwrap()
                        .symbol_value_map
                        .insert(var_ref, stack_slot);
                    
            }

        }   
    }

    fn create_arithematic(&self, 
        op: &BinOp, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef
    {
        unsafe {
            let res = match op {
                BinOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, "".as_ptr() as *const i8),
                BinOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, "".as_ptr() as *const i8),
                BinOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, "".as_ptr() as *const i8),
                BinOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, "".as_ptr() as *const i8),
                BinOp::Mod => LLVMBuildSRem(self.builder, lhs, rhs, "".as_ptr() as *const i8),
                _ => panic!("not implemented")
            };
            res
        }
    }

    fn set_insert_point_before_terminator(&self, bb: LLVMBasicBlockRef) {
        unsafe {
            let TermInst = LLVMGetBasicBlockTerminator(bb);
            if TermInst.is_null() {
                LLVMPositionBuilderAtEnd(self.builder, bb);
            } else {
                LLVMPositionBuilderBefore(self.builder, TermInst)
            }
        }
    }

    fn raw_primitive_type(&self, mptype: PrimitiveType) -> LLVMTypeRef {
        unsafe {
           match mptype {
                PrimitiveType::Int => LLVMInt32TypeInContext(self.context),
                PrimitiveType::Bool => LLVMInt1TypeInContext(self.context),
                PrimitiveType::Float => LLVMDoubleTypeInContext(self.context),
                PrimitiveType::Unit => LLVMVoidTypeInContext(self.context),
                _ => panic!("not implemented"),
            }
        }
    }

    fn object_ptr_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMPointerTypeInContext(self.context, 0)
        }
    }

    fn create_fn_type(&self, 
                      ret_ty: LLVMTypeRef, 
                      params_ty: *mut LLVMTypeRef, 
                      param_count: u32,
                      is_var_arg: bool) -> LLVMTypeRef 
    {
        unsafe {
            LLVMFunctionType(ret_ty, 
                                params_ty, 
                                param_count,
                                 is_var_arg as LLVMBool)
        }
    }
}