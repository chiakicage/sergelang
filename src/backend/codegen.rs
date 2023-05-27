use std::collections::HashMap;
use std::ptr::null_mut;

use crate::midend::mir::*;
use crate::utils::type_context::*;

use libc::*;

use llvm_sys::LLVMValueKind;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use rpds::HashTrieMap;
use slotmap::SlotMap;

type SymTable<K, V> = HashTrieMap<K, V>;


#[derive(Debug)]
pub struct FunctionEmissionState {
    symbol_value_map: HashMap<VarRef, LLVMValueRef>,
    alloca_block: LLVMBasicBlockRef,
    ret_ptr: LLVMValueRef,
}

pub struct CodeGen{
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub function: Option<FunctionEmissionState>,

    pub name_ctx: SymTable<String, TypeRef>,
    pub ty_ctx: TypeContext
}

impl CodeGen {
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
                context: context,
                module: module,
                builder: builder,
                function: None,
                name_ctx: name_ctx.clone(),
                ty_ctx: ty_ctx.clone(),
            }
        }
    }
}

impl CodeGen {
    fn create_stmt(&mut self, sblocks: &SlotMap<BlockRef, Block>) {

    }

    fn create_terminators(&mut self, blocks: &SlotMap<BlockRef, Block>) {

    }

    fn create_function(&mut self, mfn: &Func) {
        unsafe {
            let mfn_name = mfn.name.as_ptr() as *const i8;
            let mfn_type = self.ty_ctx.get_type_by_typeref(mfn.typ);
            if let Type::Callable {params: params, ret: ret} = mfn_type {
                let ret_ty = self.object_ptr_type();
                let mut params_ty = params.iter().map(|&x| self.object_ptr_type()).collect::<Vec<_>>();
                let fn_type = self.create_fn_type(ret_ty, &mut params_ty, params_ty.len() as u32, false);
                
                let func = LLVMAddFunction(self.module, mfn_name, fn_type);

                // start generate basicblocks
                // create alloca basic block
                let alloca_bb = LLVMAppendBasicBlock(func, "alloca".as_ptr() as *const c_char);
                // set insert point
                self.set_insert_point_before_terminator(alloca_bb);
                // create ret value basic block
                let ret_ptr = LLVMBuildAlloca(self.builder, self.object_ptr_type(), "ret_val.ptr".as_ptr() as *const c_char);

                // emit all BB
                for (blockref, block) in mfn.blocks {
                    let name = block.name.clone();
                    let bb = LLVMAppendBasicBlock(func, name.as_ptr() as *const c_char);   
                }
                
                self.function = Some(FunctionEmissionState { 
                    symbol_value_map: HashMap::new(), 
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

    fn store_fn_variables(&mut self, slots: &SlotMap<VarRef, Var>, variables: &[VarRef]) {
        unsafe {
            self.set_insert_point_before_terminator(self.function.unwrap().alloca_block);
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
                      params_ty: &mut Vec<LLVMTypeRef>, 
                      param_count: u32,
                      is_var_arg: bool) -> LLVMTypeRef 
    {
        unsafe {
            LLVMFunctionType(ret_ty, params_ty.as_mut_ptr(), param_count, is_var_arg)
        }
    }
}