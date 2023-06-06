use std::collections::HashMap;
use std::hash::Hash;
use std::ptr::null_mut;
use std::sync::Arc;
use std::thread::current;

use crate::midend::mir::*;
use crate::midend::typed_ast::LiteralKind;
use crate::utils::type_context::*;

use libc::*;

use llvm_sys::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::analysis::*;
use rpds::HashTrieMap;
use slotmap::SlotMap;

use std::mem::MaybeUninit;

use super::runtime::RuntimeLibrary;
use crate::utils::to_c_str;

type SymTable<K, V> = HashTrieMap<K, V>;
type BinOp = crate::ast::BinOp;
type UnOp = crate::ast::UnOp;

#[derive(Debug)]
pub struct FunctionEmissionState<'a> {
    var_map: &'a SlotMap<VarRef, Var>,
    // represent a stack slot pointer of a local variables (ref),
    // or a intermediate result (raw value)
    symbol_value_map: HashMap<VarRef, (bool, LLVMValueRef)>,
    deref_type_map: HashMap<VarRef, LLVMTypeRef>,

    block_map: HashMap<BlockRef, LLVMBasicBlockRef>,
    alloca_block: LLVMBasicBlockRef,
    entry_block: LLVMBasicBlockRef,
    exit_block: LLVMBasicBlockRef,
    ret_ptr: LLVMValueRef,
    function_value: LLVMValueRef
}

pub struct CodeGen<'a> {
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub function: Option<FunctionEmissionState<'a>>,
    pub function_type_map: HashMap<LLVMValueRef, LLVMTypeRef>,

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
            let module = LLVMModuleCreateWithNameInContext(to_c_str("Serge").as_ptr() as *const i8, context);
            let builder = LLVMCreateBuilderInContext(context);
            CodeGen {
                context,
                module,
                builder,
                function: None,
                function_type_map: HashMap::new(),
                name_ctx: name_ctx.clone(),
                ty_ctx: ty_ctx.clone(),
            }
        }
    }
}

impl<'a> CodeGen<'a> {

    // crate raw LLVM top-level variable, represent a expression, object type.
    fn create_rvalue(&self, value: &Rvalue) -> LLVMValueRef {
        let Rvalue {typ, val} = value;
        match val.as_ref() {
            RvalueEnum::BinaryOperator(op, lhs , rhs) => {
                let lhs_raw = self.create_raw_operand(lhs);
                let rhs_raw = self.create_raw_operand(rhs);
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Div | BinOp::Mul | BinOp::Mod => self.create_arithematic(op, typ, lhs_raw, rhs_raw),
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                        let typ = if lhs.typ == self.ty_ctx.get_f64() 
                            || rhs.typ == self.ty_ctx.get_f64() 
                        {
                            self.ty_ctx.get_f64()
                        } else {
                            self.ty_ctx.get_i32()
                        };
                        self.create_compare(op, &typ, lhs_raw, rhs_raw)
                    }
                    _ => panic!("not implemented!"),
                }
            }
            RvalueEnum::Call(name, operands) => {
                let mut args = operands.iter()
                            .map(|operand| self.create_operand(operand))
                            .collect::<Vec<_>>();
                println!("create call {}", name);
                unsafe {
                    let func = LLVMGetNamedFunction(self.module, to_c_str(name).as_ptr());
                    LLVMBuildCall2(self.builder, 
                                    self.function_type_map.get(&func).unwrap().clone(),
                                    func, 
                                    args.as_mut_ptr(), 
                                    args.len() as u32, 
                                    to_c_str("").as_ptr())
                }
            }
            RvalueEnum::Index(container, index) => {
                let container_type = self.ty_ctx.get_type_by_typeref(container.typ);
                match container_type {
                    Type::Array(_) => {
                        let runtime_fn = self.get_runtime_array_read_index();
                        let container_var = self.create_operand(container);
                        let raw_index = self.create_raw_operand(index);
                        let mut args = [container_var, raw_index];
                        unsafe {
                            LLVMBuildCall2(self.builder,
                                          self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                          runtime_fn,
                                          args.as_mut_ptr(),
                                          2,
                                          to_c_str("array_index").as_ptr())
                        }
                    },
                    Type::Tuple(_) => {
                        let runtime_fn = self.get_runtime_extract_tuple_field();
                        let container_var = self.create_operand(container);
                        let raw_index = self.create_raw_operand(index);
                        let mut args = [container_var, raw_index];
                        unsafe {
                            LLVMBuildCall2(self.builder,
                                          self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                          runtime_fn,
                                          args.as_mut_ptr(),
                                          2,
                                          to_c_str("tuple_index").as_ptr())
                        }
                    },
                    _ => { panic!("only array and tuple allow indexing!"); }
                }
            }
            RvalueEnum::Operand(operand) => self.create_operand(operand),
            RvalueEnum::MakeTuple(operands) => {
                let mut args = operands.iter()
                    .map(|operand| self.create_operand(operand))
                    .collect::<Vec<_>>();
                let n = self.create_raw_int(args.len() as i32);
                args.insert(0, n);
                let runtime_fn = self.get_runtime_make_tuple();
                unsafe {
                    LLVMBuildCall2(self.builder,
                                   self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                   runtime_fn,
                                   args.as_mut_ptr(),
                                   args.len() as u32,
                                   to_c_str("make_tuple").as_ptr())
                }
            },
            RvalueEnum::Construct(tag, operands) => {
                let mut args = operands.iter()
                    .map(|operand| self.create_operand(operand))
                    .collect::<Vec<_>>();
                let n = self.create_raw_int(args.len() as i32);
                args.insert(0, n);
                let runtime_fn = self.get_runtime_make_tuple();
                let tuple = unsafe {
                    LLVMBuildCall2(self.builder,
                                   self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                   runtime_fn,
                                   args.as_mut_ptr(),
                                   args.len() as u32,
                                   to_c_str("make_tuple").as_ptr())
                };
                let tag = self.create_liteal(&LiteralKind::Int(*tag as i32), false);
                let runtime_fn = self.get_runtime_make_enum();
                unsafe {
                    LLVMBuildCall2(self.builder,
                                   self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                   runtime_fn,
                                   [tag, tuple].as_mut_ptr(),
                                   2,
                                   to_c_str("make_enum").as_ptr())
                }
            },
            RvalueEnum::ExtractTupleField(tuple, index) => {
                let runtime_fn = self.get_runtime_extract_tuple_field();
                let tuple_var = self.create_operand(tuple);
                let raw_index = self.create_raw_int(*index as i32);
                let mut args = [tuple_var, raw_index];
                unsafe {
                    LLVMBuildCall2(self.builder,
                                   self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                   runtime_fn,
                                   args.as_mut_ptr(),
                                   2,
                                   to_c_str("extract_tuple_field").as_ptr())
                }
            },
            RvalueEnum::ExtractEnumField(r#enum, field) => {
                let runtime_fn = self.get_runtime_extract_enum_field();
                let enum_var = self.create_operand(r#enum);
                let raw_field = self.create_raw_int(*field as i32);
                let mut args = [enum_var, raw_field];
                unsafe {
                    LLVMBuildCall2(self.builder,
                                   self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                   runtime_fn,
                                   args.as_mut_ptr(),
                                   2,
                                   to_c_str("extract_enum_field").as_ptr())
                }
            },
            RvalueEnum::ExtractEnumTag(r#enum) => {
                let runtime_fn = self.get_runtime_extract_enum_tag();
                let enum_var = self.create_operand(r#enum);
                let mut args = [enum_var];
                unsafe {
                    LLVMBuildCall2(self.builder,
                                   self.function_type_map.get(&runtime_fn).unwrap().clone(),
                                   runtime_fn,
                                   args.as_mut_ptr(),
                                   1,
                                   to_c_str("extract_enum_tag").as_ptr())
                }
            }
            _ => { panic!("not implemented1") }
        }
    }

    // Create LLVM top-level variables, value type scope.
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
                let rhs = self.create_rvalue(rhs); // may boxed value
                // statement stored to left value (a local variable)
                if let Some(lhs) = stmt.left { 
                    let ptr = self.get_top_level_variable_addr(lhs);
                    unsafe {
                        LLVMBuildStore(self.builder, rhs, ptr);
                    }
                }
            }
        }
    }

    fn create_terminators(&self, blocks: &SlotMap<BlockRef, Block>) {
        let function_state = self.function.as_ref().unwrap();
        let ret_ptr = function_state.ret_ptr;
        let exit_bb = function_state.exit_block;
        let block_map = &function_state.block_map;

        for (block_ref, block) in blocks {
            let current_bb = block_map.get(&block_ref).unwrap().clone();
            // insert at the end of bb.
            self.set_insert_point_before_terminator(current_bb);
            match &block.terminator {
                Terminator::Branch(cond, then, or) => {
                    // should be bool type
                    let cond = self.create_raw_operand(cond);
                    unsafe {
                        LLVMBuildCondBr(self.builder, 
                                        cond, 
                                        block_map.get(then).unwrap().clone(), 
                                        block_map.get(or).unwrap().clone());
                    }
                }
                Terminator::Jump(blockref) => {
                    let target_bb = block_map.get(blockref).unwrap().clone();
                    unsafe {
                        LLVMBuildBr(self.builder, target_bb);
                    }
                }
                Terminator::Return => {
                    unsafe {
                        // jump to exit block, load ret val
                        let current_bb = block_map.get(&block_ref).unwrap().clone();
                        if current_bb != exit_bb {
                            LLVMBuildBr(self.builder, exit_bb);
                        }
                        if !ret_ptr.is_null() {
                            let ret_val = LLVMBuildLoad2(self.builder, 
                                            self.object_ptr_type(),
                                                ret_ptr,
                                            to_c_str("ret_val.ptr").as_ptr());
                            LLVMBuildRet(self.builder, ret_val);
                        } else {
                            LLVMBuildRetVoid(self.builder);
                        }
                    }
                }
                _ => { panic!("not implemented"); }
            }
        }
    }

    fn create_function(&mut self, mfn: &'a Func) {
        unsafe {
            println!("create function: {}", mfn.name);
            let mfn_type = self.ty_ctx.get_type_by_typeref(mfn.typ);
            if let Type::Callable {params: params, ret: ret} = mfn_type {
                let ret_ty = if ret == self.ty_ctx.get_unit() {
                    self.void_type()
                } else {
                    self.object_ptr_type()
                };
                let mut params_ty = params.iter().map(|&x| self.object_ptr_type()).collect::<Vec<_>>();
                let fn_type = self.create_fn_type(ret_ty, 
                                                                 params_ty.as_mut_ptr(),
                                                                params_ty.len() as u32, 
                                                                false);
                
                let func = LLVMAddFunction(self.module, to_c_str(&mfn.name).as_ptr(), fn_type);
                self.function_type_map.insert(func, fn_type);

                // start generate basicblocks
                // create alloca basic block
                let alloca_bb = LLVMAppendBasicBlock(func, to_c_str("alloca").as_ptr());
                // set insert point
                self.set_insert_point_before_terminator(alloca_bb);

                // emit all BB
                let mut block_map = HashMap::new();
                for (blockref, block) in mfn.blocks.iter() {
                    let name = block.name.clone();
                    let bb = LLVMAppendBasicBlock(func, to_c_str(&name).as_ptr() );   
                    block_map.insert(blockref, bb);
                }
                // ret_ptr
                let mut ret_ptr = null_mut();

                // jump from alloca bb to the first code bb.
                let entry_bb = block_map.get(&mfn.entry).unwrap().clone();
                LLVMBuildBr(self.builder, entry_bb);

                let exit_bb = block_map.get(&mfn.exit).unwrap().clone();

                self.function = Some(FunctionEmissionState { 
                    var_map: & mfn.variables,
                    symbol_value_map: HashMap::new(), 
                    deref_type_map: HashMap::new(),
                    block_map: block_map,
                    alloca_block: alloca_bb, 
                    entry_block: entry_bb,
                    exit_block: exit_bb,
                    ret_ptr: ret_ptr, 
                    function_value: func
                });

                // store arguments, local variables
                self.store_local_variables(&mfn.variables, &mfn.locals);
                self.store_argument(&mfn.variables, &mfn.params);
                // alloca slots for temp variables
                self.store_temp_variable(&mfn.variables, &mfn.temporaries);

                // get return value slot
                if let Some(ret_var) = &mfn.return_value {
                    let (_, slot) = self.function.
                                        as_ref().
                                        unwrap().
                                        symbol_value_map.
                                        get(ret_var).
                                        unwrap();
                    self.function.
                        as_mut().
                        unwrap().
                        ret_ptr = slot.clone();
                }

                // generate instructions.
                self.create_stmts(&mfn.blocks);
                // generate teminators.
                self.create_terminators(&mfn.blocks);

                // verify check
                LLVMVerifyFunction(func, LLVMVerifierFailureAction::LLVMPrintMessageAction);
            }
        }
    }

    pub fn create_module(&mut self, mir: &'a MIR) {
        self.insert_runtime_function_declaration();
        for funcdef in mir.module.iter() {
            self.create_function(funcdef);
        }
        unsafe {
            let mut err_string = MaybeUninit::uninit();
            LLVMVerifyModule(self.module, 
                            LLVMVerifierFailureAction::LLVMPrintMessageAction, 
                            err_string.as_mut_ptr());
        }
    }

    // create object type LLVM Value
    fn create_operand(&self, operand: &Operand) -> LLVMValueRef {
        let Operand {typ , val} = operand;
        match val.as_ref() {
            OperandEnum::Imm(_) => { panic!("You should use create_raw_operand to get Imm!")  }
            OperandEnum::Literal(literal) => self.create_liteal(literal, true),
            OperandEnum::Var(var) => { 
                let (is_object, llvm_var) = self.load_stack_slot_variable(*var);
                llvm_var
            }
        }
    }

    // In some cases, e.g. do comparison, we must expose raw data.
    // We will call runtime function here.
    fn create_raw_operand(&self, operand: &Operand) -> LLVMValueRef {
        let Operand {typ, val} = operand;
        unsafe {
            match val.as_ref() {
                OperandEnum::Imm(number) => {
                    let int_type = LLVMInt32TypeInContext(self.context);
                    LLVMConstInt(int_type, *number as u64, 0)
                }
                OperandEnum::Var(var) => {
                    // load from stack slot => LLVM top level value
                    let (is_object, var_value) = self.load_stack_slot_variable(*var);
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
                            self.function_type_map.get(&runtime_func).unwrap().clone(),
                            runtime_func, 
                            [var_value].as_mut_ptr(), 
                            1, 
                            to_c_str("extract_i32").as_ptr())
                    }
                    if typ == self.ty_ctx.get_primitive("f64") {
                        let runtime_func = self.get_runtime_extract_f64();
                        return LLVMBuildCall2(self.builder, 
                            self.function_type_map.get(&runtime_func).unwrap().clone(),
                            runtime_func, 
                            [var_value].as_mut_ptr(), 
                            1, 
                            to_c_str("extract_f64").as_ptr())
                    }
                    if typ == self.ty_ctx.get_primitive("bool") {
                        let runtime_func = self.get_runtime_extract_bool();
                        return LLVMBuildCall2(self.builder, 
                            self.function_type_map.get(&runtime_func).unwrap().clone(),
                            runtime_func, 
                            [var_value].as_mut_ptr(), 
                            1, 
                            to_c_str("extract_bool").as_ptr())
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
                    if !is_object {
                        return raw_value;
                    }
                    return self.box_raw_value_i32(raw_value);
                }
                LiteralKind::Float(f) => {
                    let double_type = LLVMDoubleTypeInContext(self.context);
                    let raw_value = LLVMConstReal(double_type, *f);
                    if !is_object {
                        return raw_value;
                    }
                    return self.box_raw_value_f64(raw_value);
                }
                LiteralKind::Bool(b) => {
                    let bool_type = LLVMInt1TypeInContext(self.context);
                    let raw_value = LLVMConstInt(bool_type,*b as u64, 0);
                    if !is_object {
                        return raw_value;
                    }
                    return self.box_raw_value_bool(raw_value);
                }
                _ => panic!("not implemented!")
            }
        }
    }

    fn create_raw_int(&self, val: i32) -> LLVMValueRef {
        unsafe {
            let int_type = LLVMInt32TypeInContext(self.context);
            let raw_value = LLVMConstInt(int_type, val as u64, 0);
            raw_value
        }
    }

    // After we compute the result with raw value, box it again
    fn box_raw_value_i32(&self, raw_value: LLVMValueRef) -> LLVMValueRef {
        let runtime_func = self.get_runtime_alloc_i32_literal();
        unsafe {
            LLVMBuildCall2(self.builder, 
                        self.function_type_map.get(&runtime_func).unwrap().clone(), 
                      runtime_func, 
                    [raw_value].as_mut_ptr(), 
                 1, 
                    to_c_str("box_i32").as_ptr())
        }
    }

    // After we compute the result with raw value, box it again
    fn box_raw_value_f64(&self, raw_value: LLVMValueRef) -> LLVMValueRef {
        let runtime_func = self.get_runtime_alloc_f64_literal();
        unsafe {
            LLVMBuildCall2(self.builder, 
                        self.function_type_map.get(&runtime_func).unwrap().clone(), 
                        runtime_func, 
                    [raw_value].as_mut_ptr(), 
                    1, 
                    to_c_str("box_f64").as_ptr())
        }
    }

    // After we compute the result with raw value, box it again
    fn box_raw_value_bool(&self, raw_value: LLVMValueRef) -> LLVMValueRef {
        let runtime_func = self.get_runtime_alloc_bool_literal();
        unsafe {
            LLVMBuildCall2(self.builder,    
                        self.function_type_map.get(&runtime_func).unwrap().clone(), 
                      runtime_func, 
                    [raw_value].as_mut_ptr(), 
                 1, 
                    to_c_str("box_bool").as_ptr())
        }
    }

    fn get_top_level_variable_addr(&self, var: VarRef) -> LLVMValueRef {
        let (is_object, slot) = self.function
                        .as_ref()
                        .unwrap()
                        .symbol_value_map
                        .get(&var)
                        .unwrap()
                        .clone();
        slot
    }

    // Only used for load local variable, and return a LLVM top-level variable.
    fn load_stack_slot_variable(&self, var: VarRef) -> (bool, LLVMValueRef) {
        let state = self.function.as_ref().unwrap();
        let (is_object, alloca) = state
            .symbol_value_map
            .get(&var)
            .unwrap()
            .clone();
        unsafe {
            let top_level_var =  LLVMBuildLoad2(self.builder, 
                                            self.object_ptr_type(), 
                                alloca, 
                                to_c_str("").as_ptr());
            (is_object, top_level_var)
        }
    }


    // Only used for save local variale and argument in `alloca_bb`.
    // All the local variable (i.e. has a name in user code) is object type.
    fn store_local_variables(&mut self, 
                          slots: &SlotMap<VarRef, Var>, 
                          variables: &[VarRef]) 
    {
        self.set_insert_point_before_terminator(self.function
                                                        .as_ref()
                                                        .unwrap()
                                                        .alloca_block);
        for var_ref in variables.iter().copied() {
            let var = slots.get(var_ref).unwrap();
            let object_ty = self.object_ptr_type();
            unsafe {
                let state = self.function.as_mut().unwrap();
                let stack_slot = LLVMBuildAlloca(self.builder, object_ty, to_c_str(&var.name).as_ptr());
                state
                    .symbol_value_map
                    .insert(var_ref, (true, stack_slot));
                state
                    .deref_type_map
                    .insert(var_ref, object_ty);
            }
        }
    }   

    fn store_argument(&mut self, 
                      slots: &SlotMap<VarRef, Var>,
                      args: &Vec<VarRef>)
    {
        self.set_insert_point_before_terminator(self.function
            .as_ref()
            .unwrap()
            .alloca_block);

        for (args_index, var_ref) in args.iter().enumerate() {
            let var = slots.get(*var_ref).unwrap();
            let object_ty = self.object_ptr_type();
            unsafe {
                let state = self.function.as_mut().unwrap();
                let stack_slot = LLVMBuildAlloca(self.builder, object_ty, to_c_str(&var.name).as_ptr());
                let arg_value = LLVMGetParam(state.function_value, args_index as u32);
                LLVMBuildStore(self.builder, arg_value, stack_slot);
                state
                    .symbol_value_map
                    .insert(*var_ref, (true, stack_slot));
            }
        }       
    }
    
    // Only used for deciding intermediate temp variables type, and create store
    fn store_temp_variable(&mut self, 
                           slots: &SlotMap<VarRef, Var>,
                           variables: &[VarRef])
    {
        self.set_insert_point_before_terminator(self.function
                                                    .as_ref()
                                                    .unwrap()
                                                    .alloca_block);
        for var_ref in variables.iter().copied() {
            let var = slots.get(var_ref).unwrap();
            let typ = self.ty_ctx.get_type_by_typeref(var.typ);
            
            // only use raw type for int, float and bool
            let (is_object, llvm_type) = {
                if let Type::Primitive(mptype) = typ {
                    match mptype {
                        PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Bool => (false, self.object_ptr_type()),
                        _ => (true, self.object_ptr_type()),
                    }
                } else {
                    (true, self.object_ptr_type())
                }
            };
            unsafe {
                let state = self.function.as_mut().unwrap();
                let stack_slot = LLVMBuildAlloca(self.builder, 
                                                                llvm_type, 
                                                                to_c_str(&var.name).as_ptr());
                state
                    .symbol_value_map
                    .insert(var_ref, (is_object, stack_slot));
            }
        }
    }


    fn create_arithematic(&self, 
        op: &BinOp, typref: &TypeRef, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef
    {
        if *typref == self.ty_ctx.get_i32() {
            unsafe {
                return self.box_raw_value_i32(match op {
                    BinOp::Add => LLVMBuildAdd(self.builder, lhs, rhs, to_c_str("add").as_ptr()),
                    BinOp::Sub => LLVMBuildSub(self.builder, lhs, rhs, to_c_str("sub").as_ptr()),
                    BinOp::Mul => LLVMBuildMul(self.builder, lhs, rhs, to_c_str("mul").as_ptr()),
                    BinOp::Div => LLVMBuildSDiv(self.builder, lhs, rhs, to_c_str("div").as_ptr()),
                    BinOp::Mod => LLVMBuildSRem(self.builder, lhs, rhs, to_c_str("rem").as_ptr()),
                    _ => panic!("not implemented")
                });
            }
        }
        if *typref == self.ty_ctx.get_f64() {
            unsafe {
                return self.box_raw_value_f64(match op {
                    BinOp::Add => LLVMBuildFAdd(self.builder, lhs, rhs, to_c_str("add").as_ptr()),
                    BinOp::Sub => LLVMBuildFSub(self.builder, lhs, rhs, to_c_str("sub").as_ptr()),
                    BinOp::Mul => LLVMBuildFMul(self.builder, lhs, rhs, to_c_str("mul").as_ptr()),
                    BinOp::Div => LLVMBuildFDiv(self.builder, lhs, rhs, to_c_str("div").as_ptr()),
                    BinOp::Mod => LLVMBuildFRem(self.builder, lhs, rhs, to_c_str("rem").as_ptr()),
                    _ => panic!("not implemented")
                });
            }
        }
        panic!("Unknown type in create_arithematic!");
    }

    fn create_compare(&self, 
        op: &BinOp, typref: &TypeRef, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef
    {
        if *typref == self.ty_ctx.get_i32() {
            let pred = match op {
                BinOp::Eq => LLVMIntPredicate::LLVMIntEQ,
                BinOp::Neq => LLVMIntPredicate::LLVMIntNE,
                BinOp::Lt => LLVMIntPredicate::LLVMIntSLT,
                BinOp::Gt => LLVMIntPredicate::LLVMIntSGT,
                BinOp::Lte => LLVMIntPredicate::LLVMIntSLE,
                BinOp::Gte => LLVMIntPredicate::LLVMIntSGE,
                _ => panic!("not implemented")
            };
            unsafe { 
                return self.box_raw_value_bool(
                    LLVMBuildICmp(self.builder, pred, lhs, rhs, to_c_str("icmp").as_ptr()));
            }
        }
        if *typref == self.ty_ctx.get_f64() {
            let pred = match op {
                BinOp::Eq => LLVMRealPredicate::LLVMRealUEQ,
                BinOp::Neq => LLVMRealPredicate::LLVMRealUNE,
                BinOp::Lt => LLVMRealPredicate::LLVMRealULT,
                BinOp::Gt => LLVMRealPredicate::LLVMRealUGT,
                BinOp::Lte => LLVMRealPredicate::LLVMRealULE,
                BinOp::Gte => LLVMRealPredicate::LLVMRealUGE,
                _ => panic!("not implemented")
            };
            unsafe { 
                return self.box_raw_value_bool(
                    LLVMBuildFCmp(self.builder, pred, lhs, rhs, to_c_str("fcmp").as_ptr()));
            }
        }
        panic!("Unknown type in create_compare!");
    }

    fn set_insert_point_before_terminator(&self, bb: LLVMBasicBlockRef) {
        unsafe {
            let term_inst = LLVMGetBasicBlockTerminator(bb);
            if term_inst.is_null() {
                LLVMPositionBuilderAtEnd(self.builder, bb);
            } else {
                LLVMPositionBuilderBefore(self.builder, term_inst)
            }
        }
    }

    fn get_llvm_type(&self, typeref: TypeRef) -> LLVMTypeRef {
        let typ = self.ty_ctx.get_type_by_typeref(typeref);
        match typ {
            Type::Primitive(mptype) => self.raw_primitive_type(mptype),
            _ => self.object_ptr_type()
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

    fn void_type(&self) -> LLVMTypeRef {
        unsafe {
            LLVMVoidTypeInContext(self.context)
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
