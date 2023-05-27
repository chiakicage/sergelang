/// The basic design of middle level IR - MIR, follows the MIR of rust
use slotmap::{new_key_type, SlotMap};
use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Write;
use std::{panic, vec};

use crate::ast::ast::{BinOp, UnOp};
use crate::midend::typed_ast::*;
use crate::utils::type_context::*;

use rpds::HashTrieMap;

type SymTable<K, V> = HashTrieMap<K, V>;

type Literal = TypedLiteral;

new_key_type! {
    pub struct VarRef;
    pub struct BlockRef;
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub typ: TypeRef,
    pub blocks: SlotMap<BlockRef, Block>,
    pub variables: SlotMap<VarRef, Var>,

    pub params: Vec<VarRef>,
    pub captured: Vec<VarRef>,

    // corresponds to MIR Locals
    pub locals: Vec<VarRef>,
    // Intermediate Rvalues
    pub temporaries: Vec<VarRef>,

    pub return_value: Option<VarRef>,

    pub entry: BlockRef,
    pub exit: BlockRef,
    pub panic: BlockRef,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Var {
    pub name: String,
    pub typ: TypeRef,
}

// MIR Basic Block
#[derive(Debug, Clone)]
pub struct Block {
    pub name: String,
    pub stmts: Vec<Stmt>,
    pub terminator: Terminator,
}

#[derive(Debug, Default, Clone)]
pub struct Stmt {
    pub left: Option<VarRef>,
    pub right: Option<Rvalue>,
}

// MIR Terminator
#[derive(Debug, Clone)]
pub enum Terminator {
    Select(VarRef, Vec<(usize, BlockRef)>, BlockRef),
    Branch(Operand, BlockRef, BlockRef),
    Jump(BlockRef),
    Return,
    Panic,
}

#[derive(Debug, Clone)]
pub struct Operand {
    pub typ: TypeRef,
    pub val: OperandEnum,
}

#[derive(Debug, Clone)]
pub enum OperandEnum {
    Imm(i32),
    Literal(Literal),
    Var(VarRef),
}

#[derive(Debug, Clone)]
pub enum Rvalue {
    BinaryOperator(BinOp, Operand, Operand),
    UnaryOperator(UnOp, Operand, Operand),
    Call(String, Vec<Operand>),
}

#[derive(Debug, Default, Clone)]
pub struct MIR {
    pub ty_ctx: TypeContext,
    pub name_ctx: SymTable<String, TypeRef>,
    pub module: Vec<Func>,
}

impl MIR {
    pub fn create_from_typed_ast(typed_ast: &TypedModule) -> Self {
        let ty_ctx = typed_ast.ty_ctx.clone();
        let name_ctx = typed_ast.func_table.clone();
        let mut mir = MIR {
            ty_ctx,
            name_ctx,
            module: Vec::new(),
        };
        for func in &typed_ast.func_defs {
            mir.module
                .push(mir.build_func(func, mir.name_ctx.clone(), SymTable::new()));
        }
        mir
    }
    fn build_func(
        &self,
        func: &TypedFunc,
        ext_name_ctx: SymTable<String, TypeRef>,
        name_var_map: SymTable<String, VarRef>,
    ) -> Func {
        let mut blocks = SlotMap::with_key();
        let exit = blocks.insert(Block {
            name: "exit".to_string(),
            stmts: Vec::new(),
            terminator: Terminator::Return,
        });
        let entry = blocks.insert(Block {
            name: "entry".to_string(),
            stmts: Vec::new(),
            terminator: Terminator::Jump(exit),
        });
        let panic = blocks.insert(Block {
            name: "panic".to_string(),
            stmts: Vec::new(),
            terminator: Terminator::Panic,
        });

        let mut variables = SlotMap::with_key();
        let mut name_ctx = ext_name_ctx.clone();
        let mut name_var_map = name_var_map.clone();
        let params = func
            .params
            .iter()
            .map(|param| {
                let var = Var {
                    name: param.0.clone(),
                    typ: param.1,
                };
                let var = variables.insert(var);
                name_ctx = name_ctx.insert(param.0.clone(), param.1);
                name_var_map = name_var_map.insert(param.0.clone(), var);
                var
            })
            .collect::<Vec<_>>();

        let mut func = Func {
            name: func.name.clone(),
            typ: func.ty,
            blocks,
            variables,
            params,
            captured: Vec::new(),
            locals: Vec::new(),
            temporaries: Vec::new(),
            return_value: None,
            entry,
            exit,
            panic,
        };
        func
    }
}
