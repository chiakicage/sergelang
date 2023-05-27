/// The basic design of middle level IR - MIR, follows the MIR of rust

use slotmap::{new_key_type, SlotMap};
use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Write;
use std::{panic, vec};

use crate::midend::typed_ast::*;
use crate::utils::type_context::*;

use rpds::HashTrieMap;

type SymTable<K, V> = HashTrieMap<K, V>;

type Literal = TypedLiteral;
type BinOp = TypedBinOp;
type UnOp = TypedUnOp;

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

    // corresponds to MIR Locals
    pub locals: Vec<VarRef>, 
    // Intermediate Rvalues
    pub temporaries: Vec<VarRef>, 

    pub return_value: Option<VarRef>, 

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
struct Stmt {
    pub left: Option<VarRef>,
    pub right: Option<Rvalue>
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
    pub val: Box<OperandEnum>,
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
    Call(String, Vec<Operand>)
}



#[derive(Debug, Default, Clone)]
pub struct MIR {
    pub ty_ctx: TypeContext,
    pub name_ctx: SymTable<String, TypeRef>,
    pub module: Vec<Func>
}
