use rpds::HashTrieMap;
use std::collections::HashMap;

type SymTable<K, V> = HashTrieMap<K, V>;

use crate::utils::types::{Enum, PrimitiveType, Type};
use crate::ast::ast::*;

pub trait TypedAst {
	fn ty(&self) -> Type;
}

#[derive(Debug)]
pub struct TypedModule {
	pub enum_table: SymTable<String, Enum>,
	pub func_table: SymTable<String, Type>,
	pub func_defs: Vec<TypedFunc>,

}

#[derive(Debug)]
pub struct TypedFunc {
	pub name: String,
	pub params: Vec<(String, Type)>,
	pub return_ty: Type,
	pub body: TypedBlock,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedBlock {
	pub exprs: Vec<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub enum TypedLiteral {
	Int(i32),
	Float(f64),
	Bool(bool),
	Char(char),
	Str(String),
}

impl TypedAst for TypedLiteral {
	fn ty(&self) -> Type {
		match self {
			TypedLiteral::Int(_) => Type::Primitive(PrimitiveType::Int),
			TypedLiteral::Float(_) => Type::Primitive(PrimitiveType::Float),
			TypedLiteral::Bool(_) => Type::Primitive(PrimitiveType::Bool),
			TypedLiteral::Char(_) => Type::Primitive(PrimitiveType::Char),
			TypedLiteral::Str(_) => Type::Primitive(PrimitiveType::String),
		}
	}
}

#[derive(Debug)]
pub struct TypedVariable {
	pub name: String,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedTuple {
	pub elements: Vec<TypedExpr>,
	pub ty: Type
}

#[derive(Debug)]
pub struct TypedArray {
	pub elements: Vec<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedBinOp {
	pub op: BinOp,
	pub lhs: Box<TypedExpr>,
	pub rhs: Box<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedUnOp {
	pub op: UnOp,
	pub rhs: Box<TypedExpr>,
	pub ty: Type,
}
#[derive(Debug)]
pub enum TypedElse {
	Else(TypedBlock),
	ElseIf(Box<TypedIf>),
	None,
}
impl TypedAst for TypedElse {
	fn ty(&self) -> Type {
		match self {
			TypedElse::Else(block) => block.ty.clone(),
			TypedElse::ElseIf(r#if) => r#if.ty.clone(),
			TypedElse::None => Type::Primitive(PrimitiveType::Unit)
		}
	}
}

#[derive(Debug)]
pub struct TypedIf {
	pub cond: Box<TypedExpr>,
	pub then: TypedBlock,
	pub els: TypedElse,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedCall {
	pub func: Box<TypedExpr>,
	pub args: Vec<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedIndex {
	pub array: Box<TypedExpr>,
	pub index: Box<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub enum TypedExprFields {
	UnnamedFields(Vec<TypedExpr>),
	NamedFields(HashMap<String, Option<TypedExpr>>),
}
#[derive(Debug)]
pub enum TypedPatternFields {
	UnnamedFields(Vec<TypedPattern>),
	NamedFields(HashMap<String, (Type, Option<TypedPattern>)>),
}

#[derive(Debug)]
pub enum TypedPattern {
	Lit(TypedLiteral),
	Var(TypedVariable),
	Tuple(Vec<TypedPattern>),
	Ctor {
		ty_name: String,
		name: String,
		fields: Option<TypedPatternFields>,
	}
}

impl TypedPattern {
	pub fn is_var(&self) -> bool {
		match self {
			TypedPattern::Var(_) => true,
			_ => false,
		}
	}
}

#[derive(Debug)]
pub struct TypedCtor {
	pub ty_name: String,
	pub name: String,
	pub fields: Option<TypedExprFields>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedMatchArm {
	pub pattern: TypedPattern,
	pub expr: Box<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedMatch {
	pub expr: Box<TypedExpr>,
	pub arms: Vec<TypedMatchArm>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedClosure {
	pub args_name: Vec<String>,
	pub args_ty: Vec<Type>,
	pub return_ty: Type,
	pub body: Box<TypedExpr>,
	pub ty: Type,
}

#[derive(Debug)]
pub struct TypedLet {
	pub name: String,
	pub ty: Type,
	pub rhs: Box<TypedExpr>,
}

#[derive(Debug)]
pub struct TypedWhile {
	pub cond: Box<TypedExpr>,
	pub body: TypedBlock,
}

#[derive(Debug)]
pub struct TypedFor {
	pub var: String,
	pub start: Box<TypedExpr>,
	pub end: Box<TypedExpr>,
	pub body: TypedBlock,
}

#[derive(Debug)]
pub struct TypedReturn {
	pub expr: Box<Option<TypedExpr>>,
}

#[derive(Debug)]
pub struct TypedAssign {
	pub name: String,
	pub rhs: Box<TypedExpr>,
}


#[derive(Debug)]
pub enum TypedExpr {
	Literal(TypedLiteral),
	Variable(TypedVariable),
	Tuple(TypedTuple),
	Array(TypedArray),
	Block(TypedBlock),
	BinOp(TypedBinOp),
	UnOp(TypedUnOp),
	If(TypedIf),
	Call(TypedCall),
	Index(TypedIndex),
	Ctor(TypedCtor),
	Match(TypedMatch),
	Closure(TypedClosure),
	Let(TypedLet),
	While(TypedWhile),
	For(TypedFor),
	Return(TypedReturn),
	Break,
	Continue,
	Assign(TypedAssign),
}

impl TypedAst for TypedExpr {
	fn ty(&self) -> Type {
		match self {
			TypedExpr::Literal(lit) => lit.ty(),
			TypedExpr::Variable(var) => var.ty.clone(),
			TypedExpr::Tuple(tuple) => tuple.ty.clone(),
			TypedExpr::Array(array) => array.ty.clone(),
			TypedExpr::Block(block) => block.ty.clone(),
			TypedExpr::BinOp(binop) => binop.ty.clone(),
			TypedExpr::UnOp(unop) => unop.ty.clone(),
			TypedExpr::If(if_) => if_.ty.clone(),
			TypedExpr::Call(call) => call.ty.clone(),
			TypedExpr::Index(index) => index.ty.clone(),
			TypedExpr::Ctor(ctor) => ctor.ty.clone(),
			TypedExpr::Match(match_) => match_.ty.clone(),
			TypedExpr::Closure(closure) => closure.ty.clone(),
			TypedExpr::Let(_) => Type::Primitive(PrimitiveType::Unit),
			TypedExpr::While(_) => Type::Primitive(PrimitiveType::Unit),
			TypedExpr::For(_) => Type::Primitive(PrimitiveType::Unit),
			TypedExpr::Return(_) => Type::Primitive(PrimitiveType::Unit),
			TypedExpr::Break => Type::Primitive(PrimitiveType::Unit),
			TypedExpr::Continue => Type::Primitive(PrimitiveType::Unit),
			TypedExpr::Assign(_) => Type::Primitive(PrimitiveType::Unit),
		}
	}
}

