use super::ast::*;
use crate::utils::error::Error;

#[derive(Debug)]
pub enum PrimitiveType {
	Int,
	Float,
	Bool,
	Char,
	String,
	Unit,
}

#[derive(Debug)]
pub enum Type {
	Primitive(PrimitiveType),
	Named(String),
	Func(Vec<Type>, Box<Type>),
	Tuple(Vec<Type>),
	Array(Box<Type>),
}

pub fn expr_type_check<'src>(expr: Expr<'src>) -> Result<Type, Error> {
	// Ok(Type::Primitive(PrimitiveType::Int))
	Err(Error::custom((1..2).into(), "Not implemented yet"))
}

