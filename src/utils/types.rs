use std::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
	Int,
	Float,
	Bool,
	Char,
	String,
	Unit,
}

#[derive(Debug)]
pub enum FieldsType {
	UnnamedFields(Vec<Type>),
	NamedFields(HashMap<String, Type>),
}

#[derive(Debug)]
pub struct Enum {
	pub name: String,
	pub ctors: HashMap<String, Option<FieldsType>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
	Primitive(PrimitiveType),
	Named(String),
	Func(Vec<Type>, Box<Type>),
	Tuple(Vec<Type>),
	Array(Box<Type>),
}

impl fmt::Display for PrimitiveType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			PrimitiveType::Int => write!(f, "int"),
			PrimitiveType::Float => write!(f, "float"),
			PrimitiveType::Bool => write!(f, "bool"),
			PrimitiveType::Char => write!(f, "char"),
			PrimitiveType::String => write!(f, "string"),
			PrimitiveType::Unit => write!(f, "unit"),
		}
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Type::Primitive(ty) => write!(f, "{}", ty),
			Type::Named(name) => write!(f, "{}", name),
			Type::Func(params, ret) => {
				write!(f, "fn(")?;
				for (i, param) in params.iter().enumerate() {
					if i != 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", param)?;
				}
				write!(f, ") -> {}", ret)
			},
			Type::Tuple(types) => {
				write!(f, "(")?;
				for (i, ty) in types.iter().enumerate() {
					if i != 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", ty)?;
				}
				write!(f, ")")
			},
			Type::Array(ty) => write!(f, "[{}]", ty),
		}
	}
}
