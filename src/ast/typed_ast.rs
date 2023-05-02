use super::ast::*;
use crate::utils::error::{Error, Span, Spanned};
use crate::utils::types::{Enum, FieldsType, PrimitiveType, Type};
use rpds::HashTrieMap;
use std::collections::HashMap;

type SymTable<K, V> = HashTrieMap<K, V>;
// type EnumTable = HashMap<String, Enum>;

pub fn convert_type_ref(ty: &TypeRef, ty_table: &SymTable<String, Enum>) -> Result<Type, Error> {
	match ty {
		TypeRef::Named((s, span)) => {
			let ty = match *s {
				"int" => Type::Primitive(PrimitiveType::Int),
				"float" => Type::Primitive(PrimitiveType::Float),
				"bool" => Type::Primitive(PrimitiveType::Bool),
				"string" => Type::Primitive(PrimitiveType::String),
				"char" => Type::Primitive(PrimitiveType::Char),
				name => {
					if ty_table.contains_key(name) {
						Type::Named(name.to_string())
					} else {
						return Err(Error::custom(*span, format!("undefined type {}", name)));
					}
				}
			};
			Ok(ty)
		},
		TypeRef::Func(params, ret) => {
			let mut param_tys = Vec::new();
			for (t, _) in params {
				param_tys.push(convert_type_ref(t, ty_table)?);
			}
			let ret = Box::new(convert_type_ref(&ret.0, ty_table)?);
			Ok(Type::Func(param_tys, ret))
		},
		TypeRef::Tuple(tys) => {
			let mut ret_tys = Vec::new();
			for (t, _) in tys {
				ret_tys.push(convert_type_ref(t, ty_table)?);
			}
			Ok(Type::Tuple(ret_tys))
		},
		TypeRef::Array(ty) => {
			let ty = Box::new(convert_type_ref(&ty.0, ty_table)?);
			Ok(Type::Array(ty))
		},
	}
}

pub fn expr_type_check<'src>(
    expr: &Spanned<Expr<'src>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
) -> Result<Type, Error> {
    let (expr, span) = expr;
	let span = *span;
    match expr {
        Expr::Lit(lit) => match lit {
            Literal::Int(_) => Ok(Type::Primitive(PrimitiveType::Int)),
            Literal::Float(_) => Ok(Type::Primitive(PrimitiveType::Float)),
            Literal::Bool(_) => Ok(Type::Primitive(PrimitiveType::Bool)),
            Literal::Str(_) => Ok(Type::Primitive(PrimitiveType::String)),
            Literal::Char(_) => Ok(Type::Primitive(PrimitiveType::Char)),
        },
        Expr::Var((name, span)) => {
            if let Some(ty) = sym_table.get(*name) {
                Ok(ty.clone())
            } else {
                Err(Error::custom(*span, format!("undefined variable {}", name)))
            }
        }
        Expr::Tuple(exprs) => {
            let mut types = Vec::new();
            for expr in exprs {
                types.push(expr_type_check(expr, sym_table, ty_table)?);
            }
            Ok(Type::Tuple(types))
        }
        Expr::Array(exprs) => {
            if exprs.is_empty() {
                return Err(Error::custom(span, "empty array is not allowed"));
            }
            let ty = expr_type_check(&exprs[0], sym_table, ty_table)?;
            for expr in exprs {
                if ty != expr_type_check(expr, sym_table, ty_table)? {
                    return Err(Error::custom(
                        span,
                        "array elements must have the same type",
                    ));
                }
            }
            Ok(Type::Array(Box::new(ty)))
        }
        Expr::Block(block) => block_type_check(block, sym_table, ty_table),
        Expr::BinOpExpr { lhs, op, rhs } => {
            let ty_lhs = expr_type_check(lhs, sym_table, ty_table)?;
            let ty_rhs = expr_type_check(rhs, sym_table, ty_table)?;

            match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    match (&ty_lhs, &ty_rhs) {
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Ok(Type::Primitive(PrimitiveType::Int)),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Ok(Type::Primitive(PrimitiveType::Float)),
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Ok(Type::Primitive(PrimitiveType::Float)),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Ok(Type::Primitive(PrimitiveType::Float)),
                        (_, _) => Err(Error::custom(
                            span,
                            format!(
                                "invalid arithmetic operation between {} and {}",
                                ty_lhs, ty_rhs
                            ),
                        )),
                    }
                }
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    match (&ty_lhs, &ty_rhs) {
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Ok(Type::Primitive(PrimitiveType::Bool)),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Ok(Type::Primitive(PrimitiveType::Bool)),
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Ok(Type::Primitive(PrimitiveType::Bool)),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Ok(Type::Primitive(PrimitiveType::Bool)),
                        (_, _) => Err(Error::custom(
                            span,
                            format!(
                                "invalid comparison operation between {} and {}",
                                ty_lhs, ty_rhs
                            ),
                        )),
                    }
                }
                BinOp::And | BinOp::Or => match (&ty_lhs, &ty_rhs) {
                    (
                        Type::Primitive(PrimitiveType::Bool),
                        Type::Primitive(PrimitiveType::Bool),
                    ) => Ok(Type::Primitive(PrimitiveType::Bool)),
                    (_, _) => Err(Error::custom(
                        span,
                        format!(
                            "invalid logical operation between {} and {}",
                            ty_lhs, ty_rhs
                        ),
                    )),
                },
            }
        }
        Expr::UnOpExpr { op, rhs } => {
            let ty_rhs = expr_type_check(rhs, sym_table, ty_table)?;
            match op {
                UnOp::Neg => match ty_rhs {
                    Type::Primitive(PrimitiveType::Int) => Ok(Type::Primitive(PrimitiveType::Int)),
                    Type::Primitive(PrimitiveType::Float) => {
                        Ok(Type::Primitive(PrimitiveType::Float))
                    }
                    _ => Err(Error::custom(
                        span,
                        format!("invalid negation operation on {}", ty_rhs),
                    )),
                },
                UnOp::Not => match ty_rhs {
                    Type::Primitive(PrimitiveType::Bool) => {
                        Ok(Type::Primitive(PrimitiveType::Bool))
                    }
                    _ => Err(Error::custom(
                        span,
                        format!("invalid not operation on {}", ty_rhs),
                    )),
                },
                _ => unimplemented!(),
            }
        }
        Expr::If { cond, then, els } => {
            let ty_cond = expr_type_check(cond, sym_table, ty_table)?;
            let ty_then = block_type_check(then, sym_table, ty_table)?;
            if let Some(els) = els {
                let ty_else = block_type_check(els, sym_table, ty_table)?;
                if ty_cond != Type::Primitive(PrimitiveType::Bool) {
                    return Err(Error::custom(
                        span,
                        format!("invalid condition type {}", ty_cond),
                    ));
                }
                if ty_then != ty_else {
                    return Err(Error::custom(
                        span,
                        format!("then and else must have the same type"),
                    ));
                }
                Ok(ty_then)
            } else {
                if ty_cond != Type::Primitive(PrimitiveType::Bool) {
                    return Err(Error::custom(
                        span,
                        format!("invalid condition type {}", ty_cond),
                    ));
                }
                Ok(Type::Primitive(PrimitiveType::Unit))
            }
        }
        Expr::Call { func, args } => {
            let ty_func = expr_type_check(func, sym_table, ty_table)?;
            match ty_func {
                Type::Func(params, ret) => {
                    if params.len() != args.len() {
                        return Err(Error::custom(
                            span,
                            format!(
                                "invalid number of arguments, expected {}, got {}",
                                params.len(),
                                args.len()
                            ),
                        ));
                    }
                    for (param, arg) in params.iter().zip(args.iter()) {
                        if param != &expr_type_check(arg, sym_table, ty_table)? {
                            return Err(Error::custom(
                                arg.1,
                                format!(
                                    "invalid argument type, expected {}, got {}",
                                    param,
                                    expr_type_check(arg, sym_table, ty_table)?
                                ),
                            ));
                        }
                    }
                    Ok(*ret)
                }
                _ => Err(Error::custom(
                    span,
                    format!("invalid function type {}", ty_func),
                )),
            }
        }
        Expr::Index { array, index } => {
            let ty_array = expr_type_check(array, sym_table, ty_table)?;
            let ty_index = expr_type_check(index, sym_table, ty_table)?;
            match ty_array {
                Type::Array(ty) => {
                    if ty_index != Type::Primitive(PrimitiveType::Int) {
                        return Err(Error::custom(
                            index.1,
                            format!("invalid index type {}", ty_index),
                        ));
                    }
                    Ok(*ty)
                }
                _ => Err(Error::custom(
                    array.1,
                    format!("invalid array type {}", ty_array),
                )),
            }
        }
        Expr::Ctor {
            ty_name,
            name,
            fields,
        } => {
            let ty = ty_table
                .get(ty_name.0)
                .ok_or_else(|| Error::custom(span, format!("type {} not found", ty_name.0)))?;
            let ctor_fields = ty
                .ctors
                .get(name.0)
                .ok_or_else(|| Error::custom(span, format!("constructor {} not found", name.0)))?;
            if let Some(ctor_fields) = ctor_fields {
                match ctor_fields {
                    FieldsType::NamedFields(ctor_fields) => {
                        if let Some(ExprFields::NamedFields(fields)) = fields {
                            if ctor_fields.len() != fields.len() {
                                return Err(Error::custom(
                                    span,
                                    format!(
                                        "invalid number of fields, expected {}, got {}",
                                        ctor_fields.len(),
                                        fields.len()
                                    ),
                                ));
                            }
                            for (name, val) in fields {
                                if let Some(ty) = ctor_fields.get(name.0) {
                                    if let Some(val) = val {
                                        let ty_val = expr_type_check(val, sym_table, ty_table)?;
                                        if *ty != ty_val {
                                            return Err(Error::custom(
                                                val.1,
                                                format!(
                                                    "invalid field type, expected {}, got {}",
                                                    ty, ty_val
                                                ),
                                            ));
                                        }
                                    } else {
                                        let ty_val = sym_table.get(name.0).ok_or_else(|| {
                                            Error::custom(
                                                name.1,
                                                format!("variable {} not found", name.0),
                                            )
                                        })?;
                                        if ty != ty_val {
                                            return Err(Error::custom(
                                                name.1,
                                                format!(
                                                    "invalid field type, expected {}, got {}",
                                                    ty, ty_val
                                                ),
                                            ));
                                        }
                                    }
                                } else {
                                    return Err(Error::custom(
                                        name.1,
                                        format!("invalid field name {}", name.0),
                                    ));
                                }
                            }
                            Ok(Type::Named(ty_name.0.to_string()))
                        } else {
                            Err(Error::custom(span, format!("expected named fields")))
                        }
                    }
                    FieldsType::NamelessFields(ctor_fields) => {
                        if let Some(ExprFields::NamelessFields(fields)) = fields {
                            if ctor_fields.len() != fields.len() {
                                return Err(Error::custom(
                                    span,
                                    format!(
                                        "invalid number of fields, expected {}, got {}",
                                        ctor_fields.len(),
                                        fields.len()
                                    ),
                                ));
                            }
                            for (ty, field) in ctor_fields.iter().zip(fields.iter()) {
                                let ty_val = expr_type_check(field, sym_table, ty_table)?;
                                if ty != &ty_val {
                                    return Err(Error::custom(
                                        field.1,
                                        format!(
                                            "invalid field type, expected {}, got {}",
                                            ty, ty_val
                                        ),
                                    ));
                                }
                            }

                            Ok(Type::Named(ty_name.0.to_string()))
                        } else {
                            Err(Error::custom(span, format!("expected nameless fields")))
                        }
                    }
                }
            } else {
				match fields {
					Some(_) => Err(Error::custom(span, format!("expected no fields"))),
					None => Ok(Type::Named(ty_name.0.to_string())),
				}
            }
        },
        Expr::Match { expr, arms } => {
			let ty_expr = expr_type_check(expr, sym_table, ty_table)?;
			let mut ty = None;
			for (arm, span) in arms {
				let sym_table = pattern_type_check(&arm.pattern, &ty_expr, sym_table, ty_table)?;
				let ty_arm = expr_type_check(&arm.expr, &sym_table, ty_table)?;
				if let Some(ty) = ty.clone() {
					if ty != ty_arm {
						return Err(Error::custom(
							arm.expr.1,
							format!("invalid arm type, expected {}, got {}", ty, ty_arm),
						));
					}
				} else {
					ty = Some(ty_arm);
				}
			}
			if let Some(ty) = ty {
				Ok(ty)
			} else {
				Err(Error::custom(span, format!("expected at least one arm")))
			}
		},
		Expr::Closure { args, return_ty, body} => {
			let mut sym_table = sym_table.clone();
            let mut args_tys = Vec::new();
			for (name, ty) in args {
				let ty = convert_type_ref(&ty.0, ty_table)?;
                args_tys.push(ty.clone());
				sym_table = sym_table.insert(name.0.to_string(), ty.clone());
			}
			let ty_body = block_type_check(body, &sym_table, ty_table)?;
			if let Some(return_ty) = return_ty {
				let return_ty = convert_type_ref(&return_ty.0, ty_table)?;
				if ty_body != return_ty {
					return Err(Error::custom(
						body.1,
						format!("invalid return type, expected {}, got {}", return_ty, ty_body),
					));
				}
			} else {
				if ty_body != Type::Primitive(PrimitiveType::Unit) {
					return Err(Error::custom(
						body.1,
						format!("expected no return type"),
					));
				}
				
			}
			Ok(Type::Func (
				args_tys,
				Box::new(ty_body),
			))
		},
    }
}

pub fn pattern_type_check<'src>(
    pattern: &Spanned<Pattern<'src>>,
    ty: &Type,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
) -> Result<SymTable<String, Type>, Error> {
    let (pattern, span) = pattern;
	let span = *span;
    let mut sym_table = sym_table.clone();
    match pattern {
        Pattern::Lit(lit) => {
            let ty_pat = match lit {
                Literal::Int(_) => Type::Primitive(PrimitiveType::Int),
                Literal::Float(_) => Type::Primitive(PrimitiveType::Float),
                Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool),
                Literal::Str(_) => Type::Primitive(PrimitiveType::String),
                Literal::Char(_) => Type::Primitive(PrimitiveType::Char),
            };
            if ty_pat != *ty {
                return Err(Error::custom(
                    span,
                    format!("invalid pattern type, expected {}, got {}", ty, ty_pat),
                ));
            }
            Ok(sym_table)
        }
        Pattern::Var((name, _)) => {
            if *name != "_" {
                sym_table = sym_table.insert(name.to_string(), ty.clone());
            }
            Ok(sym_table)
        }
        Pattern::Tuple(pats) => {
            let tys = match ty {
                Type::Tuple(tys) => tys,
                _ => {
                    return Err(Error::custom(
                        span,
                        format!("invalid pattern type, expected tuple, got {}", ty),
                    ))
                }
            };
            if tys.len() != pats.len() {
                return Err(Error::custom(
                    span,
                    format!(
                        "invalid pattern type, expected tuple of size {}, got {}",
                        tys.len(),
                        pats.len()
                    ),
                ));
            }
            for (ty, pat) in tys.iter().zip(pats.iter()) {
                sym_table = pattern_type_check(pat, ty, &sym_table, ty_table)?;
            }
            Ok(sym_table)
        }
        Pattern::Ctor {
            ty_name,
            name,
            fields,
        } => {
            if let Type::Named(ty) = ty {
                if ty_name.0 != ty {
                    return Err(Error::custom(
                        span,
                        format!("invalid pattern type, expected {}, got {}", ty, ty_name.0),
                    ));
                }
            } else {
                return Err(Error::custom(
                    span,
                    format!("invalid pattern type, expected {}, got {}", ty, ty_name.0),
                ));
            }
            let ty = ty_table
                .get(ty_name.0)
                .ok_or_else(|| Error::custom(span, format!("enum {} not found", ty_name.0)))?;
            let ty_fields = ty
                .ctors
                .get(name.0)
                .ok_or_else(|| Error::custom(span, format!("ctor {} not found in enum", name.0)))?;
            if let Some(ty_fields) = ty_fields {
                match ty_fields {
                    FieldsType::NamelessFields(ty_fields) => {
                        if let Some(PatternFields::NamelessFields(fields)) = fields {
                            if ty_fields.len() != fields.len() {
                                return Err(Error::custom(
                                    span,
                                    format!(
                                        "invalid number of fields, expected {}, got {}",
                                        ty_fields.len(),
                                        fields.len()
                                    ),
                                ));
                            }
                            for (ty, field) in ty_fields.iter().zip(fields.iter()) {
                                sym_table = pattern_type_check(field, ty, &sym_table, ty_table)?;
                            }
                            Ok(sym_table)
                        } else {
                            Err(Error::custom(span, format!("expected nameless fields")))
                        }
                    }
                    FieldsType::NamedFields(ty_fields) => {
                        if let Some(PatternFields::NamedFields(fields)) = fields {
                            if ty_fields.len() != fields.len() {
                                return Err(Error::custom(
                                    span,
                                    format!(
                                        "invalid number of fields, expected {}, got {}",
                                        ty_fields.len(),
                                        fields.len()
                                    ),
                                ));
                            }
                            for (name, pattern) in fields {
								if name.0 != "_" {
									let ty = ty_fields.get(name.0).ok_or_else(|| {
										Error::custom(
											name.1,
											format!("field {} not found in enum", name.0),
										)
									})?;
									if let Some(pattern) = pattern {
										sym_table =
											pattern_type_check(pattern, ty, &sym_table, ty_table)?;
									} else {
										sym_table = sym_table.insert(name.0.to_string(), ty.clone());
									}
								}
                            }
                            Ok(sym_table)
                        } else {
                            Err(Error::custom(span, format!("expected named fields")))
                        }
                    }
                }
            } else {
				match fields {
					Some(_) => Err(Error::custom(span, format!("expected no fields"))),
					None => Ok(sym_table),
				}
            }
        }
    }
}

pub fn stmt_type_check<'src>(
    stmt: &Spanned<Stmt<'src>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
) -> Result<SymTable<String, Type>, Error> {
    Ok(sym_table.clone())
}

pub fn block_type_check<'src>(
    block: &Spanned<Block<'src>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
) -> Result<Type, Error> {
    let (block, span) = block;
    let mut sym_table = sym_table.clone();
    if let Some(stmts) = &block.stmts {
        for stmt in stmts {
            sym_table = stmt_type_check(stmt, &sym_table, ty_table)?;
        }
    }
    if let Some(expr) = &block.return_value {
        expr_type_check(expr, &sym_table, ty_table)
    } else {
        Ok(Type::Primitive(PrimitiveType::Unit))
    }
}

// pub fn
