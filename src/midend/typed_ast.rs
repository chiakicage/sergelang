use crate::ast::ast::*;
use crate::utils::error::{Error, Span, Spanned};
use crate::utils::type_context::{Enum, FieldsType, PrimitiveType, Type, TypeContext, TypeRef};
use rpds::HashTrieMap;
use std::collections::{HashMap, HashSet};

type SymTable<K, V> = HashTrieMap<K, V>;

#[derive(Debug)]
pub struct TypedModule {
    pub ty_ctx: TypeContext,
    pub func_table: SymTable<String, TypeRef>,
    pub func_defs: Vec<TypedFunc>,
}

#[derive(Debug)]
pub struct TypedFunc {
    pub name: String,
    pub params: Vec<(String, TypeRef)>,
    pub return_ty: TypeRef,
    pub body: TypedBlock,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedBlock {
    pub exprs: Vec<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int(i32),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
}

#[derive(Debug, Clone)]
pub struct TypedLiteral {
    pub kind: LiteralKind,
    pub ty: TypeRef,
}

#[derive(Debug, Clone)]
pub struct TypedVariable {
    pub name: String,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedTuple {
    pub elements: Vec<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedArray {
    pub elements: Vec<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedBinOp {
    pub op: BinOp,
    pub lhs: Box<TypedExpr>,
    pub rhs: Box<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedUnOp {
    pub op: UnOp,
    pub rhs: Box<TypedExpr>,
    pub ty: TypeRef,
}
#[derive(Debug)]
pub enum ElseKind {
    Else(TypedBlock),
    ElseIf(Box<TypedIf>),
    None,
}

#[derive(Debug)]
pub struct TypedElse {
    pub kind: ElseKind,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedIf {
    pub cond: Box<TypedExpr>,
    pub then: TypedBlock,
    pub els: TypedElse,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedCall {
    pub func: Box<TypedExpr>,
    pub args: Vec<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedIndex {
    pub array: Box<TypedExpr>,
    pub index: Box<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub enum TypedExprFields {
    UnnamedFields(Vec<TypedExpr>),
    NamedFields(HashMap<String, Option<TypedExpr>>),
}
#[derive(Debug, Clone)]
pub enum TypedPatternFields {
    UnnamedFields(Vec<TypedPattern>),
    NamedFields(HashMap<String, (Type, Option<TypedPattern>)>),
}

#[derive(Debug, Clone)]
pub enum TypedPattern {
    Lit(TypedLiteral),
    Var(TypedVariable),
    Tuple(Vec<TypedPattern>),
    Ctor {
        ty_name: String,
        name: String,
        fields: Option<TypedPatternFields>,
    },
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
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub expr: Box<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedMatch {
    pub expr: Box<TypedExpr>,
    pub arms: Vec<TypedMatchArm>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedClosure {
    pub args_name: Vec<String>,
    pub args_ty: Vec<TypeRef>,
    pub return_ty: TypeRef,
    pub body: Box<TypedExpr>,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct TypedLet {
    pub name: String,
    pub ty: TypeRef,
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
pub enum ExprKind {
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

#[derive(Debug)]
pub struct TypedExpr {
    pub kind: ExprKind,
    pub ty: TypeRef,
}

impl TypedModule {
    pub fn create_from_ast<'src>(module: &Spanned<Module<'src>>) -> Result<TypedModule, Error> {
        let (module, _) = module;

        let mut typed_ast = TypedModule {
            ty_ctx: Default::default(),
            func_table: SymTable::new(),
            func_defs: Vec::new(),
        };

        let enum_decls = module
            .decls
            .iter()
            .filter_map(|(decl, _)| match decl {
                Decl::EnumDecl { name, ctors } => Some((name, ctors)),
                _ => None,
            })
            .collect::<Vec<_>>();

        let func_decls = module
            .decls
            .iter()
            .filter_map(|(decl, _)| match decl {
                Decl::FuncDecl {
                    name,
                    args,
                    return_ty,
                    body,
                } => Some((name, args, return_ty, body)),
                _ => None,
            })
            .collect::<Vec<_>>();

        typed_ast.resolve_all_enums(enum_decls)?;
        typed_ast.resolve_all_funcs(func_decls)?;

        Ok(typed_ast)
    }
    fn literal_type_check<'src>(&mut self, lit: &Literal<'src>) -> Result<TypedLiteral, Error> {
        let kind = match lit {
            Literal::Int(i) => LiteralKind::Int(*i),
            Literal::Float(f) => LiteralKind::Float(*f),
            Literal::Bool(b) => LiteralKind::Bool(*b),
            Literal::Char(c) => LiteralKind::Char(*c),
            Literal::Str(s) => LiteralKind::Str(s.to_string()),
        };
        let ty = match lit {
            Literal::Int(_) => self.ty_ctx.get_primitive("i32"),
            Literal::Float(_) => self.ty_ctx.get_primitive("f64"),
            Literal::Bool(_) => self.ty_ctx.get_primitive("bool"),
            Literal::Char(_) => self.ty_ctx.get_primitive("char"),
            Literal::Str(_) => self.ty_ctx.get_primitive("str"),
        };
        Ok(TypedLiteral { kind, ty })
    }

    fn expr_type_check<'src>(
        &mut self,
        expr: &Spanned<Expr<'src>>,
        sym_table: &SymTable<String, TypeRef>,
        return_ty: TypeRef, // return type of the function
        in_loop: bool,
    ) -> Result<TypedExpr, Error> {
        let (expr, span) = expr;
        let span = *span;
        let result = match expr {
            Expr::Lit(lit) => {
                let lit = self.literal_type_check(lit)?;
                Ok(ExprKind::Literal(lit))
            }
            Expr::Var((name, span)) => {
                if let Some(ty) = sym_table.get(*name).copied() {
                    let var = TypedVariable {
                        name: name.to_string(),
                        ty,
                    };
                    Ok(ExprKind::Variable(var))
                } else {
                    Err(Error::custom(*span, format!("undefined variable {}", name)))
                }
            }
            Expr::Tuple(exprs) => {
                let mut tuple = Vec::new();
                let mut types = Vec::new();
                for expr in exprs {
                    let expr = self.expr_type_check(expr, sym_table, return_ty, in_loop)?;
                    types.push(expr.ty);
                    tuple.push(expr);
                }
                let tuple = TypedTuple {
                    elements: tuple,
                    ty: self.ty_ctx.insert_type_or_get(Type::Tuple(types)),
                };
                Ok(ExprKind::Tuple(tuple))
            }
            Expr::Array(exprs) => {
                if exprs.is_empty() {
                    return Err(Error::custom(span, "empty array is not allowed"));
                }

                let ty = self
                    .expr_type_check(&exprs[0], sym_table, return_ty, in_loop)?
                    .ty;
                let mut ty_exprs = Vec::new();
                for expr in exprs {
                    let expr = self.expr_type_check(expr, sym_table, return_ty, in_loop)?;
                    if ty != expr.ty {
                        return Err(Error::custom(
                            span,
                            "array elements must have the same type",
                        ));
                    }
                    ty_exprs.push(expr);
                }
                let array = TypedArray {
                    elements: ty_exprs,
                    ty: self.ty_ctx.insert_type_or_get(Type::Array(ty)),
                };
                Ok(ExprKind::Array(array))
            }
            Expr::Block(block) => Ok(ExprKind::Block(
                self.block_type_check(block, sym_table, return_ty, in_loop)?,
            )),
            Expr::BinOpExpr { lhs, op, rhs } => {
                let ty_lhs = self.expr_type_check(lhs, sym_table, return_ty, in_loop)?;
                let ty_rhs = self.expr_type_check(rhs, sym_table, return_ty, in_loop)?;
                let int_type = self.ty_ctx.get_primitive("i32");
                let float_type = self.ty_ctx.get_primitive("f64");
                let bool_type = self.ty_ctx.get_primitive("bool");
                let ty_lhs_str = self.ty_ctx.typeref_to_string(ty_lhs.ty);
                let ty_rhs_str = self.ty_ctx.typeref_to_string(ty_rhs.ty);
                let ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if ty_lhs.ty != int_type && ty_lhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for lhs of binary operator {:?}: {:?}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != int_type && ty_rhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {:?}: {:?}",
                                    op, ty_rhs_str
                                ),
                            ));
                        }
                        if ty_lhs.ty == float_type || ty_rhs.ty == float_type {
                            float_type
                        } else {
                            int_type
                        }
                    }
                    BinOp::Mod => {
                        if ty_lhs.ty != int_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for lhs of binary operator {:?}: {:?}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != int_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {:?}: {:?}",
                                    op, ty_rhs_str
                                ),
                            ));
                        }
                        int_type
                    }
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                        if ty_lhs.ty != int_type && ty_lhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for lhs of binary operator {:?}: {:?}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != int_type && ty_rhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {:?}: {:?}",
                                    op, ty_rhs_str
                                ),
                            ));
                        }
                        bool_type
                    }
                    BinOp::And | BinOp::Or => {
                        if ty_lhs.ty != bool_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for lhs of binary operator {:?}: {:?}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != bool_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {:?}: {:?}",
                                    op, ty_rhs_str
                                ),
                            ));
                        }
                        bool_type
                    }
                };
                Ok(ExprKind::BinOp(TypedBinOp {
                    lhs: Box::new(ty_lhs),
                    op: op.clone(),
                    rhs: Box::new(ty_rhs),
                    ty,
                }))
            }
            Expr::UnOpExpr { op, rhs } => {
                let ty_rhs = self.expr_type_check(rhs, sym_table, return_ty, in_loop)?;
                let int_type = self.ty_ctx.get_primitive("i32");
                let float_type = self.ty_ctx.get_primitive("f64");
                let bool_type = self.ty_ctx.get_primitive("bool");
                let ty = match op {
                    UnOp::Neg => {
                        if ty_rhs.ty != int_type && ty_rhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of unary operator {:?}: {:?}",
                                    op,
                                    self.ty_ctx.typeref_to_string(ty_rhs.ty)
                                ),
                            ));
                        }
                        ty_rhs.ty
                    }
                    UnOp::Not => {
                        if ty_rhs.ty != bool_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of unary operator {:?}: {:?}",
                                    op,
                                    self.ty_ctx.typeref_to_string(ty_rhs.ty)
                                ),
                            ));
                        }
                        ty_rhs.ty
                    }
                    _ => unimplemented!(),
                };
                Ok(ExprKind::UnOp(TypedUnOp {
                    op: op.clone(),
                    rhs: Box::new(ty_rhs),
                    ty,
                }))
            }

            // Expr::If { cond, then, els } => {
            //     let r#if = if_type_check(cond, then, els, sym_table, ty_table, return_ty)?;
            //     Ok(TypedExpr::If(r#if))
            // }
            Expr::Call { func, args } => {
                let ty_func = self.expr_type_check(func, sym_table, return_ty, in_loop)?;


                
                if let Type::Callable { params, ret } = self.ty_ctx.types[ty_func.ty].clone() {
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
                    let mut ty_args = Vec::new();
                    for (param, arg) in params.iter().zip(args.iter()) {
                        let ty_arg = self.expr_type_check(arg, sym_table, return_ty, in_loop)?;
                        if *param != ty_arg.ty {
                            return Err(Error::custom(
                                arg.1,
                                format!(
                                    "invalid argument type, expected {}, got {}",
                                    self.ty_ctx.typeref_to_string(*param),
                                    self.ty_ctx.typeref_to_string(ty_arg.ty)
                                ),
                            ));
                        }
                        ty_args.push(ty_arg);
                    }
                    Ok(ExprKind::Call(TypedCall {
                        func: Box::new(ty_func),
                        args: ty_args,
                        ty: ret,
                    }))
                } else {
                    Err(Error::custom(
                        func.1,
                        format!("invalid callable type {}", self.ty_ctx.typeref_to_string(ty_func.ty)),
                    ))
                }
            }
            Expr::Index { array, index } => {
                let ty_array = self.expr_type_check(array, sym_table, return_ty, in_loop)?;
                let ty_index = self.expr_type_check(index, sym_table, return_ty, in_loop)?;
                if let Type::Array(ty) = &self.ty_ctx.types[ty_array.ty] {
                    if ty_index.ty != self.ty_ctx.get_primitive("i32") {
                        return Err(Error::custom(
                            index.1,
                            format!("invalid index type {}", self.ty_ctx.typeref_to_string(ty_index.ty)),
                        ));
                    }
                    Ok(ExprKind::Index(TypedIndex {
                        array: Box::new(ty_array),
                        index: Box::new(ty_index),
                        ty: *ty,
                    }))
                } else {
                    Err(Error::custom(
                        array.1,
                        format!("invalid array type {}", self.ty_ctx.typeref_to_string(ty_array.ty)),
                    ))
                }
            }
            /*
            Expr::Ctor {
                ty_name,
                name,
                fields,
            } => {
                let ty = ty_table
                    .get(ty_name.0)
                    .ok_or_else(|| Error::custom(span, format!("type {} not found", ty_name.0)))?;
                let ctor_fields = ty.ctors.get(name.0).ok_or_else(|| {
                    Error::custom(span, format!("constructor {} not found", name.0))
                })?;

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

                                let mut ty_fields = HashMap::new();
                                for (name, val) in fields {
                                    if let Some(ty) = ctor_fields.get(name.0) {
                                        if ty_fields.contains_key(name.0) {
                                            return Err(Error::custom(
                                                name.1,
                                                format!("field {} already set", name.0),
                                            ));
                                        }

                                        if let Some(val) = val {
                                            let ty_val = expr_type_check(
                                                val, sym_table, ty_table, return_ty,
                                            )?;
                                            if *ty != ty_val.ty() {
                                                return Err(Error::custom(
                                                    val.1,
                                                    format!(
                                                        "invalid field type, expected {}, got {}",
                                                        ty,
                                                        ty_val.ty()
                                                    ),
                                                ));
                                            }
                                            ty_fields.insert(name.0.to_string(), Some(ty_val));
                                        } else {
                                            let ty_val =
                                                sym_table.get(name.0).ok_or_else(|| {
                                                    Error::custom(
                                                        name.1,
                                                        format!("variable {} not found", name.0),
                                                    )
                                                })?;
                                            if *ty != *ty_val {
                                                return Err(Error::custom(
                                                    name.1,
                                                    format!(
                                                        "invalid field type, expected {}, got {}",
                                                        ty, ty_val
                                                    ),
                                                ));
                                            }
                                            ty_fields.insert(name.0.to_string(), None);
                                        }
                                    } else {
                                        return Err(Error::custom(
                                            name.1,
                                            format!("invalid field name {}", name.0),
                                        ));
                                    }
                                }
                                let ty_fields = TypedExprFields::NamedFields(ty_fields);
                                Ok(TypedExpr::Ctor(TypedCtor {
                                    ty_name: ty_name.0.to_string(),
                                    name: name.0.to_string(),
                                    fields: Some(ty_fields),
                                    ty: Type::Named(ty_name.0.to_string()),
                                }))
                            } else {
                                Err(Error::custom(span, format!("expected named fields")))
                            }
                        }
                        FieldsType::UnnamedFields(ctor_fields) => {
                            if let Some(ExprFields::UnnamedFields(fields)) = fields {
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
                                let mut ty_fields = Vec::new();
                                for (ty, field) in ctor_fields.iter().zip(fields.iter()) {
                                    let ty_val =
                                        expr_type_check(field, sym_table, ty_table, return_ty)?;
                                    if *ty != ty_val.ty() {
                                        return Err(Error::custom(
                                            field.1,
                                            format!(
                                                "invalid field type, expected {}, got {}",
                                                ty,
                                                ty_val.ty()
                                            ),
                                        ));
                                    }
                                    ty_fields.push(ty_val);
                                }
                                let ty_fields = TypedExprFields::UnnamedFields(ty_fields);
                                Ok(TypedExpr::Ctor(TypedCtor {
                                    ty_name: ty_name.0.to_string(),
                                    name: name.0.to_string(),
                                    fields: Some(ty_fields),
                                    ty: Type::Named(ty_name.0.to_string()),
                                }))
                            } else {
                                Err(Error::custom(span, format!("expected unnamed fields")))
                            }
                        }
                    }
                } else {
                    match fields {
                        Some(_) => Err(Error::custom(span, format!("expected no fields"))),
                        None => Ok(TypedExpr::Ctor(TypedCtor {
                            ty_name: ty_name.0.to_string(),
                            name: name.0.to_string(),
                            fields: None,
                            ty: Type::Named(ty_name.0.to_string()),
                        })),
                    }
                }
            }
            Expr::Match { expr, arms } => {
                let ty_expr = expr_type_check(expr, sym_table, ty_table, return_ty)?;
                match ty_expr.ty() {
                    Type::Primitive(PrimitiveType::Int)
                    | Type::Primitive(PrimitiveType::String)
                    | Type::Primitive(PrimitiveType::Char)
                    | Type::Primitive(PrimitiveType::Bool)
                    | Type::Named(_)
                    | Type::Tuple(_) => {}
                    _ => {
                        return Err(Error::custom(
                            expr.1,
                            format!("can't match type {}", ty_expr.ty()),
                        ));
                    }
                };
                let mut ty = None;
                let mut ty_arms = Vec::new();
                for (arm, _) in arms {
                    let ty_pat = pattern_type_check(&arm.pattern, &ty_expr.ty(), ty_table)?;
                    let new_sym_table = insert_pattern_symbol_binding(&ty_pat, sym_table);
                    let ty_arm = expr_type_check(&arm.expr, &new_sym_table, ty_table, return_ty)?;
                    if let Some(ty) = ty.clone() {
                        if ty != ty_arm.ty() {
                            return Err(Error::custom(
                                arm.expr.1,
                                format!("invalid arm type, expected {}, got {}", ty, ty_arm.ty()),
                            ));
                        }
                    } else {
                        ty = Some(ty_arm.ty());
                    }
                    let ty_arm = TypedMatchArm {
                        pattern: ty_pat,
                        ty: ty_arm.ty(),
                        expr: Box::new(ty_arm),
                    };
                    ty_arms.push(ty_arm);
                }
                if let Some(ty) = ty {
                    let pats = ty_arms.iter().map(|arm| &arm.pattern).collect::<Vec<_>>();
                    if pattern_non_exhaustive_check(&pats, &ty_expr.ty(), ty_table) {
                        return Err(Error::custom(span, format!("pattern not exhaustive")));
                    }
                    Ok(TypedExpr::Match(TypedMatch {
                        expr: Box::new(ty_expr),
                        arms: ty_arms,
                        ty,
                    }))
                } else {
                    Err(Error::custom(span, format!("expected at least one arm")))
                }
            }
            Expr::Closure {
                args,
                return_ty,
                body,
            } => {
                let mut sym_table = sym_table.clone();
                let mut args_ty = Vec::new();
                let mut args_name = Vec::new();
                for (name, ty) in args {
                    let ty = convert_type_str(&ty.0, ty_table)?;
                    args_ty.push(ty.clone());
                    args_name.push(name.0.to_string());
                    sym_table = sym_table.insert(name.0.to_string(), ty.clone());
                }

                let return_ty = match return_ty {
                    Some((ty, _)) => convert_type_str(ty, ty_table)?,
                    None => Type::Primitive(PrimitiveType::Unit),
                };

                let ty_body = expr_type_check(body, &sym_table, ty_table, &return_ty)?;

                if ty_body.ty() != return_ty {
                    return Err(Error::custom(
                        body.1,
                        format!(
                            "invalid return type, expected {}, got {}",
                            return_ty,
                            ty_body.ty()
                        ),
                    ));
                }
                let ty_closure = TypedClosure {
                    args_name,
                    args_ty: args_ty.clone(),
                    return_ty: return_ty.clone(),
                    body: Box::new(ty_body),
                    ty: Type::Func(args_ty, Box::new(return_ty)),
                };

                Ok(TypedExpr::Closure(ty_closure))
            }
            Expr::Let { name, ty, rhs } => {
                let ty = convert_type_str(&ty.0, ty_table)?;
                let ty_rhs = expr_type_check(rhs, sym_table, ty_table, return_ty)?;
                if ty != ty_rhs.ty() {
                    return Err(Error::custom(
                        rhs.1,
                        format!("invalid rhs type, expected {}, got {}", ty, ty_rhs.ty()),
                    ));
                }
                let ty_let = TypedLet {
                    name: name.0.to_string(),
                    ty,
                    rhs: Box::new(ty_rhs),
                };
                Ok(TypedExpr::Let(ty_let))
            }
            Expr::While { cond, body } => {
                let ty_cond = expr_type_check(cond, sym_table, ty_table, return_ty)?;
                if ty_cond.ty() != Type::Primitive(PrimitiveType::Bool) {
                    return Err(Error::custom(
                        cond.1,
                        format!(
                            "invalid condition type, expected {}, got {}",
                            Type::Primitive(PrimitiveType::Bool),
                            ty_cond.ty()
                        ),
                    ));
                }
                let body = match body.0 {
                    Expr::Block(ref block) => block,
                    _ => unreachable!(),
                };
                let ty_body = block_type_check(body, sym_table, ty_table, return_ty)?;
                if ty_body.ty != Type::Primitive(PrimitiveType::Unit) {
                    return Err(Error::custom(
                        body.1,
                        format!("invalid body type, expected no type, got {}", ty_body.ty),
                    ));
                }
                let ty_while = TypedWhile {
                    cond: Box::new(ty_cond),
                    body: ty_body,
                };

                Ok(TypedExpr::While(ty_while))
            }
            Expr::For {
                var,
                start,
                end,
                body,
            } => {
                let ty_start = expr_type_check(start, sym_table, ty_table, return_ty)?;
                let ty_end = expr_type_check(end, sym_table, ty_table, return_ty)?;
                if ty_start.ty() != Type::Primitive(PrimitiveType::Int) {
                    return Err(Error::custom(
                        start.1,
                        format!(
                            "invalid start type, expected {}, got {}",
                            Type::Primitive(PrimitiveType::Int),
                            ty_start.ty()
                        ),
                    ));
                }
                if ty_end.ty() != Type::Primitive(PrimitiveType::Int) {
                    return Err(Error::custom(
                        end.1,
                        format!(
                            "invalid end type, expected {}, got {}",
                            Type::Primitive(PrimitiveType::Int),
                            ty_end.ty()
                        ),
                    ));
                }
                let new_sym_table =
                    sym_table.insert(var.0.to_string(), Type::Primitive(PrimitiveType::Int));
                let body = match body.0 {
                    Expr::Block(ref block) => block,
                    _ => unreachable!(),
                };
                let ty_body = block_type_check(body, &new_sym_table, ty_table, return_ty)?;
                if ty_body.ty != Type::Primitive(PrimitiveType::Unit) {
                    return Err(Error::custom(
                        body.1,
                        format!("invalid body type, expected no type, got {}", ty_body.ty),
                    ));
                }
                let ty_for = TypedFor {
                    var: var.0.to_string(),
                    start: Box::new(ty_start),
                    end: Box::new(ty_end),
                    body: ty_body,
                };
                Ok(TypedExpr::For(ty_for))
            }
            Expr::Return(expr) => {
                let expr = match expr {
                    Some(expr) => Some(expr_type_check(expr, sym_table, ty_table, return_ty)?),
                    None => None,
                };
                let ty_expr = match &expr {
                    Some(expr) => expr.ty(),
                    None => Type::Primitive(PrimitiveType::Unit),
                };

                if ty_expr != *return_ty {
                    return Err(Error::custom(
                        span,
                        format!(
                            "invalid return type, expected {}, got {}",
                            return_ty, ty_expr
                        ),
                    ));
                }
                Ok(TypedExpr::Return(TypedReturn {
                    expr: Box::new(expr),
                }))
            }
            Expr::Break => Ok(TypedExpr::Break),
            Expr::Continue => Ok(TypedExpr::Continue),
            Expr::Assign { name, rhs } => {
                let ty_var = if let Some(ty) = sym_table.get(name.0) {
                    Ok(ty.clone())
                } else {
                    Err(Error::custom(
                        span,
                        format!("undefined variable {}", name.0),
                    ))
                }?;
                let ty_rhs = expr_type_check(rhs, sym_table, ty_table, return_ty)?;
                if ty_var != ty_rhs.ty() {
                    return Err(Error::custom(
                        rhs.1,
                        format!("invalid rhs type, expected {}, got {}", ty_var, ty_rhs.ty()),
                    ));
                }
                Ok(TypedExpr::Assign(TypedAssign {
                    name: name.0.to_string(),
                    rhs: Box::new(ty_rhs),
                }))
            }*/
            _ => todo!(),
        };
        result.map(|expr| {
            let ty = match &expr {
                ExprKind::Literal(lit) => lit.ty,
                ExprKind::Variable(var) => var.ty,
                ExprKind::Tuple(tuple) => tuple.ty,
                ExprKind::Array(array) => array.ty,
                ExprKind::Block(block) => block.ty,
                ExprKind::BinOp(binop) => binop.ty,
                ExprKind::UnOp(unop) => unop.ty,
                ExprKind::If(if_) => if_.ty,
                ExprKind::Call(call) => call.ty,
                ExprKind::Index(index) => index.ty,
                ExprKind::Ctor(ctor) => ctor.ty,
                ExprKind::Match(match_) => match_.ty,
                ExprKind::Closure(closure) => closure.ty,
                ExprKind::Let(_) => self.ty_ctx.get_primitive("unit"),
                ExprKind::While(_) => self.ty_ctx.get_primitive("unit"),
                ExprKind::For(_) => self.ty_ctx.get_primitive("unit"),
                ExprKind::Return(_) => self.ty_ctx.get_primitive("unit"),
                ExprKind::Break => self.ty_ctx.get_primitive("unit"),
                ExprKind::Continue => self.ty_ctx.get_primitive("unit"),
                ExprKind::Assign(_) => self.ty_ctx.get_primitive("unit"),
            };
            TypedExpr { kind: expr, ty }
        })
    }
    fn block_type_check<'src>(
        &mut self,
        block: &Spanned<Block<'src>>,
        sym_table: &SymTable<String, TypeRef>,
        return_ty: TypeRef,
        in_loop: bool,
    ) -> Result<TypedBlock, Error> {
        let mut sym_table = sym_table.clone();
        let mut exprs = Vec::new();
        let mut last_ty = self.ty_ctx.get_primitive("unit");

        for expr in &block.0 .0 {
            let ty_expr = self.expr_type_check(expr, &sym_table, return_ty, in_loop)?;
            match &ty_expr.kind {
                ExprKind::Let(TypedLet { name, ty, .. }) => {
                    sym_table = sym_table.insert(name.clone(), *ty);
                }
                ExprKind::Break | ExprKind::Continue => {
                    if !in_loop {
                        return Err(Error::custom(
                            expr.1,
                            "break/continue outside of loop".to_string(),
                        ));
                    }
                }
                ExprKind::Return(TypedReturn { expr: ty_expr }) => match ty_expr.as_ref() {
                    Some(ty_expr) => {
                        if ty_expr.ty != return_ty {
                            return Err(Error::custom(
                                expr.1,
                                format!(
                                    "invalid return type, expected {}, got {}",
                                    self.ty_ctx.typeref_to_string(return_ty),
                                    self.ty_ctx.typeref_to_string(ty_expr.ty)
                                ),
                            ));
                        }
                    }
                    None => {
                        if return_ty != self.ty_ctx.get_primitive("unit") {
                            return Err(Error::custom(
                                expr.1,
                                format!(
                                    "invalid return type, expected {}, got unit",
                                    self.ty_ctx.typeref_to_string(return_ty),
                                ),
                            ));
                        }
                    }
                },
                _ => {}
            }
            last_ty = ty_expr.ty;
            exprs.push(ty_expr);
        }
        Ok(TypedBlock { exprs, ty: last_ty })
    }
    fn func_type_check<'src>(
        &mut self,
        name: String,
        params: Vec<(String, TypeRef)>,
        return_ty: TypeRef,
        body: &Spanned<Expr<'src>>,
    ) -> Result<TypedFunc, Error> {
        let body = match &body.0 {
            Expr::Block(block) => block,
            _ => unreachable!(),
        };

        let mut sym_table = self.func_table.clone();

        for (name, ty) in &params {
            sym_table = sym_table.insert(name.clone(), *ty);
        }
        let ty_body = self.block_type_check(body, &sym_table, return_ty, false)?;

        if ty_body.ty != return_ty {
            return Err(Error::custom(
                body.1,
                format!(
                    "invalid return type, expected {}, got {}",
                    self.ty_ctx.typeref_to_string(return_ty),
                    self.ty_ctx.typeref_to_string(ty_body.ty)
                ),
            ));
        }

        let ty = self.ty_ctx.func_type(
            params.iter().map(|(_, ty)| *ty).collect::<Vec<_>>(),
            return_ty,
        );
        Ok(TypedFunc {
            name,
            params,
            return_ty,
            ty,
            body: ty_body,
        })
    }

    fn resolve_all_funcs<'src>(
        &mut self,
        func_decls: Vec<(
            &Spanned<&'src str>,
            &Vec<(Spanned<&'src str>, Spanned<TypeStr>)>,
            &Option<Spanned<TypeStr>>,
            &Box<Spanned<Expr>>,
        )>,
    ) -> Result<(), Error> {
        for (name, args, return_ty, body) in &func_decls {
            let func_name = name.0;
            let mut arg_tys = Vec::new();
            for (_, (ty, _)) in *args {
                let ty = self.ty_ctx.convert_type_str(ty)?;
                arg_tys.push(ty);
            }

            let return_ty = match return_ty {
                Some((ty, _)) => self.ty_ctx.convert_type_str(ty)?,
                None => self.ty_ctx.get_primitive("unit"),
            };

            let func_ty = self.ty_ctx.func_type(arg_tys, return_ty);

            if self.func_table.contains_key(func_name) {
                return Err(Error::custom(
                    name.1,
                    format!("function {} already defined", func_name),
                ));
            }

            self.func_table = self.func_table.insert(func_name.to_string(), func_ty);
        }
        for (name, args, return_ty, body) in &func_decls {
            let mut params = Vec::new();
            for ((name, _), (ty, _)) in *args {
                let ty = self.ty_ctx.convert_type_str(ty)?;
                params.push((name.to_string(), ty));
            }

            let return_ty = match return_ty {
                Some((ty, _)) => self.ty_ctx.convert_type_str(ty)?,
                None => self.ty_ctx.get_primitive("unit"),
            };

            let func = self.func_type_check(name.0.to_string(), params, return_ty, body)?;
            self.func_defs.push(func);
        }

        Ok(())
    }

    fn resolve_all_enums<'src>(
        &mut self,
        enum_decls: Vec<(&Spanned<&'src str>, &Vec<Spanned<CtorDecl>>)>,
    ) -> Result<(), Error> {
        let mut enum_set = HashSet::new();
        for (name, _) in &enum_decls {
            let enum_name = name.0;

            if enum_set.contains(enum_name) {
                return Err(Error::custom(
                    name.1,
                    format!("enum {} already defined", enum_name),
                ));
            }
            if enum_name.chars().next().unwrap().is_lowercase() {
                return Err(Error::custom(
                    name.1,
                    "enum name must start with an uppercase letter".to_string(),
                ));
            }
            enum_set.insert(enum_name.to_string());
            self.ty_ctx.opaque_type(enum_name.to_string());
        }

        for (name, ctors) in &enum_decls {
            let enum_name = name.0;

            let mut ctors_map: HashMap<String, Option<FieldsType>> = HashMap::new();

            for (ctor, span) in *ctors {
                let ctor_name = ctor.name.0;
                if ctor_name.chars().next().unwrap().is_lowercase() {
                    return Err(Error::custom(
                        *span,
                        "enum name must start with an uppercase letter".to_string(),
                    ));
                }
                if ctors_map.contains_key(ctor_name) {
                    return Err(Error::custom(
                        *span,
                        format!("ctor {} already defined in enum {}", ctor_name, enum_name),
                    ));
                }
                let fields = match &ctor.fields {
                    Some(fields) => match fields {
                        Fields::UnnamedFields(fields) => {
                            let mut fields_tys = Vec::new();
                            for (ty, _) in fields {
                                let ty = self.ty_ctx.convert_type_str(ty)?;
                                fields_tys.push(ty);
                            }
                            Some(FieldsType::UnnamedFields(fields_tys))
                        }
                        Fields::NamedFields(fields) => {
                            let mut fields_tys = HashMap::new();
                            for (name, (ty, _)) in fields {
                                let ty = self.ty_ctx.convert_type_str(ty)?;
                                fields_tys.insert(name.0.to_string(), ty);
                            }
                            Some(FieldsType::NamedFields(fields_tys))
                        }
                    },
                    None => None,
                };
                ctors_map.insert(ctor_name.to_string(), fields);
            }
            if ctors_map.is_empty() {
                return Err(Error::custom(
                    name.1,
                    format!("enum {} must have at least one ctor", enum_name),
                ));
            }
            let enum_ty = Enum {
                name: enum_name.to_string(),
                ctors: ctors_map,
            };
            self.ty_ctx.enum_type(enum_ty);
        }

        self.ty_ctx.refine_all_type();
        Ok(())
    }
}
