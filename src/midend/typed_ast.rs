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
pub enum ExprFieldsKind {
    UnnamedFields(Vec<TypedExpr>),
    NamedFields(HashMap<String, Option<TypedExpr>>),
}
#[derive(Debug, Clone)]
pub enum PatternFieldsKind {
    UnnamedFields(Vec<TypedPattern>),
    NamedFields(HashMap<String, (TypeRef, Option<TypedPattern>)>),
}

#[derive(Debug, Clone)]
pub enum TypedPattern {
    Lit(TypedLiteral),
    Var(TypedVariable),
    Tuple(Vec<TypedPattern>),
    Ctor {
        ty_name: String,
        name: String,
        fields: Option<PatternFieldsKind>,
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
    pub fields: Option<ExprFieldsKind>,
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
    pub expr: Option<Box<TypedExpr>>,
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
        typed_ast.add_stdlib_functions();
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

    pub fn else_type_check<'src>(
        &mut self,
        expr: &Option<Box<Spanned<Expr<'src>>>>,
        sym_table: &SymTable<String, TypeRef>,
        return_ty: TypeRef, // return type of the function
        in_loop: bool,
    ) -> Result<TypedElse, Error> {
        if let Some(expr) = expr {
            match &expr.0 {
                Expr::Block(block) => {
                    let block = self.block_type_check(block, sym_table, return_ty, in_loop)?;
                    let ty = block.ty;
                    Ok(TypedElse {
                        kind: ElseKind::Else(block),
                        ty,
                    })
                }
                Expr::If {
                    cond,
                    then,
                    ref els,
                } => {
                    let r#if =
                        self.if_type_check(&cond, &then, els, sym_table, return_ty, in_loop)?;
                    let ty = r#if.ty;
                    Ok(TypedElse {
                        kind: ElseKind::ElseIf(Box::new(r#if)),
                        ty,
                    })
                }
                _ => unreachable!(),
            }
        } else {
            Ok(TypedElse {
                kind: ElseKind::None,
                ty: self.ty_ctx.get_unit(),
            })
        }
    }

    pub fn if_type_check<'src>(
        &mut self,
        cond: &Spanned<Expr<'src>>,
        then: &Spanned<Expr<'src>>,
        els: &Option<Box<Spanned<Expr<'src>>>>,
        sym_table: &SymTable<String, TypeRef>,
        return_ty: TypeRef, // return type of the function
        in_loop: bool,
    ) -> Result<TypedIf, Error> {
        let typed_cond = self.expr_type_check(cond, sym_table, return_ty, in_loop)?;
        if typed_cond.ty != self.ty_ctx.get_bool() {
            return Err(Error::custom(
                cond.1,
                format!(
                    "type of condition is {} but expected bool",
                    self.ty_ctx.typeref_to_string(typed_cond.ty)
                ),
            ));
        }
        let then = match then.0 {
            Expr::Block(ref block) => block,
            _ => unreachable!(),
        };
        let ty_then = self.block_type_check(then, sym_table, return_ty, in_loop)?;
        let ty_els = self.else_type_check(els, sym_table, return_ty, in_loop)?;
        if ty_then.ty != ty_els.ty && els.is_some() {
            return Err(Error::custom(
                then.1,
                format!(
                    "type of then branch is {} and the type of else branch is {}",
                    self.ty_ctx.typeref_to_string(ty_then.ty),
                    self.ty_ctx.typeref_to_string(ty_els.ty)
                ),
            ));
        }
        let ty = ty_then.ty;
        Ok(TypedIf {
            cond: Box::new(typed_cond),
            then: ty_then,
            els: ty_els,
            ty,
        })
    }
    pub fn pattern_ctor_extract(
        &mut self,
        patterns: &Vec<&TypedPattern>,
        ty: TypeRef,
    ) -> HashMap<String, Vec<Vec<TypedPattern>>> {
        let mut ret = HashMap::new();
        let ty = self.ty_ctx.types[ty].clone();
        if let Type::Enum(ty) = ty {
            for (name, fields) in &ty.ctors {
                if let Some(fields) = fields {
                    let pat_fields = patterns
                        .iter()
                        .filter(|p| match p {
                            TypedPattern::Ctor {
                                name: ctor_name, ..
                            } => ctor_name == name,
                            TypedPattern::Var(_) => true,
                            _ => false,
                        })
                        .map(|p| {
                            match p {
                                TypedPattern::Ctor {
                                    fields: pat_fields, ..
                                } => {
                                    match fields {
                                        FieldsType::UnnamedFields(tys) => {
                                            if let PatternFieldsKind::UnnamedFields(pat_fields) =
                                                pat_fields.as_ref().unwrap()
                                            {
                                                pat_fields.clone()
                                            } else {
                                                unreachable!();
                                            }
                                        }
                                        FieldsType::NamedFields(ctors) => {
                                            if let PatternFieldsKind::NamedFields(pat_fields) =
                                                pat_fields.as_ref().unwrap()
                                            {
                                                // pat_fields.clone()
                                                let mut new_fields = Vec::new();
                                                for (field_name, _) in ctors {
                                                    let pat = pat_fields.get(field_name).unwrap();
                                                    if let Some(pat) = &pat.1 {
                                                        new_fields.push(pat.clone())
                                                    } else {
                                                        new_fields.push(TypedPattern::Var(
                                                            TypedVariable {
                                                                name: name.clone(),
                                                                ty: pat.0,
                                                            },
                                                        ))
                                                    }
                                                }
                                                new_fields
                                            } else {
                                                unreachable!();
                                            }
                                        }
                                    }
                                }
                                TypedPattern::Var(_) => match fields {
                                    FieldsType::UnnamedFields(tys) => tys
                                        .iter()
                                        .copied()
                                        .map(|ty| {
                                            TypedPattern::Var(TypedVariable {
                                                name: "_".to_string(),
                                                ty: ty,
                                            })
                                        })
                                        .collect::<Vec<_>>(),
                                    FieldsType::NamedFields(ctors) => ctors
                                        .values()
                                        .copied()
                                        .map(|ty| {
                                            TypedPattern::Var(TypedVariable {
                                                name: "_".to_string(),
                                                ty: ty,
                                            })
                                        })
                                        .collect::<Vec<_>>(),
                                },
                                _ => unreachable!(),
                            }
                        })
                        .collect::<Vec<_>>();
                    ret.insert(name.clone(), pat_fields);
                } else {
                    let pat_fields = patterns
                        .iter()
                        .filter(|p| match p {
                            TypedPattern::Ctor {
                                name: ctor_name, ..
                            } => ctor_name == name,
                            TypedPattern::Var(_) => true,
                            _ => false,
                        })
                        .map(|p| match p {
                            TypedPattern::Ctor { .. } | TypedPattern::Var(_) => Vec::new(),
                            _ => unreachable!(),
                        })
                        .collect::<Vec<Vec<TypedPattern>>>();
                    ret.insert(name.clone(), pat_fields);
                }
            }
        } else {
            unreachable!();
        }
        ret
    }
    pub fn pattern_tuple_extract(
        &mut self,
        patterns: &Vec<&TypedPattern>,
        ty: TypeRef,
    ) -> Vec<Vec<TypedPattern>> {
        if let Type::Tuple(tys) = &self.ty_ctx.types[ty] {
            return patterns
                .iter()
                .map(|p| {
                    if let TypedPattern::Tuple(pat_fields) = p {
                        pat_fields.clone()
                    } else if let TypedPattern::Var(_) = p {
                        tys.iter()
                            .copied()
                            .map(|ty| {
                                TypedPattern::Var(TypedVariable {
                                    name: "_".to_string(),
                                    ty,
                                })
                            })
                            .collect::<Vec<_>>()
                    } else {
                        unreachable!();
                    }
                })
                .collect::<Vec<_>>();
        } else {
            unreachable!();
        }
    }

    // use algorithm in http://moscova.inria.fr/~maranget/papers/warn/warn004.html

    pub fn pattern_non_exhaustive_check(
        &mut self,
        patterns: &Vec<&TypedPattern>,
        ty: TypeRef,
    ) -> bool {
        let has_var = patterns.iter().any(|p| p.is_var());
        if has_var {
            return false;
        }
        match self.ty_ctx.types[ty].clone() {
            Type::Primitive(pri_ty) => match pri_ty {
                PrimitiveType::Int | PrimitiveType::String | PrimitiveType::Char => true,
                PrimitiveType::Bool => {
                    let mut set = HashSet::new();
                    for p in patterns {
                        if let TypedPattern::Lit(TypedLiteral {
                            kind: LiteralKind::Bool(b),
                            ..
                        }) = p
                        {
                            set.insert(*b);
                        } else {
                            unreachable!();
                        }
                    }
                    set.len() == 2
                }
                _ => unreachable!(),
            },
            Type::Enum(ty) => {
                let mut set = HashSet::new();
                let ctors = &ty.ctors;

                for p in patterns {
                    if let TypedPattern::Ctor { name, .. } = p {
                        set.insert(name.clone());
                    } else {
                        unreachable!();
                    }
                }
                if set.len() == ctors.len() {
                    let new_patterns = self.pattern_ctor_extract(
                        patterns,
                        self.ty_ctx.get_typeref_by_name(&ty.name).unwrap(),
                    );
                    for (name, pats) in new_patterns {
                        let fields = ctors.get(&name).unwrap();
                        let new_pats = pats
                            .iter()
                            .map(|p| TypedPattern::Tuple(p.clone()))
                            .collect::<Vec<_>>();
                        let new_pats = new_pats.iter().collect::<Vec<_>>();
                        if let Some(fields) = fields {
                            match fields {
                                FieldsType::UnnamedFields(tys) => {
                                    let new_ty = self.ty_ctx.tuple_type(tys.clone());
                                    if self.pattern_non_exhaustive_check(&new_pats, new_ty) {
                                        return true;
                                    }
                                }
                                FieldsType::NamedFields(ctors) => {
                                    // let new_ty =
                                    //     Type::Tuple(ctors.values().cloned().collect::<Vec<_>>());
                                    let new_ty = self
                                        .ty_ctx
                                        .tuple_type(ctors.values().cloned().collect::<Vec<_>>());
                                    if self.pattern_non_exhaustive_check(&new_pats, new_ty) {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    return false;
                }
                true
            }
            Type::Tuple(tuple) => {
                if tuple.len() == 1 {
                    let new_patterns = patterns
                        .iter()
                        .map(|p| {
                            if let TypedPattern::Tuple(p) = p {
                                p.first().unwrap()
                            } else {
                                unreachable!();
                            }
                        })
                        .collect::<Vec<_>>();
                    let new_ty = tuple.first().copied().unwrap();
                    return self.pattern_non_exhaustive_check(&new_patterns, new_ty);
                }
                let patterns = patterns
                    .iter()
                    .map(|p| {
                        if let TypedPattern::Tuple(pats) = p {
                            pats
                        } else {
                            unreachable!();
                        }
                    })
                    .collect::<Vec<_>>();

                let first_ty = tuple.first().copied().unwrap();
                let first_patterns = patterns
                    .iter()
                    .map(|p| p.first().unwrap())
                    .collect::<Vec<_>>();

                match self.ty_ctx.types[first_ty].clone() {
                    Type::Primitive(PrimitiveType::Bool) => {
                        let mut set = HashSet::new();
                        for p in first_patterns {
                            if let TypedPattern::Lit(TypedLiteral {
                                kind: LiteralKind::Bool(b),
                                ..
                            }) = p
                            {
                                set.insert(*b);
                            } else if let TypedPattern::Var(_) = p {
                            } else {
                                unreachable!();
                            }
                        }
                        if set.len() == 2 {
                            let true_new_patterns = patterns
                                .iter()
                                .filter(|p| match p.first().unwrap() {
                                    TypedPattern::Lit(TypedLiteral {
                                        kind: LiteralKind::Bool(b),
                                        ..
                                    }) => *b,
                                    TypedPattern::Var(_) => true,
                                    _ => unreachable!(),
                                })
                                .map(|p| {
                                    TypedPattern::Tuple(
                                        p.iter().skip(1).cloned().collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();
                            let false_new_patterns = patterns
                                .iter()
                                .filter(|p| match p.first().unwrap() {
                                    TypedPattern::Lit(TypedLiteral {
                                        kind: LiteralKind::Bool(b),
                                        ..
                                    }) => !b,
                                    TypedPattern::Var(_) => true,
                                    _ => unreachable!(),
                                })
                                .map(|p| {
                                    TypedPattern::Tuple(
                                        p.iter().skip(1).cloned().collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();
                            let new_ty = self
                                .ty_ctx
                                .tuple_type(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                            let true_new_patterns = true_new_patterns.iter().collect::<Vec<_>>();
                            let false_new_patterns = false_new_patterns.iter().collect::<Vec<_>>();
                            return self.pattern_non_exhaustive_check(&true_new_patterns, new_ty)
                                || self.pattern_non_exhaustive_check(&false_new_patterns, new_ty);
                        } else {
                            let new_patterns = patterns
                                .iter()
                                .filter(|p| match p.first().unwrap() {
                                    TypedPattern::Lit(TypedLiteral {
                                        kind: LiteralKind::Bool(_),
                                        ..
                                    }) => false,
                                    TypedPattern::Var(_) => true,
                                    _ => unreachable!(),
                                })
                                .map(|p| {
                                    TypedPattern::Tuple(
                                        p.iter().skip(1).cloned().collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();
                            let new_ty = self
                                .ty_ctx
                                .tuple_type(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                            let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                            return self.pattern_non_exhaustive_check(&new_patterns, new_ty);
                        }
                    }
                    Type::Enum(ty) => {
                        let mut set = HashSet::new();

                        for p in &first_patterns {
                            match p {
                                TypedPattern::Ctor { name, .. } => {
                                    set.insert(name.clone());
                                }
                                TypedPattern::Var(_) => {}
                                _ => unreachable!(),
                            }
                        }
                        if set.len() == ty.ctors.len() {
                            let new_first_patterns =
                                self.pattern_ctor_extract(&first_patterns, first_ty);

                            for (name, fields) in &ty.ctors {
                                let residual_patterns = patterns
                                    .iter()
                                    .filter(|p| match p.first().unwrap() {
                                        TypedPattern::Ctor {
                                            name: ctor_name, ..
                                        } => ctor_name == name,
                                        TypedPattern::Var(_) => true,
                                        _ => unreachable!(),
                                    })
                                    .map(|p| p.iter().skip(1).cloned().collect::<Vec<_>>())
                                    .collect::<Vec<_>>();
                                let new_first_patterns = new_first_patterns.get(name).unwrap();
                                let new_patterns = new_first_patterns
                                    .iter()
                                    .zip(residual_patterns.iter())
                                    .map(|(first, residual)| {
                                        TypedPattern::Tuple(
                                            first
                                                .iter()
                                                .chain(residual.iter())
                                                .cloned()
                                                .collect::<Vec<_>>(),
                                        )
                                    })
                                    .collect::<Vec<_>>();
                                if let Some(fields) = fields {
                                    let new_first_tys = match fields {
                                        FieldsType::UnnamedFields(tys) => tys.clone(),
                                        FieldsType::NamedFields(fields) => {
                                            fields.values().cloned().collect::<Vec<_>>()
                                        }
                                    };
                                    let new_tys = new_first_tys
                                        .iter()
                                        .chain(tuple.iter().skip(1))
                                        .cloned()
                                        .collect::<Vec<_>>();
                                    let new_ty = self.ty_ctx.tuple_type(new_tys);
                                    let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                                    if self.pattern_non_exhaustive_check(&new_patterns, new_ty) {
                                        return true;
                                    }
                                } else {
                                    let new_ty = self.ty_ctx.tuple_type(
                                        tuple.iter().skip(1).cloned().collect::<Vec<_>>(),
                                    );
                                    let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                                    if self.pattern_non_exhaustive_check(&new_patterns, new_ty) {
                                        return true;
                                    }
                                }
                            }
                            return false;
                        } else {
                            let new_patterns = patterns
                                .iter()
                                .filter(|p| match p.first().unwrap() {
                                    TypedPattern::Var(_) => true,
                                    TypedPattern::Ctor { .. } => false,
                                    _ => unreachable!(),
                                })
                                .map(|p| {
                                    TypedPattern::Tuple(
                                        p.iter().skip(1).cloned().collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();

                            let new_ty = self
                                .ty_ctx
                                .tuple_type(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                            let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                            return self.pattern_non_exhaustive_check(&new_patterns, new_ty);
                        }
                    }
                    Type::Primitive(PrimitiveType::Char)
                    | Type::Primitive(PrimitiveType::String)
                    | Type::Primitive(PrimitiveType::Int)
                    | Type::Primitive(PrimitiveType::Float) => {
                        let new_patterns = patterns
                            .iter()
                            .filter(|p| match p.first().unwrap() {
                                TypedPattern::Lit(_) => false,
                                TypedPattern::Var(_) => true,
                                _ => unreachable!(),
                            })
                            .map(|p| {
                                TypedPattern::Tuple(p.iter().skip(1).cloned().collect::<Vec<_>>())
                            })
                            .collect::<Vec<_>>();
                        let new_ty = self
                            .ty_ctx
                            .tuple_type(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                        let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                        return self.pattern_non_exhaustive_check(&new_patterns, new_ty);
                    }
                    Type::Tuple(tys) => {
                        let new_first_patterns =
                            self.pattern_tuple_extract(&first_patterns, first_ty);

                        let residual_new_patterns = patterns
                            .iter()
                            .map(|p| p.iter().skip(1).cloned().collect::<Vec<_>>())
                            .collect::<Vec<_>>();

                        let new_tys = tys
                            .iter()
                            .chain(tuple.iter().skip(1))
                            .cloned()
                            .collect::<Vec<_>>();
                        let new_ty = self.ty_ctx.tuple_type(new_tys);
                        let new_patterns = new_first_patterns
                            .iter()
                            .zip(residual_new_patterns.iter())
                            .map(|(first, residual)| {
                                TypedPattern::Tuple(
                                    first
                                        .iter()
                                        .chain(residual.iter())
                                        .cloned()
                                        .collect::<Vec<_>>(),
                                )
                            })
                            .collect::<Vec<_>>();
                        let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                        return self.pattern_non_exhaustive_check(&new_patterns, new_ty);
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn pattern_type_check<'src>(
        &mut self,
        pattern: &Spanned<Pattern<'src>>,
        expected_ty: TypeRef,
    ) -> Result<TypedPattern, Error> {
        let (pattern, span) = pattern;
        let span = *span;
        match pattern {
            Pattern::Lit(lit) => {
                let lit = self.literal_type_check(lit)?;
                if lit.ty != expected_ty {
                    return Err(Error::custom(
                        span,
                        format!(
                            "invalid pattern type, expected {}, got {}",
                            self.ty_ctx.typeref_to_string(expected_ty),
                            self.ty_ctx.typeref_to_string(lit.ty)
                        ),
                    ));
                }
                Ok(TypedPattern::Lit(lit))
            }
            Pattern::Var((name, _)) => {
                let var = TypedVariable {
                    name: name.to_string(),
                    ty: expected_ty,
                };
                Ok(TypedPattern::Var(var))
            }
            Pattern::Tuple(pats) => {
                let tys = match self.ty_ctx.types[expected_ty].clone() {
                    Type::Tuple(tys) => tys,
                    _ => {
                        return Err(Error::custom(
                            span,
                            format!(
                                "invalid pattern type, expected tuple, got {}",
                                self.ty_ctx.typeref_to_string(expected_ty)
                            ),
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
                let mut tuple = Vec::new();
                for (ty, pat) in tys.iter().zip(pats.iter()) {
                    let pat = self.pattern_type_check(pat, *ty)?;
                    tuple.push(pat);
                }
                Ok(TypedPattern::Tuple(tuple))
            }
            Pattern::Ctor {
                ty_name,
                name,
                fields,
            } => {
                let ty = self
                    .ty_ctx
                    .convert_type_str(&TypeStr::Named(ty_name.clone()))?;
                if ty != expected_ty {
                    return Err(Error::custom(
                        span,
                        format!(
                            "invalid pattern type, expected {}, got {}",
                            self.ty_ctx.typeref_to_string(expected_ty),
                            self.ty_ctx.typeref_to_string(ty)
                        ),
                    ));
                }
                let r#enum = self.ty_ctx.get_enum_by_typeref(ty).unwrap().clone();
                let ctor_fields = r#enum.ctors.get(name.0).ok_or({
                    Error::custom(name.1, format!("constructor {} not found", name.0))
                })?;
                if let Some(ctor_fields) = ctor_fields {
                    match ctor_fields {
                        FieldsType::UnnamedFields(ty_fields) => {
                            if let Some(PatternFields::UnnamedFields(fields)) = fields {
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
                                let mut pat_fields = Vec::new();
                                for (ty, field) in ty_fields.iter().copied().zip(fields.iter()) {
                                    let pat = self.pattern_type_check(field, ty)?;
                                    pat_fields.push(pat);
                                }
                                let pat_fields = PatternFieldsKind::UnnamedFields(pat_fields);
                                Ok(TypedPattern::Ctor {
                                    ty_name: ty_name.0.to_string(),
                                    name: name.0.to_string(),
                                    fields: Some(pat_fields),
                                })
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
                                let mut pat_fields = HashMap::new();
                                let mut fields_set = HashSet::new();
                                for (name, pattern) in fields {
                                    let ty = ty_fields.get(name.0).copied().ok_or({
                                        Error::custom(
                                            name.1,
                                            format!("field {} not found in enum", name.0),
                                        )
                                    })?;
                                    if !fields_set.insert(name.0) {
                                        return Err(Error::custom(
                                            name.1,
                                            format!("field {} already set", name.0),
                                        ));
                                    }
                                    if let Some(pattern) = pattern {
                                        let pat = self.pattern_type_check(pattern, ty)?;
                                        pat_fields.insert(name.0.to_string(), (ty, Some(pat)));
                                    } else {
                                        pat_fields.insert(name.0.to_string(), (ty, None));
                                    }
                                }
                                let pat_fields = PatternFieldsKind::NamedFields(pat_fields);
                                Ok(TypedPattern::Ctor {
                                    ty_name: ty_name.0.to_string(),
                                    name: name.0.to_string(),
                                    fields: Some(pat_fields),
                                })
                            } else {
                                Err(Error::custom(span, format!("expected named fields")))
                            }
                        }
                    }
                } else {
                    match fields {
                        Some(_) => Err(Error::custom(span, format!("expected no fields"))),
                        None => Ok(TypedPattern::Ctor {
                            ty_name: ty_name.0.to_string(),
                            name: name.0.to_string(),
                            fields: None,
                        }),
                    }
                }
            }
        }
    }
    pub fn insert_pattern_symbol_binding(
        &mut self,
        pat: &TypedPattern,
        sym_table: &SymTable<String, TypeRef>,
    ) -> SymTable<String, TypeRef> {
        let mut sym_table = sym_table.clone();
        match pat {
            TypedPattern::Var(var) => {
                if var.name != "_" {
                    sym_table = sym_table.insert(var.name.clone(), var.ty);
                }
            }
            TypedPattern::Tuple(pats) => {
                for pat in pats {
                    sym_table = self.insert_pattern_symbol_binding(pat, &sym_table);
                }
            }
            TypedPattern::Ctor { fields, .. } => {
                if let Some(fields) = fields {
                    match fields {
                        PatternFieldsKind::UnnamedFields(pats) => {
                            for pat in pats {
                                sym_table = self.insert_pattern_symbol_binding(pat, &sym_table);
                            }
                        }
                        PatternFieldsKind::NamedFields(pats) => {
                            for (name, (ty, pat)) in pats.iter() {
                                if let Some(pat) = pat {
                                    sym_table = self.insert_pattern_symbol_binding(pat, &sym_table);
                                } else {
                                    sym_table = sym_table.insert(name.clone(), *ty);
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        sym_table
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
                    ty: self.ty_ctx.get_or_insert_type(Type::Tuple(types)),
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
                    ty: self.ty_ctx.get_or_insert_type(Type::Array(ty)),
                };
                Ok(ExprKind::Array(array))
            }
            Expr::Block(block) => Ok(ExprKind::Block(
                self.block_type_check(block, sym_table, return_ty, in_loop)?,
            )),
            Expr::BinOpExpr { lhs, op, rhs } => {
                let ty_lhs = self.expr_type_check(lhs, sym_table, return_ty, in_loop)?;
                let ty_rhs = self.expr_type_check(rhs, sym_table, return_ty, in_loop)?;
                let int_type = self.ty_ctx.get_i32();
                let float_type = self.ty_ctx.get_f64();
                let bool_type = self.ty_ctx.get_bool();
                let ty_lhs_str = self.ty_ctx.typeref_to_string(ty_lhs.ty);
                let ty_rhs_str = self.ty_ctx.typeref_to_string(ty_rhs.ty);
                let ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if ty_lhs.ty != int_type && ty_lhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for lhs of binary operator {}: {}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != int_type && ty_rhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {}: {}",
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
                                    "invalid type for lhs of binary operator {}: {}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != int_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {}: {}",
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
                                    "invalid type for lhs of binary operator {}: {}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != int_type && ty_rhs.ty != float_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {}: {}",
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
                                    "invalid type for lhs of binary operator {}: {}",
                                    op, ty_lhs_str
                                ),
                            ));
                        }
                        if ty_rhs.ty != bool_type {
                            return Err(Error::custom(
                                span,
                                format!(
                                    "invalid type for rhs of binary operator {}: {}",
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
                                    "invalid type for rhs of unary operator {}: {}",
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
                                    "invalid type for rhs of unary operator {}: {}",
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

            Expr::If { cond, then, els } => {
                let r#if = self.if_type_check(cond, then, els, sym_table, return_ty, in_loop)?;
                Ok(ExprKind::If(r#if))
            }
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
                        format!(
                            "invalid callable type {}",
                            self.ty_ctx.typeref_to_string(ty_func.ty)
                        ),
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
                            format!(
                                "invalid index type {}",
                                self.ty_ctx.typeref_to_string(ty_index.ty)
                            ),
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
                        format!(
                            "invalid array type {}",
                            self.ty_ctx.typeref_to_string(ty_array.ty)
                        ),
                    ))
                }
            }

            Expr::Ctor {
                ty_name,
                name,
                fields,
            } => {
                let ty_enum = self
                    .ty_ctx
                    .get_typeref_by_name(ty_name.0)
                    .ok_or(Error::custom(
                        ty_name.1,
                        format!("enum {} not found", name.0),
                    ))?;
                let r#enum = self.ty_ctx.get_enum_by_typeref(ty_enum).unwrap().clone();
                let ctor_fields = r#enum.ctors.get(name.0).ok_or({
                    Error::custom(name.1, format!("constructor {} not found", name.0))
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
                                    if let Some(ty) = ctor_fields.get(name.0).copied() {
                                        if ty_fields.contains_key(name.0) {
                                            return Err(Error::custom(
                                                name.1,
                                                format!("field {} already set", name.0),
                                            ));
                                        }

                                        if let Some(val) = val {
                                            let ty_val = self.expr_type_check(
                                                val, sym_table, return_ty, in_loop,
                                            )?;
                                            if ty != ty_val.ty {
                                                return Err(Error::custom(
                                                    val.1,
                                                    format!(
                                                        "invalid field type, expected {}, got {}",
                                                        self.ty_ctx.typeref_to_string(ty),
                                                        self.ty_ctx.typeref_to_string(ty_val.ty)
                                                    ),
                                                ));
                                            }
                                            ty_fields.insert(name.0.to_string(), Some(ty_val));
                                        } else {
                                            let ty_val = sym_table.get(name.0).copied().ok_or({
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
                                                        self.ty_ctx.typeref_to_string(ty),
                                                        self.ty_ctx.typeref_to_string(ty_val)
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
                                let ty_fields = ExprFieldsKind::NamedFields(ty_fields);
                                Ok(ExprKind::Ctor(TypedCtor {
                                    ty_name: ty_name.0.to_string(),
                                    name: name.0.to_string(),
                                    fields: Some(ty_fields),
                                    ty: ty_enum,
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
                                for (ty, field) in ctor_fields.iter().copied().zip(fields.iter()) {
                                    let ty_val =
                                        self.expr_type_check(field, sym_table, return_ty, in_loop)?;
                                    if ty != ty_val.ty {
                                        return Err(Error::custom(
                                            field.1,
                                            format!(
                                                "invalid field type, expected {}, got {}",
                                                self.ty_ctx.typeref_to_string(ty),
                                                self.ty_ctx.typeref_to_string(ty_val.ty)
                                            ),
                                        ));
                                    }
                                    ty_fields.push(ty_val);
                                }
                                let ty_fields = ExprFieldsKind::UnnamedFields(ty_fields);
                                Ok(ExprKind::Ctor(TypedCtor {
                                    ty_name: ty_name.0.to_string(),
                                    name: name.0.to_string(),
                                    fields: Some(ty_fields),
                                    ty: ty_enum,
                                }))
                            } else {
                                Err(Error::custom(span, format!("expected unnamed fields")))
                            }
                        }
                    }
                } else {
                    match fields {
                        Some(_) => Err(Error::custom(span, format!("expected no fields"))),
                        None => Ok(ExprKind::Ctor(TypedCtor {
                            ty_name: ty_name.0.to_string(),
                            name: name.0.to_string(),
                            fields: None,
                            ty: ty_enum,
                        })),
                    }
                }
            }
            Expr::Match { expr, arms } => {
                let ty_expr = self.expr_type_check(expr, sym_table, return_ty, in_loop)?;
                if !self.ty_ctx.check_ty_can_be_matched(ty_expr.ty) {
                    return Err(Error::custom(
                        expr.1,
                        format!(
                            "type {} cannot be matched",
                            self.ty_ctx.typeref_to_string(ty_expr.ty)
                        ),
                    ));
                }
                let mut ty = None;
                let mut ty_arms = Vec::new();
                for (arm, _) in arms {
                    let ty_pat = self.pattern_type_check(&arm.pattern, ty_expr.ty)?;
                    let new_sym_table = self.insert_pattern_symbol_binding(&ty_pat, sym_table);
                    let ty_arm =
                        self.expr_type_check(&arm.expr, &new_sym_table, return_ty, in_loop)?;
                    if let Some(ty) = ty {
                        if ty != ty_arm.ty {
                            return Err(Error::custom(
                                arm.expr.1,
                                format!(
                                    "invalid arm type, expected {}, got {}",
                                    self.ty_ctx.typeref_to_string(ty),
                                    self.ty_ctx.typeref_to_string(ty_arm.ty)
                                ),
                            ));
                        }
                    } else {
                        ty = Some(ty_arm.ty);
                    }
                    let ty_arm = TypedMatchArm {
                        pattern: ty_pat,
                        ty: ty_arm.ty,
                        expr: Box::new(ty_arm),
                    };
                    ty_arms.push(ty_arm);
                }
                if let Some(ty) = ty {
                    let pats = ty_arms.iter().map(|arm| &arm.pattern).collect::<Vec<_>>();
                    if self.pattern_non_exhaustive_check(&pats, ty_expr.ty) {
                        return Err(Error::custom(span, format!("pattern not exhaustive")));
                    }
                    Ok(ExprKind::Match(TypedMatch {
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
                    let ty = self.ty_ctx.convert_type_str(&ty.0)?;
                    args_ty.push(ty);
                    args_name.push(name.0.to_string());
                    sym_table = sym_table.insert(name.0.to_string(), ty);
                }

                let return_ty = match return_ty {
                    Some((ty, _)) => self.ty_ctx.convert_type_str(ty)?,
                    None => self.ty_ctx.get_unit(),
                };

                let ty_body = self.expr_type_check(body, &sym_table, return_ty, false)?;

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
                let ty_closure = TypedClosure {
                    args_name,
                    args_ty: args_ty.clone(),
                    return_ty,
                    body: Box::new(ty_body),
                    ty: self.ty_ctx.func_type(args_ty, return_ty),
                };

                Ok(ExprKind::Closure(ty_closure))
            }
            Expr::Let { name, ty, rhs } => {
                let ty = self.ty_ctx.convert_type_str(&ty.0)?;
                let ty_rhs = self.expr_type_check(rhs, sym_table, return_ty, in_loop)?;
                if ty != ty_rhs.ty {
                    return Err(Error::custom(
                        rhs.1,
                        format!(
                            "invalid rhs type, expected {}, got {}",
                            self.ty_ctx.typeref_to_string(ty),
                            self.ty_ctx.typeref_to_string(ty_rhs.ty)
                        ),
                    ));
                }
                let ty_let = TypedLet {
                    name: name.0.to_string(),
                    ty,
                    rhs: Box::new(ty_rhs),
                };
                Ok(ExprKind::Let(ty_let))
            }

            Expr::While { cond, body } => {
                let ty_cond = self.expr_type_check(cond, sym_table, return_ty, in_loop)?;
                if ty_cond.ty != self.ty_ctx.get_bool() {
                    return Err(Error::custom(
                        cond.1,
                        format!(
                            "invalid condition type, expected bool, got {}",
                            self.ty_ctx.typeref_to_string(ty_cond.ty)
                        ),
                    ));
                }
                let body = match body.0 {
                    Expr::Block(ref block) => block,
                    _ => unreachable!(),
                };
                let ty_body = self.block_type_check(body, sym_table, return_ty, true)?;
                if ty_body.ty != self.ty_ctx.get_unit() {
                    return Err(Error::custom(
                        body.1,
                        format!(
                            "invalid body type, expected no type, got {}",
                            self.ty_ctx.typeref_to_string(ty_body.ty)
                        ),
                    ));
                }
                let ty_while = TypedWhile {
                    cond: Box::new(ty_cond),
                    body: ty_body,
                };

                Ok(ExprKind::While(ty_while))
            }
            Expr::For {
                var,
                start,
                end,
                body,
            } => {
                let ty_start = self.expr_type_check(start, sym_table, return_ty, in_loop)?;
                let ty_end = self.expr_type_check(end, sym_table, return_ty, in_loop)?;
                if ty_start.ty != self.ty_ctx.get_i32() {
                    return Err(Error::custom(
                        start.1,
                        format!(
                            "invalid start type, expected i32, got {}",
                            self.ty_ctx.typeref_to_string(ty_start.ty)
                        ),
                    ));
                }
                if ty_end.ty != self.ty_ctx.get_i32() {
                    return Err(Error::custom(
                        end.1,
                        format!(
                            "invalid end type, expected i32, got {}",
                            self.ty_ctx.typeref_to_string(ty_end.ty)
                        ),
                    ));
                }
                let new_sym_table = sym_table.insert(var.0.to_string(), self.ty_ctx.get_i32());
                let body = match body.0 {
                    Expr::Block(ref block) => block,
                    _ => unreachable!(),
                };
                let ty_body = self.block_type_check(body, &new_sym_table, return_ty, true)?;
                if ty_body.ty != self.ty_ctx.get_unit() {
                    return Err(Error::custom(
                        body.1,
                        format!(
                            "invalid body type, expected no type, got {}",
                            self.ty_ctx.typeref_to_string(ty_body.ty)
                        ),
                    ));
                }
                let ty_for = TypedFor {
                    var: var.0.to_string(),
                    start: Box::new(ty_start),
                    end: Box::new(ty_end),
                    body: ty_body,
                };
                Ok(ExprKind::For(ty_for))
            }
            Expr::Return(expr) => {
                let expr = match expr {
                    Some(expr) => Some(self.expr_type_check(expr, sym_table, return_ty, in_loop)?),
                    None => None,
                };
                let ty_expr = expr.as_ref().map_or(self.ty_ctx.get_unit(), |e| e.ty);

                if ty_expr != return_ty {
                    return Err(Error::custom(
                        span,
                        format!(
                            "invalid return type, expected {}, got {}",
                            self.ty_ctx.typeref_to_string(return_ty),
                            self.ty_ctx.typeref_to_string(ty_expr)
                        ),
                    ));
                }
                Ok(ExprKind::Return(TypedReturn {
                    expr: expr.map(Box::new),
                }))
            }
            Expr::Break => Ok(ExprKind::Break),
            Expr::Continue => Ok(ExprKind::Continue),
            Expr::Assign { name, rhs } => {
                let ty_var = sym_table.get(name.0).copied().ok_or(Error::custom(
                    span,
                    format!("undefined variable {}", name.0),
                ))?;
                let ty_rhs = self.expr_type_check(rhs, sym_table, return_ty, in_loop)?;
                if ty_var != ty_rhs.ty {
                    return Err(Error::custom(
                        rhs.1,
                        format!(
                            "invalid rhs type, expected {}, got {}",
                            self.ty_ctx.typeref_to_string(ty_var),
                            self.ty_ctx.typeref_to_string(ty_rhs.ty)
                        ),
                    ));
                }
                Ok(ExprKind::Assign(TypedAssign {
                    name: name.0.to_string(),
                    rhs: Box::new(ty_rhs),
                }))
            }
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
                ExprKind::Let(_) => self.ty_ctx.get_unit(),
                ExprKind::While(_) => self.ty_ctx.get_unit(),
                ExprKind::For(_) => self.ty_ctx.get_unit(),
                ExprKind::Return(_) => self.ty_ctx.get_unit(),
                ExprKind::Break => self.ty_ctx.get_unit(),
                ExprKind::Continue => self.ty_ctx.get_unit(),
                ExprKind::Assign(_) => self.ty_ctx.get_unit(),
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
        let mut last_ty = self.ty_ctx.get_unit();
        for expr in &block.0.stmts {
            let inner_expr = match expr {
                BlockedExpr::WithSemicolon(expr) => expr,
                BlockedExpr::WithoutSemicolon(expr) => expr,
            };
            let mut ty_expr = self.expr_type_check(inner_expr, &sym_table, return_ty, in_loop)?;
            match &ty_expr.kind {
                ExprKind::Let(TypedLet { name, ty, .. }) => {
                    sym_table = sym_table.insert(name.clone(), *ty);
                }
                ExprKind::Break | ExprKind::Continue => {
                    if !in_loop {
                        return Err(Error::custom(
                            inner_expr.1,
                            "break/continue outside of loop".to_string(),
                        ));
                    }
                }
                ExprKind::Return(TypedReturn { expr: ty_expr }) => match ty_expr.as_ref() {
                    Some(ty_expr) => {
                        if return_ty == self.ty_ctx.get_unit() {
                            return Err(Error::custom(
                                inner_expr.1,
                                "invalid return, expected no return type".to_string(),
                            ));
                        }
                        if ty_expr.ty != return_ty {
                            return Err(Error::custom(
                                inner_expr.1,
                                format!(
                                    "invalid return type, expected {}, got {}",
                                    self.ty_ctx.typeref_to_string(return_ty),
                                    self.ty_ctx.typeref_to_string(ty_expr.ty)
                                ),
                            ));
                        }
                    }
                    None => {
                        if return_ty != self.ty_ctx.get_unit() {
                            return Err(Error::custom(
                                inner_expr.1,
                                format!(
                                    "invalid return type, expected {}, got no return type",
                                    self.ty_ctx.typeref_to_string(return_ty),
                                ),
                            ));
                        }
                    }
                },
                _ => {}
            }
            last_ty = match expr {
                BlockedExpr::WithSemicolon(_) => self.ty_ctx.get_unit(),
                BlockedExpr::WithoutSemicolon(_) => ty_expr.ty,
            };
            ty_expr.ty = last_ty;
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
    fn add_stdlib_functions(&mut self) {
        let getint = self.ty_ctx.func_type(vec![], self.ty_ctx.get_i32());
        let getch = self.ty_ctx.func_type(vec![], self.ty_ctx.get_char());

        let putint = self
            .ty_ctx
            .func_type(vec![self.ty_ctx.get_i32()], self.ty_ctx.get_unit());
        let putch = self
            .ty_ctx
            .func_type(vec![self.ty_ctx.get_char()], self.ty_ctx.get_unit());

        let starttime = self.ty_ctx.func_type(vec![], self.ty_ctx.get_unit());
        let stoptime = self.ty_ctx.func_type(vec![], self.ty_ctx.get_unit());

        self.func_table = self.func_table.insert("__serge_read_i32".to_string(), getint);
        // self.func_table = self.func_table.insert("getch".to_string(), getch);
        self.func_table = self.func_table.insert("__serge_print".to_string(), putint);
        // self.func_table = self.func_table.insert("putch".to_string(), putch);
        // self.func_table = self.func_table.insert("starttime".to_string(), starttime);
        // self.func_table = self.func_table.insert("stoptime".to_string(), stoptime);
    }
}
