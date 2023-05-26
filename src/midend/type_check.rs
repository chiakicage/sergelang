use super::typed_ast::*;
use crate::ast::ast::*;
use crate::utils::error::{Error, Span, Spanned};
use crate::utils::type_context::{Enum, FieldsType, PrimitiveType, Type, TypeRef, TypeContext};
use rpds::HashTrieMap;
use std::collections::{HashMap, HashSet};

type SymTable<K, V> = HashTrieMap<K, V>;
// type EnumTable = HashMap<String, Enum>;

/*
pub fn literal_type_check<'src>(lit: &Literal<'src>) -> Result<TypedLiteral, Error> {
    match lit {
        Literal::Int(i) => Ok(TypedLiteral::Int(*i)),
        Literal::Float(f) => Ok(TypedLiteral::Float(*f)),
        Literal::Bool(b) => Ok(TypedLiteral::Bool(*b)),
        Literal::Char(c) => Ok(TypedLiteral::Char(*c)),
        Literal::Str(s) => Ok(TypedLiteral::Str(s.to_string())),
    }
}

pub fn else_type_check<'src>(
    expr: &Option<Box<Spanned<Expr<'src>>>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
    return_ty: &Type, // return type of the function
) -> Result<TypedElse, Error> {
    if let Some(expr) = expr {
        match &expr.0 {
            Expr::Block(block) => {
                let block = block_type_check(block, sym_table, ty_table, return_ty)?;
                Ok(TypedElse::Else(block))
            }
            Expr::If {
                cond,
                then,
                ref els,
            } => {
                let r#if = if_type_check(&cond, &then, els, sym_table, ty_table, return_ty)?;
                Ok(TypedElse::ElseIf(Box::new(r#if)))
            }
            _ => unreachable!(),
        }
    } else {
        Ok(TypedElse::None)
    }
}

pub fn if_type_check<'src>(
    cond: &Spanned<Expr<'src>>,
    then: &Spanned<Expr<'src>>,
    els: &Option<Box<Spanned<Expr<'src>>>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
    return_ty: &Type, // return type of the function
) -> Result<TypedIf, Error> {
    let typed_cond = expr_type_check(cond, sym_table, ty_table, return_ty)?;
    if typed_cond.ty() != Type::Primitive(PrimitiveType::Bool) {
        return Err(Error::custom(
            cond.1,
            format!("type of condition is {} but expected bool", typed_cond.ty()),
        ));
    }
    let then = match then.0 {
        Expr::Block(ref block) => block,
        _ => unreachable!(),
    };
    let ty_then = block_type_check(then, sym_table, ty_table, return_ty)?;
    let ty_els = else_type_check(&els, sym_table, ty_table, return_ty)?;
    if ty_then.ty != ty_els.ty() && els.is_some() {
        return Err(Error::custom(
            then.1,
            format!(
                "type of then branch is {} and the type of else branch is {}",
                ty_then.ty,
                ty_els.ty()
            ),
        ));
    }
    let ty = ty_then.ty.clone();
    Ok(TypedIf {
        cond: Box::new(typed_cond),
        then: ty_then,
        els: ty_els,
        ty,
    })
}

pub fn pattern_ctor_extract(
    patterns: &Vec<&TypedPattern>,
    ty: &Type,
    ty_table: &SymTable<String, Enum>,
) -> HashMap<String, Vec<Vec<TypedPattern>>> {
    let mut ret = HashMap::new();
    if let Type::Named(ty_name) = ty {
        let ty = ty_table.get(ty_name).unwrap();
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
                                        if let TypedPatternFields::UnnamedFields(pat_fields) =
                                            pat_fields.as_ref().unwrap()
                                        {
                                            pat_fields.clone()
                                        } else {
                                            unreachable!();
                                        }
                                    }
                                    FieldsType::NamedFields(ctors) => {
                                        if let TypedPatternFields::NamedFields(pat_fields) =
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
                                                            ty: pat.0.clone(),
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
                                    .map(|ty| {
                                        TypedPattern::Var(TypedVariable {
                                            name: "_".to_string(),
                                            ty: ty.clone(),
                                        })
                                    })
                                    .collect::<Vec<_>>(),
                                FieldsType::NamedFields(ctors) => ctors
                                    .values()
                                    .map(|ty| {
                                        TypedPattern::Var(TypedVariable {
                                            name: "_".to_string(),
                                            ty: ty.clone(),
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
    patterns: &Vec<&TypedPattern>,
    ty: &Type,
) -> Vec<Vec<TypedPattern>> {
    if let Type::Tuple(tys) = ty {
        return patterns
            .iter()
            .map(|p| {
                if let TypedPattern::Tuple(pat_fields) = p {
                    pat_fields.clone()
                } else if let TypedPattern::Var(_) = p {
                    tys.iter()
                        .map(|ty| {
                            TypedPattern::Var(TypedVariable {
                                name: "_".to_string(),
                                ty: ty.clone(),
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
    patterns: &Vec<&TypedPattern>,
    ty: &Type,
    ty_table: &SymTable<String, Enum>,
) -> bool {
    let has_var = patterns.iter().any(|p| p.is_var());
    if has_var {
        return false;
    }
    match ty {
        Type::Primitive(pri_ty) => match pri_ty {
            PrimitiveType::Int | PrimitiveType::String | PrimitiveType::Char => true,
            PrimitiveType::Bool => {
                let mut set = HashSet::new();
                for p in patterns {
                    if let TypedPattern::Lit(TypedLiteral::Bool(b)) = p {
                        set.insert(*b);
                    } else {
                        unreachable!();
                    }
                }
                set.len() == 2
            }
            _ => unreachable!(),
        },
        Type::Named(ty_name) => {
            let mut set = HashSet::new();
            let ctors = &ty_table.get(ty_name).unwrap().ctors;

            for p in patterns {
                if let TypedPattern::Ctor { name, .. } = p {
                    set.insert(name.clone());
                } else {
                    unreachable!();
                }
            }
            if set.len() == ctors.len() {
                let new_patterns = pattern_ctor_extract(patterns, ty, ty_table);
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
                                let new_ty = Type::Tuple(tys.clone());
                                if pattern_non_exhaustive_check(&new_pats, &new_ty, ty_table) {
                                    return true;
                                }
                            }
                            FieldsType::NamedFields(ctors) => {
                                let new_ty =
                                    Type::Tuple(ctors.values().cloned().collect::<Vec<_>>());
                                if pattern_non_exhaustive_check(&new_pats, &new_ty, ty_table) {
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
                let new_ty = tuple.first().unwrap();
                return pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table);
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

            let first_ty = tuple.first().unwrap();
            let first_patterns = patterns
                .iter()
                .map(|p| p.first().unwrap())
                .collect::<Vec<_>>();

            match first_ty {
                Type::Primitive(PrimitiveType::Bool) => {
                    let mut set = HashSet::new();
                    for p in first_patterns {
                        if let TypedPattern::Lit(TypedLiteral::Bool(b)) = p {
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
                                TypedPattern::Lit(TypedLiteral::Bool(b)) => *b,
                                TypedPattern::Var(_) => true,
                                _ => unreachable!(),
                            })
                            .map(|p| {
                                TypedPattern::Tuple(p.iter().skip(1).cloned().collect::<Vec<_>>())
                            })
                            .collect::<Vec<_>>();
                        let false_new_patterns = patterns
                            .iter()
                            .filter(|p| match p.first().unwrap() {
                                TypedPattern::Lit(TypedLiteral::Bool(b)) => !b,
                                TypedPattern::Var(_) => true,
                                _ => unreachable!(),
                            })
                            .map(|p| {
                                TypedPattern::Tuple(p.iter().skip(1).cloned().collect::<Vec<_>>())
                            })
                            .collect::<Vec<_>>();
                        let new_ty = Type::Tuple(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                        let true_new_patterns = true_new_patterns.iter().collect::<Vec<_>>();
                        let false_new_patterns = false_new_patterns.iter().collect::<Vec<_>>();
                        return pattern_non_exhaustive_check(&true_new_patterns, &new_ty, ty_table)
                            || pattern_non_exhaustive_check(
                                &false_new_patterns,
                                &new_ty,
                                ty_table,
                            );
                    } else {
                        let new_patterns = patterns
                            .iter()
                            .filter(|p| match p.first().unwrap() {
                                TypedPattern::Lit(TypedLiteral::Bool(_)) => false,
                                TypedPattern::Var(_) => true,
                                _ => unreachable!(),
                            })
                            .map(|p| {
                                TypedPattern::Tuple(p.iter().skip(1).cloned().collect::<Vec<_>>())
                            })
                            .collect::<Vec<_>>();
                        let new_ty = Type::Tuple(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                        let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                        return pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table);
                    }
                }
                Type::Named(ty_name) => {
                    let mut set = HashSet::new();
                    let ty = ty_table.get(ty_name).unwrap();

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
                            pattern_ctor_extract(&first_patterns, &first_ty, ty_table);

                        let ty = ty_table.get(ty_name).unwrap();
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
                                let new_ty = Type::Tuple(new_tys);
                                let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                                if pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table) {
                                    return true;
                                }
                            } else {
                                let new_ty =
                                    Type::Tuple(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                                let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                                if pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table) {
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
                                TypedPattern::Tuple(p.iter().skip(1).cloned().collect::<Vec<_>>())
                            })
                            .collect::<Vec<_>>();

                        let new_ty = Type::Tuple(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                        let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                        return pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table);
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
                        .map(|p| TypedPattern::Tuple(p.iter().skip(1).cloned().collect::<Vec<_>>()))
                        .collect::<Vec<_>>();
                    let new_ty = Type::Tuple(tuple.iter().skip(1).cloned().collect::<Vec<_>>());
                    let new_patterns = new_patterns.iter().collect::<Vec<_>>();
                    return pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table);
                }
                Type::Tuple(tys) => {
                    let new_first_patterns =
                        pattern_tuple_extract(&first_patterns, &first_ty);

                    let residual_new_patterns = patterns
                        .iter()
                        .map(|p| p.iter().skip(1).cloned().collect::<Vec<_>>())
                        .collect::<Vec<_>>();

                    let new_tys = tys
                        .iter()
                        .chain(tuple.iter().skip(1))
                        .cloned()
                        .collect::<Vec<_>>();
                    let new_ty = Type::Tuple(new_tys);
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
                    return pattern_non_exhaustive_check(&new_patterns, &new_ty, ty_table);
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

pub fn pattern_type_check<'src>(
    pattern: &Spanned<Pattern<'src>>,
    expected_ty: &Type,
    ty_table: &SymTable<String, Enum>,
) -> Result<TypedPattern, Error> {
    let (pattern, span) = pattern;
    let span = *span;
    match pattern {
        Pattern::Lit(lit) => {
            let lit = literal_type_check(lit)?;
            if lit.ty() != *expected_ty {
                return Err(Error::custom(
                    span,
                    format!(
                        "invalid pattern type, expected {}, got {}",
                        expected_ty,
                        lit.ty()
                    ),
                ));
            }
            Ok(TypedPattern::Lit(lit))
        }
        Pattern::Var((name, _)) => {
            let var = TypedVariable {
                name: name.to_string(),
                ty: expected_ty.clone(),
            };
            Ok(TypedPattern::Var(var))
        }
        Pattern::Tuple(pats) => {
            let tys = match expected_ty {
                Type::Tuple(tys) => tys,
                _ => {
                    return Err(Error::custom(
                        span,
                        format!("invalid pattern type, expected tuple, got {}", expected_ty),
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
                let pat = pattern_type_check(pat, ty, ty_table)?;
                tuple.push(pat);
            }
            Ok(TypedPattern::Tuple(tuple))
        }
        Pattern::Ctor {
            ty_name,
            name,
            fields,
        } => {
            if let Type::Named(ty) = expected_ty {
                if ty_name.0 != ty {
                    return Err(Error::custom(
                        span,
                        format!("invalid pattern type, expected {}, got {}", ty, ty_name.0),
                    ));
                }
            } else {
                return Err(Error::custom(
                    span,
                    format!(
                        "invalid pattern type, expected {}, got {}",
                        expected_ty, ty_name.0
                    ),
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
                            for (ty, field) in ty_fields.iter().zip(fields.iter()) {
                                let pat = pattern_type_check(field, ty, ty_table)?;
                                pat_fields.push(pat);
                            }
                            let pat_fields = TypedPatternFields::UnnamedFields(pat_fields);
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
                            for (name, pattern) in fields {
                                if name.0 != "_" {
                                    let ty = ty_fields.get(name.0).ok_or_else(|| {
                                        Error::custom(
                                            name.1,
                                            format!("field {} not found in enum", name.0),
                                        )
                                    })?;
                                    if let Some(pattern) = pattern {
                                        let pat = pattern_type_check(pattern, ty, ty_table)?;
                                        pat_fields
                                            .insert(name.0.to_string(), (ty.clone(), Some(pat)));
                                    } else {
                                        pat_fields.insert(name.0.to_string(), (ty.clone(), None));
                                    }
                                }
                            }
                            let pat_fields = TypedPatternFields::NamedFields(pat_fields);
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
    pat: &TypedPattern,
    sym_table: &SymTable<String, Type>,
) -> SymTable<String, Type> {
    let mut sym_table = sym_table.clone();
    match pat {
        TypedPattern::Var(var) => {
            if var.name != "_" {
                sym_table = sym_table.insert(var.name.clone(), var.ty.clone());
            }
        }
        TypedPattern::Tuple(pats) => {
            for pat in pats {
                sym_table = insert_pattern_symbol_binding(pat, &sym_table);
            }
        }
        TypedPattern::Ctor { fields, .. } => {
            if let Some(fields) = fields {
                match fields {
                    TypedPatternFields::UnnamedFields(pats) => {
                        for pat in pats {
                            sym_table = insert_pattern_symbol_binding(pat, &sym_table);
                        }
                    }
                    TypedPatternFields::NamedFields(pats) => {
                        for (name, (ty, pat)) in pats.iter() {
                            if let Some(pat) = pat {
                                sym_table = insert_pattern_symbol_binding(pat, &sym_table);
                            } else {
                                sym_table = sym_table.insert(name.clone(), ty.clone());
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

pub fn expr_type_check<'src>(
    expr: &Spanned<Expr<'src>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
    return_ty: &Type, // return type of the function
) -> Result<TypedExpr, Error> {
    let (expr, span) = expr;
    let span = *span;
    match expr {
        Expr::Lit(lit) => {
            let lit = literal_type_check(lit)?;
            Ok(TypedExpr::Literal(lit))
        }
        Expr::Var((name, span)) => {
            if let Some(ty) = sym_table.get(*name) {
                let var = TypedVariable {
                    name: name.to_string(),
                    ty: ty.clone(),
                };
                Ok(TypedExpr::Variable(var))
            } else {
                Err(Error::custom(*span, format!("undefined variable {}", name)))
            }
        }
        Expr::Tuple(exprs) => {
            let mut tuple = Vec::new();
            let mut types = Vec::new();
            for expr in exprs {
                let expr = expr_type_check(expr, sym_table, ty_table, return_ty)?;
                types.push(expr.ty());
                tuple.push(expr);
            }
            let tuple = TypedTuple {
                elements: tuple,
                ty: Type::Tuple(types),
            };
            Ok(TypedExpr::Tuple(tuple))
        }
        Expr::Array(exprs) => {
            if exprs.is_empty() {
                return Err(Error::custom(span, "empty array is not allowed"));
            }

            let ty = expr_type_check(&exprs[0], sym_table, ty_table, return_ty)?.ty();
            let mut ty_exprs = Vec::new();
            for expr in exprs {
                let expr = expr_type_check(expr, sym_table, ty_table, return_ty)?;
                if ty != expr.ty() {
                    return Err(Error::custom(
                        span,
                        "array elements must have the same type",
                    ));
                }
                ty_exprs.push(expr);
            }
            let array = TypedArray {
                elements: ty_exprs,
                ty: Type::Array(Box::new(ty)),
            };
            Ok(TypedExpr::Array(array))
        }
        Expr::Block(block) => Ok(TypedExpr::Block(block_type_check(
            block, sym_table, ty_table, return_ty,
        )?)),
        Expr::BinOpExpr { lhs, op, rhs } => {
            let ty_lhs = expr_type_check(lhs, sym_table, ty_table, return_ty)?;
            let ty_rhs = expr_type_check(rhs, sym_table, ty_table, return_ty)?;

            let ty = match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    match (&ty_lhs.ty(), &ty_rhs.ty()) {
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Type::Primitive(PrimitiveType::Int),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Type::Primitive(PrimitiveType::Float),
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Type::Primitive(PrimitiveType::Float),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Type::Primitive(PrimitiveType::Float),
                        (_, _) => Type::Primitive(PrimitiveType::Unit),
                    }
                }
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    match (&ty_lhs.ty(), &ty_rhs.ty()) {
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Type::Primitive(PrimitiveType::Bool),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Type::Primitive(PrimitiveType::Bool),
                        (
                            Type::Primitive(PrimitiveType::Int),
                            Type::Primitive(PrimitiveType::Float),
                        ) => Type::Primitive(PrimitiveType::Bool),
                        (
                            Type::Primitive(PrimitiveType::Float),
                            Type::Primitive(PrimitiveType::Int),
                        ) => Type::Primitive(PrimitiveType::Bool),
                        (_, _) => Type::Primitive(PrimitiveType::Unit),
                    }
                }
                BinOp::And | BinOp::Or => match (&ty_lhs.ty(), &ty_rhs.ty()) {
                    (
                        Type::Primitive(PrimitiveType::Bool),
                        Type::Primitive(PrimitiveType::Bool),
                    ) => Type::Primitive(PrimitiveType::Bool),
                    (_, _) => Type::Primitive(PrimitiveType::Unit),
                },
            };
            if ty == Type::Primitive(PrimitiveType::Unit) {
                Err(Error::custom(
                    span,
                    format!(
                        "invalid types for binary operator {:?} between {:?} and {:?}",
                        op,
                        ty_lhs.ty(),
                        ty_rhs.ty()
                    ),
                ))
            } else {
                Ok(TypedExpr::BinOp(TypedBinOp {
                    lhs: Box::new(ty_lhs),
                    op: op.clone(),
                    rhs: Box::new(ty_rhs),
                    ty,
                }))
            }
        }
        Expr::UnOpExpr { op, rhs } => {
            let ty_rhs = expr_type_check(rhs, sym_table, ty_table, return_ty)?;
            let ty = match op {
                UnOp::Neg => match ty_rhs.ty() {
                    Type::Primitive(PrimitiveType::Int) => Type::Primitive(PrimitiveType::Int),
                    Type::Primitive(PrimitiveType::Float) => Type::Primitive(PrimitiveType::Float),
                    _ => Type::Primitive(PrimitiveType::Unit),
                },
                UnOp::Not => match ty_rhs.ty() {
                    Type::Primitive(PrimitiveType::Bool) => Type::Primitive(PrimitiveType::Bool),
                    _ => Type::Primitive(PrimitiveType::Unit),
                },
                _ => unimplemented!(),
            };
            if ty == Type::Primitive(PrimitiveType::Unit) {
                Err(Error::custom(
                    span,
                    format!(
                        "invalid types for unary operator {:?} on {:?}",
                        op,
                        ty_rhs.ty()
                    ),
                ))
            } else {
                Ok(TypedExpr::UnOp(TypedUnOp {
                    op: op.clone(),
                    rhs: Box::new(ty_rhs),
                    ty,
                }))
            }
        }
        Expr::If { cond, then, els } => {
            let r#if = if_type_check(cond, then, els, sym_table, ty_table, return_ty)?;
            Ok(TypedExpr::If(r#if))
        }
        Expr::Call { func, args } => {
            let ty_func = expr_type_check(func, sym_table, ty_table, return_ty)?;
            if let Type::Func(params, ret_ty) = ty_func.ty() {
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
                    let ty_arg = expr_type_check(arg, sym_table, ty_table, return_ty)?;
                    if *param != ty_arg.ty() {
                        return Err(Error::custom(
                            arg.1,
                            format!(
                                "invalid argument type, expected {}, got {}",
                                param,
                                ty_arg.ty()
                            ),
                        ));
                    }
                    ty_args.push(ty_arg);
                }
                Ok(TypedExpr::Call(TypedCall {
                    func: Box::new(ty_func),
                    args: ty_args,
                    ty: *ret_ty,
                }))
            } else {
                Err(Error::custom(
                    func.1,
                    format!("invalid function type {}", ty_func.ty()),
                ))
            }
        }
        Expr::Index { array, index } => {
            let ty_array = expr_type_check(array, sym_table, ty_table, return_ty)?;
            let ty_index = expr_type_check(index, sym_table, ty_table, return_ty)?;
            match ty_array.ty() {
                Type::Array(ty) => {
                    if ty_index.ty() != Type::Primitive(PrimitiveType::Int) {
                        return Err(Error::custom(
                            index.1,
                            format!("invalid index type {}", ty_index.ty()),
                        ));
                    }
                    Ok(TypedExpr::Index(TypedIndex {
                        array: Box::new(ty_array),
                        index: Box::new(ty_index),
                        ty: *ty,
                    }))
                }
                _ => Err(Error::custom(
                    array.1,
                    format!("invalid array type {}", ty_array.ty()),
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
                                        let ty_val =
                                            expr_type_check(val, sym_table, ty_table, return_ty)?;
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
                                        let ty_val = sym_table.get(name.0).ok_or_else(|| {
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
        }
    }
}

pub fn block_type_check<'src>(
    block: &Spanned<Block<'src>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
    return_ty: &Type,
) -> Result<TypedBlock, Error> {
    let mut sym_table = sym_table.clone();
    let mut exprs = Vec::new();
    let mut last_ty = Type::Primitive(PrimitiveType::Unit);

    for expr in &block.0 .0 {
        let expr = expr_type_check(expr, &sym_table, ty_table, return_ty)?;
        if let TypedExpr::Let(TypedLet { name, ty, rhs: _ }) = &expr {
            sym_table = sym_table.insert(name.clone(), ty.clone());
        }
        last_ty = expr.ty();
        exprs.push(expr);
    }
    Ok(TypedBlock { exprs, ty: last_ty })
}

pub fn func_type_check<'src>(
    name: String,
    params: Vec<(String, Type)>,
    return_ty: &Type,
    body: &Spanned<Expr<'src>>,
    sym_table: &SymTable<String, Type>,
    ty_table: &SymTable<String, Enum>,
) -> Result<TypedFunc, Error> {
    let body = match &body.0 {
        Expr::Block(block) => block,
        _ => unreachable!(),
    };

    let mut sym_table = sym_table.clone();

    for (name, ty) in &params {
        sym_table = sym_table.insert(name.clone(), ty.clone());
    }
    let ty_body = block_type_check(body, &sym_table, ty_table, return_ty)?;

    if ty_body.ty != *return_ty {
        return Err(Error::custom(
            body.1,
            format!(
                "invalid return type, expected {}, got {}",
                return_ty, ty_body.ty
            ),
        ));
    }
    let ty = Type::Func(
        params.clone().iter().map(|(_, ty)| ty.clone()).collect(),
        Box::new(return_ty.clone()),
    );
    Ok(TypedFunc {
        name,
        params,
        return_ty: return_ty.clone(),
        ty,
        body: ty_body,
    })
}

pub fn module_type_check<'src>(module: &Spanned<Module<'src>>) -> Result<TypedModule, Error> {
    let (module, _) = module;
    let mut ty_ctx: TypeContext = Default::default();
    let mut sym_table: SymTable<String, TypeRef> = SymTable::new();
    let mut enum_set = HashSet::new();

    // let is_primitive = |name: &str| match name {
    //     "i32" | "float" | "bool" | "string" | "char" => true,
    //     _ => false,
    // };

    for (decl, _) in &module.decls {
        match decl {
            Decl::EnumDecl { name, .. } => {
                let enum_name = name.0;

                if enum_set.contains_key(enum_name) {
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

                ty_ctx.add_opaque(enum_name.to_string());
            }
            _ => {}
        }
    }
    // for (decl, _) in &module.decls {
    //     match decl {
    //         Decl::FuncDecl {
    //             name,
    //             args,
    //             return_ty,
    //             body: _,
    //         } => {
    //             let func_name = name.0;
    //             let mut arg_tys = Vec::new();
    //             let mut arg_names = Vec::new();
    //             for ((name, _), (ty, _)) in args {
    //                 let ty = convert_type_str(ty, &fake_ty_table)?;
    //                 arg_names.push(name.to_string());
    //                 arg_tys.push(ty.clone());
    //             }

    //             let return_ty = match return_ty {
    //                 Some((ty, _)) => convert_type_str(ty, &fake_ty_table)?,
    //                 None => Type::Primitive(PrimitiveType::Unit),
    //             };

    //             let func_ty = Type::Func(arg_tys, Box::new(return_ty.clone()));

    //             if sym_table.contains_key(func_name) {
    //                 return Err(Error::custom(
    //                     name.1,
    //                     format!("function {} already defined", func_name),
    //                 ));
    //             }

    //             sym_table = sym_table.insert(func_name.to_string(), func_ty.clone());
    //         }
    //         _ => {}
    //     }
    // }

    for (decl, _) in &module.decls {
        match decl {
            Decl::EnumDecl { name, ctors } => {
                let enum_name = name.0;

                let mut ctors_map: HashMap<String, Option<FieldsType>> = HashMap::new();

                for (ctor, span) in ctors {
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
                                    let ty = ty_ctx.convert_type_str(ty)?;
                                    fields_tys.push(ty);
                                }
                                Some(FieldsType::UnnamedFields(fields_tys))
                            }
                            Fields::NamedFields(fields) => {
                                let mut fields_tys = HashMap::new();
                                for (name, (ty, _)) in fields {
                                    let ty = convert_type_str(ty, &fake_ty_table)?;
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
                ty_table = ty_table.insert(enum_name.to_string(), enum_ty);
            }
            _ => {}
        }
    }
    let mut func_defs = Vec::new();
    // for (decl, _) in &module.decls {
    //     match decl {
    //         Decl::FuncDecl {
    //             name,
    //             args,
    //             return_ty,
    //             body,
    //         } => {
    //             let mut params = Vec::new();
    //             for ((name, _), (ty, _)) in args {
    //                 let ty = convert_type_str(ty, &ty_table)?;
    //                 params.push((name.to_string(), ty));
    //             }

    //             let return_ty = match return_ty {
    //                 Some((ty, _)) => convert_type_str(ty, &ty_table)?,
    //                 None => Type::Primitive(PrimitiveType::Unit),
    //             };

    //             let func = func_type_check(
    //                 name.0.to_string(),
    //                 params,
    //                 &return_ty,
    //                 body,
    //                 &sym_table,
    //                 &ty_table,
    //             )?;
    //             func_defs.push(func);
    //         }
    //         _ => {}
    //     }
    // }
    Ok(TypedModule {
        func_table: sym_table,
        ty_ctx,
        func_defs,
    })
}
*/
