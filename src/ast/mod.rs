pub mod ast;

pub use ast::*;

use crate::utils::error::Spanned;
// use std::fmt;

// type SpannedExpr<'src> = Spanned<Expr<'src>>;

// #[derive(Debug)]
// pub enum Ast<'src> {
//     Module(Spanned<Module<'src>>),
//     Decl(Spanned<Decl<'src>>),
// 	Expr(Spanned<Expr<'src>>),
// }

// #[derive(Debug)]
// pub struct AstPrinter<'src> {
// 	ast_type: Ast<'src>,
// 	level: usize,
// }

// impl AstPrinter<'_> {
// 	pub fn new<'src>(module: Spanned<Module<'src>>) -> AstPrinter<'src> {
// 		AstPrinter {
// 			ast_type: Ast::Module(module),
// 			level: 0
// 		}
// 	}
// }

// impl<'src> fmt::Display for AstPrinter<'src> {
// 	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
// 		write!(f, "{{ a: {{ b : 123}} }}")
// 	}
// }

pub fn traverse_type_str<'src>(type_str: &TypeStr<'src>) {
    print!("( ");
    match type_str {
        TypeStr::Tuple(vecs) => {
            print!("TypeStr::Tuple : ");
            for (item, _) in vecs {
                traverse_type_str(&item);
            }
        }
        TypeStr::Array(arr) => {
            print!("TypeStr::Array : ");
            traverse_type_str(&arr.0);
        }
        TypeStr::Named(name) => {
            print!("TypeStr::Named : {:?}", name.0);
        }
        TypeStr::Func(vecs, return_ty) => {
            print!("TyprRef::Func : ( Vec :");
            for (item, _) in vecs {
                traverse_type_str(&item);
            }
            print!(" )");
            traverse_type_str(&return_ty.0);
        }
    }
    print!(" )");
}

pub fn ast_walk<'src>(module: &Spanned<Module<'src>>) {
    print!("\n");
    let (ast, _) = module.clone();
    for (decl, _) in &ast.decls {
        print!("(");
        match decl {
            Decl::FuncDecl {
                name,
                args,
                return_ty,
                body,
            } => {
                print!("( Decl::FuncDecl::name : ({:?}))", name.0);
                print!("( Decl::FuncDecl::args : ");
                for (str, refs) in args {
                    print!("( {:?} ", str.0);
                    traverse_type_str(&refs.0);
                    print!(" )");
                }
                print!(")");
                if let Some(_return_ty) = return_ty {
                    print!("( Decl::FuncDecl::return_ty : ");
                    traverse_type_str(&_return_ty.0);
                    print!(")");
                }
                print!("( Decl::FuncDecl::body : ");
                traverse_body(body);
                print!(")");
            }
            Decl::EnumDecl { name, ctors } => {
                print!("( Decl::EnumDecl::name : ({:?}) )", name.0);
                print!("( Decl::EnumDecl::ctors : ");
                for (ctor, _) in ctors {
                    let _name = ctor.name.0;
                    print!("( CtorDecl::name : ({:?}) )", _name);
                    if let Some(fields) = &ctor.fields {
                        // 遍历fields
                        print!("( CtorDecl::fields : ");
                        match fields {
                            Fields::UnnamedFields(field_ref) => {
                                print!("( Fields::NamelessFields : ");
                                for (field_ref_1, _) in field_ref {
                                    traverse_type_str(&field_ref_1);
                                }
                                print!(" )");
                            }
                            Fields::NamedFields(str_refs) => {
                                print!("( Fields::NamedFields : ");
                                for (str, refs) in str_refs {
                                    print!("( {:?}  ", str.0);
                                    traverse_type_str(&refs.0);
                                    print!(" )");
                                    // 如果是Some类型的pattern，则需要递归遍历
                                }
                                print!(" )");
                            }
                        }
                        print!(" )");
                    }
                }
                print!(" )");
            }
        }
        print!(" )")
    }
    print!("\n");
}

pub fn traverse_body<'src>(body: &Box<Spanned<Expr<'src>>>) {
    traverse_expr(&body);
}

pub fn traverse_expr<'src>(expr: &Spanned<Expr<'src>>) {
    match &expr.0 {
        Expr::Array(array) => {
            traverse_array(&array);
        }
        Expr::Assign { name: _, rhs: _ } => {
            traverse_assign(&expr.0);
        }
        Expr::BinOpExpr {
            lhs: _,
            op: _,
            rhs: _,
        } => {
            traverse_binop_expr(&expr.0);
        }
        Expr::Block(block) => {
            traverse_block(&block);
        }
        Expr::Break => {
            print!("(Expr::Break)");
        }
        Expr::Call { func: _, args: _ } => {
            traverse_call(&expr.0);
        }
        Expr::Closure {
            args: _,
            return_ty: _,
            body: _,
        } => {
            traverse_closure(&expr.0);
        }
        Expr::Continue => {
            print!("(Expr::Continue)");
        }
        Expr::Ctor {
            ty_name: _,
            name: _,
            fields: _,
        } => {
            traverse_ctor_expr(&expr.0);
        }
        Expr::For {
            var: _,
            start: _,
            end: _,
            body: _,
        } => {
            traverse_for(&expr.0);
        }
        Expr::If {
            cond: _,
            then: _,
            els: _,
        } => {
            traverse_if(&expr.0);
        }
        Expr::Index { array: _, index: _ } => {
            traverse_index(&expr.0);
        }
        Expr::Let {
            name: _,
            ty: _,
            rhs: _,
        } => {
            traverse_let(&expr.0);
        }
        Expr::Lit(lit) => {
            traverse_literal(&lit);
        }
        Expr::Match { expr: _, arms: _ } => {
            traverse_match(&expr.0);
        }
        Expr::Return(_) => {
            traverse_return_opt_expr(&expr.0);
        }
        Expr::Tuple(tuple) => {
            traverse_tuple_expr(&tuple);
        }
        Expr::UnOpExpr { op: _, rhs: _ } => {
            traverse_unop_expr(&expr.0);
        }
        Expr::Var(var) => {
            traverse_var(&var);
        }
        Expr::While { cond: _, body: _ } => {
            traverse_while(&expr.0);
        }
    }
}

fn traverse_literal<'src>(literal: &Literal<'src>) {
    // 在这里执行遍历逻辑
    match literal {
        Literal::Int(i) => print!("( Integer: {} )", i),
        Literal::Float(f) => print!("( Float: {} )", f),
        Literal::Str(s) => print!("( String: {} )", s),
        Literal::Bool(b) => print!("( Boolean: {} )", b),
        Literal::Char(c) => print!("( Character: {} )", c),
    }
}

fn traverse_var<'src>(spanned: &Spanned<&'src str>) {
    // 在这里执行遍历逻辑
    print!("( Expr::Var : {:?})", spanned.0);
}

fn traverse_tuple_expr<'src>(tuple: &Vec<Spanned<Expr<'src>>>) {
    print!("( Expr::Tuple : ");

    for expr in tuple {
        traverse_expr(&expr);
    }
    print!(" )");
}

fn traverse_array<'src>(array: &Vec<Spanned<Expr<'src>>>) {
    print!("( Expr::Array :");
    for expr in array {
        traverse_expr(&expr);
    }
    print!(" )");
}

fn traverse_block<'src>(block: &Spanned<Block<'src>>) {
    // 在这里执行遍历逻辑
    print!("( Expr::Block : ");
    let block = &block.0;
    for sub_block in &block.0 {
        traverse_expr(&sub_block);
    }
    print!(" )");
}

fn traverse_binop_expr<'src>(binop_expr: &Expr<'src>) {
    print!("( Expr::BinOpExpr : ");
    match binop_expr {
        Expr::BinOpExpr { lhs, op, rhs } => {
            print!("( lhs : ");
            traverse_expr(&lhs);
            print!(" )");
            print!("( op : ");
            // 在这里执行遍历逻辑
            match op {
                BinOp::Add => print!("(+)"),
                BinOp::Sub => print!("(-)"),
                BinOp::Mul => print!("(*)"),
                BinOp::Div => print!("(/)"),
                BinOp::Mod => print!("(%)"),
                BinOp::Eq => print!("(==)"),
                BinOp::Neq => print!("(!=)"),
                BinOp::Lt => print!("(<)"),
                BinOp::Gt => print!("(>)"),
                BinOp::Lte => print!("(<=)"),
                BinOp::Gte => print!("(>=)"),
                BinOp::And => print!("(&)"),
                BinOp::Or => print!("(|)"),
            }
            print!(" )");
            print!("( rhs : ");
            traverse_expr(&rhs);
            print!(" )");
        }
        _ => {}
    }
    print!(" )");
}

fn traverse_unop_expr<'src>(unop_expr: &Expr<'src>) {
    print!("( Expr::UnOpExpr : ");
    match unop_expr {
        Expr::UnOpExpr { op, rhs } => {
            print!("( op : ");
            match op {
                UnOp::Neg => print!("(-)"),
                UnOp::Not => print!("(Not)"),
                UnOp::BitNot => print!("(BitNot)"),
            }
            print!(" )");
            print!("( rhs : ");
            traverse_expr(&rhs);
            print!(" )");
        }
        _ => {}
    }

    // 在这里执行遍历逻辑
    print!(" )");
}

// print!("(");
// match  {

// }
// print!(")");

fn traverse_if<'src>(if_expr: &Expr<'src>) {
    print!("( Expr::If : ");
    match if_expr {
        Expr::If { cond, then, els } => {
            print!("( cond : ");
            traverse_expr(cond);
            print!(" )");
            print!("( then : ");
            traverse_expr(then);
            print!(" )");
            if let Some(_els) = els {
                print!("( else : ");
                traverse_expr(&_els);
                print!(" )");
            }
        }
        _ => {}
    }
    print!(")");
}

fn traverse_call<'src>(call_expr: &Expr<'src>) {
    print!("( Expr::Call : ");
    match call_expr {
        Expr::Call { func, args } => {
            print!("( func : ");
            traverse_expr(&func);
            print!(" )");
            print!("( args : ");
            for arg in args {
                traverse_expr(&arg);
            }
            print!(" )");
        }
        _ => {}
    }
    print!(")");
}

fn traverse_index<'src>(index_expr: &Expr<'src>) {
    print!("( Expr::Index : ");
    match index_expr {
        Expr::Index { array, index } => {
            print!("( array : ");
            traverse_expr(&array);
            print!(" )");
            print!("( index : ");
            traverse_expr(&index);
            print!(" )");
        }
        _ => {}
    }
    print!(" )");
}

fn traverse_ctor_expr<'src>(ctor_expr: &Expr<'src>) {
    print!("( Expr::Ctor : ");
    // 在这里执行遍历逻辑
    match ctor_expr {
        Expr::Ctor {
            ty_name,
            name,
            fields,
        } => {
            print!("( ty_name : ({:?})) ( name : ({:?}))", ty_name.0, name.0);
            if let Some(fields) = fields {
                print!("( Exprfields : ");
                match fields {
                    ExprFields::UnnamedFields(namelessfields) => {
                        print!("( NamelessFields : ");
                        for field in namelessfields {
                            traverse_expr(&field);
                        }
                        print!(" )");
                    }
                    ExprFields::NamedFields(namefields) => {
                        print!("( NamedFields : ");
                        for (str, field) in namefields {
                            print!("( name : {:?}  ", str.0);
                            if let Some(field) = field {
                                traverse_expr(&field);
                            }
                            print!(" )");
                            // 如果是Some类型的pattern，则需要递归遍历
                        }
                        print!(" )");
                    }
                }
                print!(" )");
            }
        }
        _ => {}
    }
    print!(" )");
}

fn traverse_pattern<'src>(pattern: &Spanned<Pattern<'src>>) {
    print!("( Pattern : ");
    match &pattern.0 {
        Pattern::Lit(lit) => {
            print!("( Pattern::Lit : ");
            traverse_literal(lit);
            print!(" )");
        }
        Pattern::Var(var) => {
            print!("( Pattern::Var : ");
            traverse_var(var);
            print!(" )");
        }
        Pattern::Tuple(tuple) => {
            print!("( Pattern::Tuple : ");
            for _tuple in tuple {
                traverse_pattern(_tuple);
            }
            print!(" )");
        }
        Pattern::Ctor {
            ty_name,
            name,
            fields,
        } => {
            print!("( Pattern::Ctor : ");
            print!("( ty_name : {:?} )", ty_name.0);
            print!("( name : {:?} )", name.0);
            if let Some(fields) = fields {
                match fields {
                    PatternFields::NamedFields(patterns) => {
                        print!("( NamedFields : ");
                        for pattern in patterns {
                            print!("(name : {:?})", pattern.0 .0);
                            if let Some(_pattern) = &pattern.1 {
                                traverse_pattern(&_pattern);
                            }
                        }
                        print!(" )");
                    }
                    PatternFields::UnnamedFields(patterns) => {
                        print!("( NamelessFields : ");
                        for pattern in patterns {
                            traverse_pattern(&pattern);
                        }
                        print!(" )");
                    }
                }
            }
            print!(" )");
        }
    }
    print!(" )");
}

fn traverse_matcharm<'src>(matcharm: &MatchArm<'src>) {
    print!("( MatchArm : ");

    print!("( MatchArm::pattern : ");
    traverse_pattern(&matcharm.pattern);
    print!(" )");
    print!("( MatchArm::expr : ");
    traverse_expr(&matcharm.expr);
    print!(" )");

    print!(" )");
}

fn traverse_match<'src>(match_expr: &Expr<'src>) {
    print!("( Expr::Match : ");
    match match_expr {
        Expr::Match { expr, arms } => {
            print!("( expr : ");
            traverse_expr(&expr);
            print!(" )");
            print!("( arms : ");
            for arm in arms {
                traverse_matcharm(&arm.0);
                // 在这里执行遍历逻辑
            }
            print!(" )");
        }
        _ => {}
    }

    print!(" )");
}

fn traverse_closure<'src>(closure_expr: &Expr<'src>) {
    print!("(Expr::Closure : ");
    match closure_expr {
        Expr::Closure {
            args,
            return_ty,
            body,
        } => {
            print!("( args : ");
            for (arg_name, arg_type) in args {
                // 在这里执行遍历逻辑
                print!("((arg_name : {:?}) ", &arg_name.0);
                traverse_type_str(&arg_type.0);
                print!(" )");
            }
            print!(" )");
            if let Some(return_type) = return_ty {
                print!("( return_ty : ");
                traverse_type_str(&return_type.0);
                print!(" )");
                // 在这里执行遍历逻辑
            }
            print!("( body : ");
            traverse_expr(body);
            print!(" )");
        }
        _ => {}
    }
    print!(" )");
}

fn traverse_let<'src>(let_expr: &Expr<'src>) {
    print!("( Expr::Let : ");
    match let_expr {
        Expr::Let { name, ty, rhs } => {
            print!("( name : {:?})", name.0);
            print!("( ty : ");
            traverse_type_str(&ty.0);
            print!(" )");
            print!("( rhs : ");
            traverse_expr(&rhs);
            print!(" )");
        }
        _ => {}
    }
    // 在这里执行遍历逻辑
    print!(" )");
}

fn traverse_while<'src>(while_expr: &Expr<'src>) {
    print!("( Expr::While : ");
    match while_expr {
        Expr::While { cond, body } => {
            print!("( cond :");
            traverse_expr(&cond);
            print!(" )");
            print!("( body : ");
            traverse_expr(&body);
            print!(" )");
        }
        _ => {}
    }
    print!(" )");
}

fn traverse_for<'src>(for_expr: &Expr<'src>) {
    print!("( Expr::For : ");
    match for_expr {
        Expr::For {
            var: _,
            start,
            end,
            body,
        } => {
            print!("( start :");
            traverse_expr(&start);
            print!(" )");
            print!("( end : ");
            traverse_expr(&end);
            print!(" )");
            print!("( body : ");
            traverse_expr(&body);
            print!(" )");
        }
        _ => {}
    }
    print!(" )");
    // 在这里执行遍历逻辑
}

fn traverse_return_opt_expr<'src>(return_opt_expr: &Expr<'src>) {
    print!("( Expr::Return : ");
    match return_opt_expr {
        Expr::Return(return_opt) => {
            if let Some(return_opt) = return_opt {
                print!("( return output : ");
                traverse_expr(&return_opt);
                print!(" )");
            }
        }
        _ => {}
    }
    print!(" )");
}

fn traverse_assign<'src>(assign_expr: &Expr<'src>) {
    print!("( Expr::Assign : ");
    match assign_expr {
        Expr::Assign { name, rhs } => {
            print!("( name : {:?})", name.0);
            print!("( rhs : ");
            traverse_expr(&rhs);
            print!(" )");
        }
        _ => {}
    }
    // 在这里执行遍历逻辑
    print!(" )");
}
