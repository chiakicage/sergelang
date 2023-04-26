use chumsky::prelude::*;
use ariadne::{sources, Color, Label, Report, ReportKind};
// use std::{borrow::Borrow, collections::HashMap, hash::Hash};

mod ast;
mod parser;
mod error;

use ast::*;
use parser::lexer::lexer;
use parser::parser;
use error::Span;

// use rpds::HashTrieMap;

// type SymTable<K, V> = Vec<HashMap<K, V>>;
// #[derive(Debug, Clone, Copy)]
// struct FuncEntry<'a> {
//     name: &'a String,
//     args: &'a Vec<String>,
//     body: &'a Expr,
// }

// type SymTable<K, V> = HashTrieMap<K, V>;

// fn eval<'a>(
//     expr: &'a Expr,
//     vars: &SymTable<&'a String, f64>,
//     funcs: &SymTable<&'a String, FuncEntry<'a>>,
// ) -> Result<f64, String> {
//     match expr {
//         Expr::Num(x) => Ok(*x),
//         // Expr::Neg(a) => Ok(-eval(a, vars, funcs)?),
//         // Expr::Add(a, b) => Ok(eval(a, vars, funcs)? + eval(b, vars, funcs)?),
//         // Expr::Sub(a, b) => Ok(eval(a, vars, funcs)? - eval(b, vars, funcs)?),
//         // Expr::Mul(a, b) => Ok(eval(a, vars, funcs)? * eval(b, vars, funcs)?),
//         // Expr::Div(a, b) => Ok(eval(a, vars, funcs)? / eval(b, vars, funcs)?),
//         Expr::UnOpExpr { op, rhs } => match op {
//             UnOp::Neg => Ok(-eval(rhs, vars, funcs)?),
//             _ => unimplemented!(),
//         },
//         Expr::BinOpExpr { lhs, op, rhs } => match op {
//             BinOp::Add => Ok(eval(lhs, vars, funcs)? + eval(rhs, vars, funcs)?),
//             BinOp::Sub => Ok(eval(lhs, vars, funcs)? - eval(rhs, vars, funcs)?),
//             BinOp::Mul => Ok(eval(lhs, vars, funcs)? * eval(rhs, vars, funcs)?),
//             BinOp::Div => Ok(eval(lhs, vars, funcs)? / eval(rhs, vars, funcs)?),

//             _ => unimplemented!(),
//         },
//         Expr::Var(name) => {
//             if let Some(value) = vars.get(name) {
//                 Ok(*value)
//             } else {
//                 Err(format!("undefined variable: {}", name))
//             }
//         }
//         // Expr::Let { name, rhs, then } => {
//         //     let value = eval(rhs, vars, funcs)?;
//         //     let output = eval(then, &vars.insert(name, value), funcs);
//         //     output
//         // }
//         // Expr::Fn {
//         //     name,
//         //     args,
//         //     body,
//         //     then,
//         // } => {
//         //     let output = eval(
//         //         then,
//         //         vars,
//         //         &funcs.insert(name, FuncEntry { name, args, body }),
//         //     );
//         //     output
//         // }
//         Expr::Call(name, argexprs) => {
//             if let Some(FuncEntry { name, args, body }) = funcs.get(name).copied() {
//                 if argexprs.len() == args.len() {
//                     let args = argexprs
//                         .iter()
//                         .map(|expr| eval(expr, vars, funcs))
//                         .zip(args.iter())
//                         .map(|(val, name)| Ok((name, val?)))
//                         .collect::<Result<Vec<(&String, f64)>, String>>()?;
//                     let vars_new = args.iter().fold(vars.clone(), |vars, (name, val)| {
//                         vars.insert(name, *val)
//                     });
//                     let output = eval(body, &vars_new, funcs);
//                     output
//                 } else {
//                     Err(format!("wrong number of arguments to function: {}", name))
//                 }
//             } else {
//                 Err(format!("undefined function: {}", name))
//             }
//         }
//         // _ => unreachable!(),
//     }
// }

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&filename).unwrap();

    println!("{:#?}", src);
    let (tokens, errs) = lexer().parse(src.as_str()).into_output_errors();
    println!("{:?}", tokens);
    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser()
            .parse(
                tokens
                    .as_slice()
                    .spanned(Span::new(src.len(), src.len()))
                    .into(),
            )
            .into_output_errors();
        if let Some(ast) = ast {
            println!("{:#?}", ast);
            Vec::new()
        } else {
            parse_errs
        }
    } else {
        Vec::new()
    };
    errs.into_iter()
            .map(|e| e.map_token(|c| c.to_string()))
            .chain(
                parse_errs
                    .into_iter()
                    .map(|e| e.map_token(|tok| tok.to_string()))
            )
            .for_each(|e| {
                Report::build(ReportKind::Error, filename.clone(), e.span().start)
                    .with_message(e.to_string())
                    .with_label(
                        Label::new((filename.clone(), e.span().into_range()))
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print(sources([(filename.clone(), src.clone())]))
                    .unwrap()
            });
    // match parser().parse(result) {
    //     Ok(ast) => {
    //         println!("{:#?}", ast);
    //         match eval(
    //             &ast,
    //             &HashTrieMap::new(),
    //             &HashTrieMap::new(),
    //         ) {
    //             Ok(x) => println!("{}", x),
    //             Err(e) => println!("Evaluation error: {}", e),
    //         }
    //     }
    //     Err(parse_errs) => parse_errs
    //         .into_iter()
    //         .for_each(|e| println!("Parse error: {:?}", e)),
    // }
}
