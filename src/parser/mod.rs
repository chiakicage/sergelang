pub mod lexer;

use crate::ast::*;
use chumsky::prelude::*;
use lexer::Token;

pub fn parser() -> impl Parser<Token, Module, Error = Simple<Token>> {
    let int =
        filter(|t: &Token| if let Token::Int(_) = t { true } else { false }).map(|t| match t {
            Token::Int(x) => Expr::Num(x as f64),
            _ => unreachable!(),
        });

    let ident =
        filter(|t: &Token| if let Token::Ident(_) = t { true } else { false }).map(|t| match t {
            Token::Ident(x) => x,
            _ => unreachable!(),
        });

    let block = recursive(|block| {
        let expr = recursive(|expr| {
            let call = ident
                .then(
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .map(|(f, args)| Expr::Call(f, args));

            let term = int
                .or(expr
                    .clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)))
                .or(call)
                .or(ident.map(Expr::Var));

            let unary = choice((just(Token::Minus), just(Token::Not), just(Token::BitNot)))
                .repeated()
                .then(term)
                .foldr(|_op, rhs| Expr::UnOpExpr {
                    op: match _op {
                        Token::Minus => UnOp::Neg,
                        Token::Not => UnOp::Not,
                        Token::BitNot => UnOp::BitNot,
                        _ => unreachable!(),
                    },
                    rhs: Box::new(rhs),
                });

            let product = unary
                .clone()
                .then(
                    choice((just(Token::Mul), just(Token::Div), just(Token::Mod)))
                        .then(unary.clone())
                        .repeated(),
                )
                .foldl(|lhs, (op, rhs)| Expr::BinOpExpr {
                    lhs: Box::new(lhs),
                    op: match op {
                        Token::Mul => BinOp::Mul,
                        Token::Div => BinOp::Div,
                        Token::Mod => BinOp::Mod,
                        _ => unreachable!(),
                    },
                    rhs: Box::new(rhs),
                });

            let sum = product
                .clone()
                .then(
                    choice((just(Token::Plus), just(Token::Minus)))
                        .then(product.clone())
                        .repeated(),
                )
                .foldl(|lhs, (op, rhs)| Expr::BinOpExpr {
                    lhs: Box::new(lhs),
                    op: match op {
                        Token::Plus => BinOp::Add,
                        Token::Minus => BinOp::Sub,
                        _ => unreachable!(),
                    },
                    rhs: Box::new(rhs),
                });

            let comparison = sum
                .clone()
                .then(choice((
                    just(Token::Eq),
                    just(Token::Neq),
                    just(Token::Lt),
                    just(Token::Gt),
                    just(Token::Lte),
                    just(Token::Gte),
                )))
                .then(sum.clone())
                .map(|((lhs, op), rhs)| Expr::BinOpExpr {
                    lhs: Box::new(lhs),
                    op: match op {
                        Token::Eq => BinOp::Eq,
                        Token::Neq => BinOp::Neq,
                        Token::Lt => BinOp::Lt,
                        Token::Gt => BinOp::Gt,
                        Token::Lte => BinOp::Lte,
                        Token::Gte => BinOp::Gte,
                        _ => unreachable!(),
                    },
                    rhs: Box::new(rhs),
                });

            let r#if = just(Token::If)
                .ignore_then(comparison.clone())
                .then(block.clone())
                .then(just(Token::Else).ignore_then(block.clone()).or_not())
                .map(|((cond, then), els)| Expr::If {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    els: match els {
                        Some(x) => Some(Box::new(x)),
                        None => None,
                    },
                });
            r#if.or(block.clone().map(|x| Expr::Braket(Box::new(x))))
        });

        let r#let = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|(name, rhs)| Stmt::Let {
                name,
                rhs: Box::new(rhs),
            });

        let r#return = just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .map(|x| {
                if let Some(e) = x {
                    Stmt::Return(Some(Box::new(e)))
                } else {
                    Stmt::Return(None)
                }
            });
        let r#while = just(Token::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond, body)| Stmt::While {
                cond: Box::new(cond),
                body: Box::new(body),
            });

        let r#for = just(Token::For)
            .ignore_then(ident)
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then_ignore(just(Token::To))
            .then(expr.clone())
            .then(block.clone())
            .map(|(((var, start), end), body)| Stmt::For {
                var,
                start: Box::new(start),
                end: Box::new(end),
                body: Box::new(body),
            });
        let r#break = just(Token::Break)
            .then_ignore(just(Token::Semicolon))
            .to(Stmt::Break);
        let r#continue = just(Token::Continue)
            .then_ignore(just(Token::Semicolon))
            .to(Stmt::Continue);

        let stmt_expr = expr
            .clone()
            .then_ignore(just(Token::Semicolon))
            .map(|x| Stmt::Expr(Box::new(x)));

        let stmt = choice((
            r#let, r#return, r#while, r#for, r#break, r#continue, stmt_expr,
        ));

        stmt_expr.clone()
            .repeated()
            .or_not()
            .then(expr.clone().or_not())
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|(stmts, value)| Block {
                stmts,
                return_value: value,
            })
    });
    let r#fn = just(Token::Fn)
        .ignore_then(ident)
        .then(
            ident
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(block.clone())
        .map(|((name, args), body)| FuncDecl {
            name,
            args,
            body: Box::new(body),
        });
    let module = r#fn.repeated().map(|defs| Module { func_decls: defs });

    // stmt.then_ignore(end())
    module.then_ignore(end())
}
