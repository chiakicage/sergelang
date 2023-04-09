use chumsky::prelude::*;


pub mod lexer;

use lexer::Token;
use crate::ast::Expr;


pub fn parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
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
    let expr = recursive(|expr| {
        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|(f, args)| Expr::Call(f, args));
        let term = int
            .or(expr.delimited_by(just(Token::LParen), just(Token::RParen)))
            .or(call)
            .or(ident.map(Expr::Var));

        let unary = just(Token::Minus)
            .repeated()
            .then(term)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                just(Token::Mul)
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(just(Token::Div).to(Expr::Div as fn(_, _) -> _))
                    .then(unary.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product.clone()
            .then(
                just(Token::Plus)
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(just(Token::Minus).to(Expr::Sub as fn(_, _) -> _))
                    .then(product.clone())
                    .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        sum
    });
    let decl = recursive(|decl| {
        let r#let = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semicolon))
            .then(decl.clone())
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });
        let r#fn = just(Token::Fn)
            .ignore_then(ident)
            .then(ident
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LParen), just(Token::RParen))
            )
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semicolon))
            .then(decl.clone())
            .map(|(((name, args), body), then)| Expr::Fn {
                name,
                args,
                body: Box::new(body),
                then: Box::new(then),
            });
        r#let.or(r#fn).or(expr.clone())
    });
    decl.then_ignore(end())
}
