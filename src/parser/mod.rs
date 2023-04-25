pub mod lexer;

use crate::ast::ast::*;
use chumsky::{input::SpannedInput, prelude::*, recursive::Direct};
use lexer::{Error, Span, Spanned, Token};

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [Spanned<Token<'src>>]>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Module<'src>, Error<'tokens, Token<'src>>> + Clone
{
    let lit = select! {
        Token::Int(x) = span => (Expr::Lit(Literal::Int(x)), span),
        // Token::Str(s) = span => Expr::Lit(Literal::Str(s)).spanned(span)
    }
    .labelled("literal");

    let ident = select! {
        Token::Ident(s) = span => (s, span)
    }
    .labelled("ident");

    let var = ident
        .map_with_span(|s, span| (Expr::Var(s), span))
        .labelled("var");

    let typed_ctor = ident
        .then_ignore(just(Token::Scope))
        .then(ident)
        .map_with_span(|(ty, ctor), span| (ty, ctor));

    // let pattern = recursive(|pattern| {
    //     let items = pattern
    //         .clone()
    //         .separated_by(just(Token::Comma))
    //         .allow_trailing()
    //         .collect::<Vec<_>>();

    //     let tuple = items
    //         .delimited_by(just(Token::LParen), just(Token::RParen))
    //         .map_with_span(|args, span: Span| {
    //             (Pattern::Tuple(args), span)
    //         });

    //     let nameless_ctor = typed_ctor
    //         .then(
    //             items
    //                 .clone()
    //                 .delimited_by(just(Token::LParen), just(Token::RParen))
    //                 .map_with_span(|args, span| (args, span)),
    //         )
    //         .map_with_span(|((ty, ctor), args), span| (Pattern::Ctor(ty, ctor, args), span));

    //     let lit = lit.map_with_span(|lit, span| (Pattern::Lit(lit), span));

    //     let var = var.map_with_span(|var, span| (Pattern::Var(var), span));

    //     let pattern = choice([tuple, ctor, lit, var]);

    //     pattern
    //         .clone()
    //         .delimited_by(just(Token::LParen), just(Token::RParen))
    //         .map_with_span(|pattern, span| (pattern, span))
    //         .or(pattern)
    // });

    let block = recursive::<_, _, Error<'tokens, Token<'src>>, _, _>(
        |block: Recursive<Direct<_, Spanned<Block>, _>>| {
            let expr = recursive(|expr| {
                let tuple = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .map_with_span(|args, span| (Expr::Tuple(args), span));

                let items = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>();

                let call = ident
                    .then(
                        items
                            .clone()
                            .delimited_by(just(Token::LParen), just(Token::RParen)),
                    )
                    .map_with_span(|(f, args), span: Span| (Expr::Call(f, args), span));

                let paren = just(Token::LParen)
                    .ignore_then(expr.clone())
                    .then_ignore(just(Token::RParen))
                    .map_with_span(|exp, span| (exp.0, span));

                let term = call.or(lit).or(var).or(paren).or(tuple);
                let unary = choice((just(Token::Minus), just(Token::Not), just(Token::BitNot)))
                    .map_with_span(|token, span: Span| (token, span))
                    .repeated()
                    .foldr(term, |op, rhs: Spanned<Expr>| {
                        let span = (op.1.start..rhs.1.end).into();
                        (
                            Expr::UnOpExpr {
                                op: match op.0 {
                                    Token::Minus => UnOp::Neg,
                                    Token::Not => UnOp::Not,
                                    Token::BitNot => UnOp::BitNot,
                                    _ => unreachable!(),
                                },
                                rhs: Box::new(rhs),
                            },
                            span,
                        )
                    });
                let product = unary.clone().foldl(
                    choice((just(Token::Mul), just(Token::Div), just(Token::Mod)))
                        .then(unary.clone())
                        .repeated(),
                    |lhs, (op, rhs)| {
                        let span = (lhs.1.start..rhs.1.end).into();
                        (
                            Expr::BinOpExpr {
                                lhs: Box::new(lhs),
                                op: match op {
                                    Token::Mul => BinOp::Mul,
                                    Token::Div => BinOp::Div,
                                    Token::Mod => BinOp::Mod,
                                    _ => unreachable!(),
                                },
                                rhs: Box::new(rhs),
                            },
                            span,
                        )
                    },
                );
                let sum = product.clone().foldl(
                    choice((just(Token::Plus), just(Token::Minus), just(Token::Mod)))
                        .then(product.clone())
                        .repeated(),
                    |lhs, (op, rhs)| {
                        let span = (lhs.1.start..rhs.1.end).into();
                        (
                            Expr::BinOpExpr {
                                lhs: Box::new(lhs),
                                op: match op {
                                    Token::Plus => BinOp::Add,
                                    Token::Minus => BinOp::Sub,
                                    _ => unreachable!(),
                                },
                                rhs: Box::new(rhs),
                            },
                            span,
                        )
                    },
                );
                let comparison = sum.clone().foldl(
                    choice((
                        just(Token::Eq),
                        just(Token::Neq),
                        just(Token::Lt),
                        just(Token::Gt),
                        just(Token::Lte),
                        just(Token::Gte),
                    ))
                    .then(sum.clone())
                    .repeated(),
                    |lhs, (op, rhs)| {
                        let span = (lhs.1.start..rhs.1.end).into();
                        (
                            Expr::BinOpExpr {
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
                            },
                            span,
                        )
                    },
                );
                let r#if = just(Token::If)
                    .ignore_then(expr.clone())
                    .then(block.clone())
                    .then(just(Token::Else).ignore_then(block.clone()).or_not())
                    .map_with_span(|((cond, then), els), span: Span| {
                        if let Some(els) = els {
                            (
                                Expr::If {
                                    cond: Box::new(cond),
                                    then: Box::new(then),
                                    els: Some(Box::new(els)),
                                },
                                span,
                            )
                        } else {
                            (
                                Expr::If {
                                    cond: Box::new(cond),
                                    then: Box::new(then),
                                    els: None,
                                },
                                span,
                            )
                        }
                    });
                comparison.or(sum).or(r#if).or(block
                    .clone()
                    .map_with_span(|x, span| (Expr::Block(Box::new(x)), span)))
            });
            let stmt_let = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Colon))
                .then(ident)
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|((name, ty), rhs), span: Span| {
                    (
                        Stmt::Let {
                            name,
                            ty,
                            rhs: Box::new(rhs),
                        },
                        span,
                    )
                });

            let stmt_return = just(Token::Return)
                .ignore_then(expr.clone().or_not())
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|value, span| {
                    if let Some(e) = value {
                        (Stmt::Return(Some(Box::new(e))), span)
                    } else {
                        (Stmt::Return(None), span)
                    }
                });
            let stmt_while = just(Token::While)
                .ignore_then(expr.clone())
                .then(block.clone())
                .map_with_span(|(cond, body), span: Span| {
                    (
                        Stmt::While {
                            cond: Box::new(cond),
                            body: Box::new(body),
                        },
                        span,
                    )
                });

            let stmt_for = just(Token::For)
                .ignore_then(ident)
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .then_ignore(just(Token::To))
                .then(expr.clone())
                .then(block.clone())
                .map_with_span(|(((var, start), end), body), span: Span| {
                    (
                        Stmt::For {
                            var,
                            start: Box::new(start),
                            end: Box::new(end),
                            body: Box::new(body),
                        },
                        span,
                    )
                });
            let stmt_break = just(Token::Break)
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|_tok, span: Span| (Stmt::Break, span));

            let stmt_continue = just(Token::Continue)
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|_tok, span: Span| (Stmt::Continue, span));

            let stmt_assign = ident
                .clone()
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|(name, rhs), span: Span| {
                    (
                        Stmt::Assign {
                            name,
                            rhs: Box::new(rhs),
                        },
                        span,
                    )
                });
            let stmt_expr = expr
                .clone()
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|x, span| (Stmt::Expr(Box::new(x)), span));

            let stmt = choice((
                stmt_let,
                stmt_return,
                stmt_while,
                stmt_for,
                stmt_break,
                stmt_continue,
                stmt_assign,
                stmt_expr,
            ));

            stmt.clone()
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .or_not()
                .then(expr.clone().or_not())
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with_span(|(stmts, value), span| {
                    (
                        Block {
                            stmts,
                            return_value: value,
                        },
                        span,
                    )
                })
        },
    );

    let r#fn = just(Token::Fn)
        .ignore_then(ident)
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(ident)
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then_ignore(just(Token::Arrow))
        .then(ident)
        .then(block.clone())
        .map_with_span(|(((name, args), return_ty), body), span: Span| {
            (
                FuncDecl {
                    name,
                    args,
                    return_ty,
                    body: Box::new(body),
                },
                span,
            )
        });
    let module = r#fn
        .repeated()
        .collect::<Vec<_>>()
        .map(|defs| Module { func_decls: defs });

    module.then_ignore(end())
}
