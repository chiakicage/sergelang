pub mod lexer;

use crate::ast::*;
use chumsky::{input::SpannedInput, prelude::*, recursive::Direct};
use lexer::{Error, Span, Spanned, Token};

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [Spanned<Token<'src>>]>;

fn just_span<'tokens, 'src: 'tokens>(
    x: Token<'src>,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Span, Error<'tokens, Token<'src>>> + Clone {
    just(x).map_with_span(|_, span: Span| span)
}

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Module<'src>, Error<'tokens, Token<'src>>> + Clone
{
    let lit = select! {
        Token::Int(x) = span => (Expr::Lit(Literal::Int(x)), span),
        // Token::Str(s) = span => Expr::Lit(Literal::Str(s)).spanned(span)
    }
    .labelled("literal");

    let ident = select! {
        Token::Ident(s) => s
    }
    .labelled("ident");

    let ident_with_span = ident.map_with_span(|token, span: Span| (token, span));

    let var = ident
        .map_with_span(|s, span| (Expr::Var(s), span))
        .labelled("var");

    let block = recursive::<_, _, Error<'tokens, Token<'src>>, _, _>(
        |block: Recursive<Direct<_, Spanned<Block>, _>>| {
            let expr = recursive(|expr| {
                let items = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>();

                let call = ident_with_span
                    .then(
                        items
                            .clone()
                            .delimited_by(just(Token::LParen), just(Token::RParen))
                            .map_with_span(|args, span: Span| (args, span)),
                    )
                    .map(|(f, args)| {
                        let span = (f.1.start..args.1.end).into();
                        (Expr::Call(f.0, args.0), span)
                    });
                let term = call.or(lit).or(var);
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
                let r#if = just_span(Token::If)
                    .then(expr.clone())
                    .then(block.clone())
                    .then(just(Token::Else).ignore_then(block.clone()).or_not())
                    .map(|(((begin, cond), then), els)| {
                        if let Some(els) = els {
                            let span = (begin.start..els.1.end).into();
                            (
                                Expr::If {
                                    cond: Box::new(cond),
                                    then: Box::new(then),
                                    els: Some(Box::new(els)),
                                },
                                span,
                            )
                        } else {
                            let span = (begin.start..then.1.end).into();
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
                    .map(|(x, span)| (Expr::Bracket(Box::new((x, span))), span)))
            });
            let stmt_let = just_span(Token::Let)
                .then(ident)
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then(just_span(Token::Semicolon))
                .map(|(((begin, name), rhs), end)| {
                    let span = (begin.start..end.end).into();
                    (
                        Stmt::Let {
                            name,
                            rhs: Box::new(rhs),
                        },
                        span,
                    )
                });

            let stmt_return = just_span(Token::Return)
                .then(expr.clone().or_not())
                .then(just_span(Token::Semicolon))
                .map(|((begin, value), end)| {
                    let span = (begin.start..end.end).into();
                    if let Some(e) = value {
                        (Stmt::Return(Some(Box::new(e))), span)
                    } else {
                        (Stmt::Return(None), span)
                    }
                });
            let stmt_while = just_span(Token::While)
                .then(expr.clone())
                .then(block.clone())
                .map(|((begin, cond), body)| {
                    let span = (begin.start..body.1.end).into();
                    (
                        Stmt::While {
                            cond: Box::new(cond),
                            body: Box::new(body),
                        },
                        span,
                    )
                });

            let stmt_for = just_span(Token::For)
                .then(ident)
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .then_ignore(just(Token::To))
                .then(expr.clone())
                .then(block.clone())
                .map(|((((begin, var), start), end), body)| {
                    let span = (begin.start..body.1.end).into();
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
            let stmt_break = just_span(Token::Break)
                .then(just_span(Token::Semicolon))
                .map(|(begin, end)| (Stmt::Break, (begin.start..end.end).into()));

            let stmt_continue = just_span(Token::Continue)
                .then(just_span(Token::Semicolon))
                .map(|(begin, end)| (Stmt::Continue, (begin.start..end.end).into()));

            let stmt_assign = ident_with_span
                .clone()
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .map(|(name, rhs)| {
                    let span = (name.1.start..rhs.1.end).into();
                    (
                        Stmt::Assign {
                            name: name.0,
                            rhs: Box::new(rhs),
                        },
                        span,
                    )
                });
            let stmt_expr = expr
                .clone()
                .then(just_span(Token::Semicolon))
                .map(|(x, end)| {
                    let span = (x.1.start..end.end).into();
                    (Stmt::Expr(Box::new(x)), span)
                });

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

            just_span(Token::LBrace)
                .then(
                    stmt.clone()
                        .repeated()
                        .at_least(1)
                        .collect::<Vec<_>>()
                        .or_not()
                        .then(expr.clone().or_not()),
                )
                .then(just_span(Token::RBrace))
                .map(|((begin, (stmts, value)), end)| {
                    let span = (begin.start..end.end).into();
                    (
                        Block {
                            stmts,
                            return_value: value,
                        },
                        span
                    )
                })
        },
    );

    let r#fn = just(Token::Fn)
        .map_with_span(|token, span: Span| (token, span))
        .then(ident)
        .then(
            ident
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(block.clone())
        .map(|(((key, name), args), body)| {
            let span = (key.1.start..body.1.end).into();
            (
                FuncDecl {
                    name,
                    args,
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
