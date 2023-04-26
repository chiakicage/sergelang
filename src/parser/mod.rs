pub mod lexer;

use crate::ast::ast::*;
use crate::error::{Error, Span, Spanned};
use chumsky::{input::SpannedInput, prelude::*, recursive::Direct};
use lexer::Token;

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [Spanned<Token<'src>>]>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Module<'src>, Error<'tokens, Token<'src>>> + Clone
{
    let lit = select! {
        Token::Int(x) => Literal::Int(x),
        // Token::Str(s) = span => Expr::Lit(Literal::Str(s)).(span)
    }
    .labelled("literal");

    let ident = select! {
        Token::Ident(s) = span => (s, span)
    }
    .labelled("ident");

    let var = ident
        .map_with_span(|s, span: Span| (Expr::Var(s), span))
        .labelled("var");

    let lit = lit.map_with_span(|lit, span| (Expr::Lit(lit), span));

    let typed_ctor = ident
        .then_ignore(just(Token::Scope))
        .then(ident)
        .map(|(ty, ctor)| (ty, ctor));

    // let pattern = recursive(|pattern| {
    //     let items = pattern
    //         .clone()
    //         .separated_by(just(Token::Comma))
    //         .allow_trailing()
    //         .collect::<Vec<_>>();

    //     let tuple = items
    //         .delimited_by(just(Token::LParen), just(Token::RParen))
    //         .map_with_span(|args, span: Span| (Pattern::Tuple(args), span));

    //     let nameless_ctor = typed_ctor
    //         .then(
    //             items
    //                 .clone()
    //                 .delimited_by(just(Token::LParen), just(Token::RParen)),
    //         )
    //         .map_with_span(|((ty_name, name), fields), span| {
    //             (
    //                 Pattern::NamelessCtorPattern(NamelessCtorPattern {
    //                     ty_name,
    //                     name,
    //                     fields
    //                 }),
    //                 span,
    //             )
    //         });

    //     let named_fields = ident
    //         .then(just(Token::Colon).ignore_then(pattern.clone()).or_not())
    //         .separated_by(just(Token::Comma))
    //         .allow_trailing()
    //         .collect::<Vec<_>>();

    //     let named_ctor = typed_ctor
    //         .then(named_fields.delimited_by(just(Token::LBrace), just(Token::RBrace)))
    //         .map_with_span(|((ty_name, name), fields), span| {
    //             (
    //                 Pattern::NamedCtorPattern(NamedCtorPattern {
    //                     ty_name,
    //                     name,
    //                     fields
    //                 }),
    //                 span,
    //             )
    //         });

    //     let ctor = nameless_ctor.or(named_ctor);

    //     let lit = lit.map_with_span(|lit, span| (Pattern::Lit(lit), span));

    //     let var = ident.map_with_span(|var, span| (Pattern::Var(var), span));

    //     choice((tuple, ctor, lit, var));
    // });

    let block = recursive::<_, _, Error<'tokens, Token<'src>>, _, _>(
        |block: Recursive<Direct<_, Spanned<Block>, _>>| {
            let expr = recursive(|expr| {
                let r#if = just(Token::If)
                    .ignore_then(expr.clone())
                    .then(block.clone())
                    .then(just(Token::Else).ignore_then(block.clone()).or_not())
                    .map_with_span(|((cond, then), els), span| {
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

                let paren = expr
                    .clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen));

                let tuple = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .map_with_span(|args, span: Span| (Expr::Tuple(args), span));

                let items = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen));

                let expr_block = block
                    .clone()
                    .map_with_span(|x, span| (Expr::Block(Box::new(x)), span));

                let callable = choice((var, paren.clone(), expr_block.clone(), r#if.clone()));

                let index = expr
                    .clone()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .map(|exp| ArgsOrIndex::Index(Box::new(exp)));

                let args = items.clone().map(|args| ArgsOrIndex::Args(args));

                let call = callable.clone().foldl(
                    args.or(index)
                        .clone()
                        .map_with_span(|x, span: Span| (x, span))
                        .repeated()
                        .at_least(1),
                    |lhs, (argsorindex, span)| {
                        let span = (lhs.1.start..span.end).into();
                        (
                            match argsorindex {
                                ArgsOrIndex::Index(index) => Expr::Index {
                                    array: Box::new(lhs),
                                    index,
                                },
                                ArgsOrIndex::Args(args) => Expr::Call {
                                    func: Box::new(lhs),
                                    args,
                                },
                            },
                            span,
                        )
                    },
                );

                let named_fields = ident
                    .then(just(Token::Colon).ignore_then(expr.clone()).or_not())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map(|fields| ExprFields::NamedFields(fields));
                let nameless_fields = items.map(|fields| ExprFields::NamelessFields(fields));

                let fields = named_fields.or(nameless_fields);

                let ctor =
                    typed_ctor
                        .then(fields)
                        .map_with_span(|((ty_name, name), fields), span| {
                            (
                                Expr::Ctor {
                                    ty_name,
                                    name,
                                    fields,
                                },
                                span,
                            )
                        });

                let term = choice((call, lit, var, paren));

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

                choice((comparison, ctor, r#if, expr_block, tuple))
            });
            let stmt_let = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Colon))
                .then(ident)
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::Semicolon))
                .map_with_span(|((name, ty), rhs), span| {
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
                .map_with_span(|(cond, body), span| {
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
                .map_with_span(|(((var, start), end), body), span| {
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
                .map_with_span(|(name, rhs), span| {
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
        .map_with_span(|(((name, args), return_ty), body), span| {
            (
                Decl::FuncDecl {
                    name,
                    args,
                    return_ty,
                    body: Box::new(body),
                },
                span,
            )
        });
    let nameless_fields = ident
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .map(|fields| Fields::NamelessFields(fields));
    let named_fields = ident
        .then(just(Token::Colon).ignore_then(ident))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(|fields| Fields::NamedFields(fields));

    let fields = choice((nameless_fields, named_fields));
    let ctor = ident
        .then(fields.clone())
        .map_with_span(|(name, fields), span| (CtorDecl { name, fields }, span));
    let r#type = just(Token::Type)
        .ignore_then(ident)
        .then(
            ctor.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with_span(|(name, ctors), span| (Decl::TypeDecl { name, ctors }, span));

    let decls = choice((r#fn, r#type));
    let module = decls
        .repeated()
        .collect::<Vec<_>>()
        .map(|decls| Module { decls });

    module.then_ignore(end())
}
