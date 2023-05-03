pub mod lexer;

use crate::ast::ast::*;
use crate::utils::error::{ParserError, Span, Spanned};
use chumsky::pratt::{Associativity, InfixOperator, InfixPrecedence};
use chumsky::{input::SpannedInput, prelude::*, recursive::Direct};
use lexer::Token;

#[derive(Clone, Copy, Debug)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

impl<'src> InfixOperator<Spanned<Expr<'src>>> for Operator {
    type Strength = u8;

    fn precedence(&self) -> InfixPrecedence<Self::Strength> {
        match self {
            Self::And => InfixPrecedence::new(0, Associativity::Left),
            Self::Or => InfixPrecedence::new(0, Associativity::Left),
            Self::Eq => InfixPrecedence::new(1, Associativity::Left),
            Self::Neq => InfixPrecedence::new(1, Associativity::Left),
            Self::Lt => InfixPrecedence::new(1, Associativity::Left),
            Self::Gt => InfixPrecedence::new(1, Associativity::Left),
            Self::Lte => InfixPrecedence::new(1, Associativity::Left),
            Self::Gte => InfixPrecedence::new(1, Associativity::Left),
            Self::Add => InfixPrecedence::new(2, Associativity::Left),
            Self::Sub => InfixPrecedence::new(2, Associativity::Left),
            Self::Mul => InfixPrecedence::new(3, Associativity::Left),
            Self::Div => InfixPrecedence::new(3, Associativity::Left),
            Self::Mod => InfixPrecedence::new(3, Associativity::Left),
        }
    }

    fn build_expression(
        self,
        left: Spanned<Expr<'src>>,
        right: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        let (lhs, rhs) = (Box::new(left), Box::new(right));
        let span = (lhs.1.start..rhs.1.end).into();
        (
            match self {
                Self::And => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::And,
                    rhs,
                },
                Self::Or => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Or,
                    rhs,
                },
                Self::Eq => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Eq,
                    rhs,
                },
                Self::Neq => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Neq,
                    rhs,
                },
                Self::Lt => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Lt,
                    rhs,
                },
                Self::Gt => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Gt,
                    rhs,
                },
                Self::Lte => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Lte,
                    rhs,
                },
                Self::Gte => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Gte,
                    rhs,
                },
                Self::Add => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Add,
                    rhs,
                },
                Self::Sub => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Sub,
                    rhs,
                },
                Self::Mul => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Mul,
                    rhs,
                },
                Self::Div => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Div,
                    rhs,
                },
                Self::Mod => Expr::BinOpExpr {
                    lhs,
                    op: BinOp::Mod,
                    rhs,
                },
            },
            span,
        )
    }
}

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [Spanned<Token<'src>>]>;

pub fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Module<'src>>,
    ParserError<'tokens, Token<'src>>,
> + Clone {
    let lit = select! {
        Token::Int(x) => Literal::Int(x),
        // Token::Str(s) = span => Expr::Lit(Literal::Str(s)).(span)
    }
    .labelled("literal");

    let ident = select! {
        Token::Ident(s) = span => (s, span)
    }
    .labelled("ident");

    let typed_ctor = ident
        .then_ignore(just(Token::Scope))
        .then(ident)
        .map(|(ty, ctor)| (ty, ctor));

    let pattern = recursive::<_, _, ParserError<'tokens, Token<'src>>, _, _>(
        |pattern: Recursive<Direct<_, Spanned<Pattern>, _>>| {
            let items = pattern
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let tuple = items
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .map_with_span(|args, span: Span| (Pattern::Tuple(args), span));

            let nameless_fields = items
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .map(|args| PatternFields::NamelessFields(args));

            let named_fields = ident
                .then(just(Token::Colon).ignore_then(pattern.clone()).or_not())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(|args| PatternFields::NamedFields(args));

            let ctor = typed_ctor
                .clone()
                .then(nameless_fields.or(named_fields).or_not())
                .map_with_span(|((ty_name, name), fields), span| {
                    (
                        Pattern::Ctor {
                            ty_name,
                            name,
                            fields,
                        },
                        span,
                    )
                });

            let lit = lit.map_with_span(|lit, span: Span| (Pattern::Lit(lit), span));

            let var = ident.map_with_span(|var, span: Span| (Pattern::Var(var), span));

            choice((tuple, ctor, lit, var))
        },
    );

    let r#type = recursive(|r#type| {
        let tuple = r#type
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with_span(|args, span: Span| (TypeRef::Tuple(args), span));
        let array = r#type
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with_span(|ty, span: Span| (TypeRef::Array(Box::new(ty)), span));
        let named = ident.map_with_span(|name, span: Span| (TypeRef::Named(name), span));
        let func = just(Token::Fn)
            .ignore_then(
                r#type
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then_ignore(just(Token::Arrow))
            .then(r#type.clone())
            .map_with_span(|(args, ret), span| (TypeRef::Func(args, Box::new(ret)), span));
        choice((tuple, array, named, func))
    });

    let block = recursive::<_, _, ParserError<'tokens, Token<'src>>, _, _>(
        |block: Recursive<Direct<_, Spanned<Block>, _>>| {
            let expr = recursive(|expr| {
                let var = ident
                    .map_with_span(|s, span: Span| (Expr::Var(s), span))
                    .labelled("var");

                let lit = lit.map_with_span(|lit, span| (Expr::Lit(lit), span));

                let expr_block = block
                    .clone()
                    .map_with_span(|x, span| (Expr::Block(x), span));

                let r#if = recursive(|r#if: Recursive<Direct<_, Spanned<Expr>, _>>| {
                    just(Token::If)
                        .ignore_then(expr.clone())
                        .then(expr_block.clone())
                        .then(just(Token::Else).ignore_then(expr_block.clone().or(r#if)).or_not())
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
                        })
                });

                let match_arm = pattern
                    .clone()
                    .then(just(Token::DArrow).ignore_then(expr.clone()))
                    .map_with_span(|(pattern, expr), span| (MatchArm { pattern, expr }, span));

                let r#match = just(Token::Match)
                    .ignore_then(expr.clone())
                    .then(
                        match_arm
                            .clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                    )
                    .map_with_span(|(expr, arms), span: Span| {
                        (
                            Expr::Match {
                                expr: Box::new(expr),
                                arms,
                            },
                            span,
                        )
                    });

                let r#let = just(Token::Let)
                    .ignore_then(ident)
                    .then_ignore(just(Token::Colon))
                    .then(r#type.clone())
                    .then_ignore(just(Token::Assign))
                    .then(expr.clone())
                    .then_ignore(just(Token::Semicolon))
                    .map_with_span(|((name, ty), rhs), span| {
                        (
                            Expr::Let {
                                name,
                                ty,
                                rhs: Box::new(rhs),
                            },
                            span,
                        )
                    });

                let r#return = just(Token::Return)
                    .ignore_then(expr.clone().or_not())
                    .then_ignore(just(Token::Semicolon))
                    .map_with_span(|value, span| {
                        if let Some(e) = value {
                            (Expr::Return(Some(Box::new(e))), span)
                        } else {
                            (Expr::Return(None), span)
                        }
                    });

                let r#while = just(Token::While)
                    .ignore_then(expr.clone())
                    .then(expr_block.clone())
                    .map_with_span(|(cond, body), span| {
                        (
                            Expr::While {
                                cond: Box::new(cond),
                                body: Box::new(body),
                            },
                            span,
                        )
                    });

                let r#for = just(Token::For)
                    .ignore_then(ident)
                    .then_ignore(just(Token::In))
                    .then(expr.clone())
                    .then_ignore(just(Token::To))
                    .then(expr.clone())
                    .then(expr_block.clone())
                    .map_with_span(|(((var, start), end), body), span| {
                        (
                            Expr::For {
                                var,
                                start: Box::new(start),
                                end: Box::new(end),
                                body: Box::new(body),
                            },
                            span,
                        )
                    });

                let r#break = just(Token::Break)
                    .then_ignore(just(Token::Semicolon))
                    .map_with_span(|_tok, span: Span| (Expr::Break, span));

                let assign = ident
                    .clone()
                    .then_ignore(just(Token::Assign))
                    .then(expr.clone())
                    .then_ignore(just(Token::Semicolon))
                    .map_with_span(|(name, rhs), span| {
                        (
                            Expr::Assign {
                                name,
                                rhs: Box::new(rhs),
                            },
                            span,
                        )
                    });

                let r#continue = just(Token::Continue)
                    .then_ignore(just(Token::Semicolon))
                    .map_with_span(|_tok, span: Span| (Expr::Continue, span));

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

                let array = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .map_with_span(|args, span: Span| (Expr::Array(args), span));

                let closure = just(Token::BitOr)
                    .ignore_then(
                        ident
                            .then_ignore(just(Token::Colon))
                            .then(r#type.clone())
                            .separated_by(just(Token::Comma))
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::BitOr))
                    .then(just(Token::Arrow).ignore_then(r#type.clone()).or_not())
                    .then(expr_block.clone())
                    .map_with_span(|((args, return_ty), body), span: Span| {
                        (
                            Expr::Closure {
                                args,
                                return_ty,
                                body: Box::new(body),
                            },
                            span,
                        )
                    });

                let items = expr
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen));

                let callable = choice((
                    var,
                    closure.clone(),
                    tuple.clone(),
                    array.clone(),
                    paren.clone(),
                    expr_block.clone(),
                    r#if.clone(),
                    r#match.clone(),
                ));

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

                let ctor = typed_ctor.then(fields.or_not()).map_with_span(
                    |((ty_name, name), fields), span| {
                        (
                            Expr::Ctor {
                                ty_name,
                                name,
                                fields,
                            },
                            span,
                        )
                    },
                );

                let term = choice((call, lit, var, paren));

                let unary = choice((just(Token::Minus), just(Token::Not), just(Token::BitNot)))
                    .map_with_span(|token, span: Span| (token, span))
                    .repeated()
                    .foldr(term.clone(), |op, rhs: Spanned<Expr>| {
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
                let operator = choice((
                    just(Token::Plus).to(Operator::Add),
                    just(Token::Minus).to(Operator::Sub),
                    just(Token::Mul).to(Operator::Mul),
                    just(Token::Div).to(Operator::Div),
                    just(Token::Mod).to(Operator::Mod),
                    just(Token::And).to(Operator::And),
                    just(Token::Or).to(Operator::Or),
                    just(Token::Eq).to(Operator::Eq),
                    just(Token::Neq).to(Operator::Neq),
                    just(Token::Lt).to(Operator::Lt),
                    just(Token::Gt).to(Operator::Gt),
                    just(Token::Lte).to(Operator::Lte),
                    just(Token::Gte).to(Operator::Gte),
                ));
                let binop = unary.pratt(operator);

                choice((
                    binop, array, tuple, term, ctor, r#if, r#match, expr_block, closure, r#let,
                    r#return, r#while, r#for, r#break, r#continue, assign,
                ))
            });

            expr.clone()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with_span(|exprs, span| (Block(exprs), span))
        },
    );

    let r#fn = just(Token::Fn)
        .ignore_then(ident)
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(r#type.clone())
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(just(Token::Arrow).ignore_then(r#type.clone()).or_not())
        .then(block.clone())
        .map_with_span(|(((name, args), return_ty), body), span| {
            (
                Decl::FuncDecl {
                    name,
                    args,
                    return_ty,
                    body: Box::new((Expr::Block(body), span)),
                },
                span,
            )
        });
    let nameless_fields = r#type
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .map(|fields| Fields::NamelessFields(fields));
    let named_fields = ident
        .then(just(Token::Colon).ignore_then(r#type.clone()))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(|fields| Fields::NamedFields(fields));

    let fields = choice((nameless_fields, named_fields));
    let ctor = ident
        .then(fields.clone().or_not())
        .map_with_span(|(name, fields), span| (CtorDecl { name, fields }, span));
    let r#enum = just(Token::Enum)
        .ignore_then(ident)
        .then(
            ctor.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with_span(|(name, ctors), span| (Decl::EnumDecl { name, ctors }, span));

    let decls = choice((r#fn, r#enum));
    let module = decls
        .repeated()
        .collect::<Vec<_>>()
        .map_with_span(|decls, span: Span| (Module { decls }, span));

    module.then_ignore(end())
}
