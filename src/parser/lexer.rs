use chumsky::prelude::*;
use std::fmt;

pub type Span = SimpleSpan<usize>;
pub type Error<'src, T> = extra::Err<Rich<'src, T, Span>>;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    If,
    Else,
    Let,
    For,
    While,
    Fn,
    Return,
    Break,
    Continue,
    Match,
    Import,
    In,
    To,
    As,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    SQuote,
    DQuote,
    Assign,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Not,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitLShift,
    BitRShift,
    Dot,
    Colon,
    Arrow,
    Backslash,
    Underscore,
    Int(i32),
    Str(&'src str),
    Ident(&'src str),
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, Error<'src, char>> {
    let op = choice([
        just('=').then_ignore(just('=')).to(Token::Eq),
        just('!').then_ignore(just('=')).to(Token::Neq),
        just('<').then_ignore(just('=')).to(Token::Lte),
        just('>').then_ignore(just('=')).to(Token::Gte),
        just('&').then_ignore(just('&')).to(Token::And),
        just('|').then_ignore(just('|')).to(Token::Or),
        just('<').then_ignore(just('<')).to(Token::BitLShift),
        just('>').then_ignore(just('>')).to(Token::BitRShift),
        just('-').then_ignore(just('>')).to(Token::Arrow),
        just('.').then_ignore(just('.')).to(Token::To),
    ])
    .or(choice([
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
        just('\'').to(Token::SQuote),
        just('"').to(Token::DQuote),
        just('=').to(Token::Assign),
        just('<').to(Token::Lt),
        just('>').to(Token::Gt),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Mul),
        just('/').to(Token::Div),
        just('%').to(Token::Mod),
        just('!').to(Token::Not),
        just('&').to(Token::BitAnd),
        just('|').to(Token::BitOr),
        just('^').to(Token::BitXor),
        just('~').to(Token::BitNot),
        just('.').to(Token::Dot),
        just(':').to(Token::Colon),
        just('\\').to(Token::Backslash),
        just('_').to(Token::Underscore),
    ]));
    let keyword = choice((
        text::keyword("if").to(Token::If),
        text::keyword("else").to(Token::Else),
        text::keyword("for").to(Token::For),
        text::keyword("while").to(Token::While),
        text::keyword("fn").to(Token::Fn),
        text::keyword("let").to(Token::Let),
        text::keyword("return").to(Token::Return),
        text::keyword("match").to(Token::Match),
        text::keyword("in").to(Token::In),
        text::keyword("break").to(Token::Break),
        text::keyword("continue").to(Token::Continue),
        text::keyword("as").to(Token::As),
        text::keyword("import").to(Token::Import),
    ));
    let num = text::int::<&'src str, char, Error<'src, char>>(10)
        .from_str::<i32>()
        .unwrapped()
        .map(Token::Int);
    let r#str = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_slice(Token::Str);

    let ident = text::ident::<&'src str, char, Error<'src, char>>().map(Token::Ident);

    let token = num.or(op).or(keyword).or(ident).or(r#str);

    let comment = just::<_, &str, Error<'src, char>>("//")
        .then(none_of("\n").repeated())
        .padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect::<Vec<Spanned<Token>>>()
}
