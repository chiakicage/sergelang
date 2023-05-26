use chumsky::prelude::*;
use std::fmt;

use crate::utils::error::{ParserError, Spanned};

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
    Enum,
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
    Scope,
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
    DArrow,
    Backslash,
    // Underscore,
    True,
    False,
    Int(i32),
    Float(f64),
    Str(&'src str),
    Ident(&'src str),
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Match => write!(f, "match"),
            Token::Import => write!(f, "import"),
            Token::Enum => write!(f, "enum"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::In => write!(f, "in"),
            Token::To => write!(f, ".."),
            Token::As => write!(f, "as"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Semicolon => write!(f, ";"),
            Token::Scope => write!(f, "::"),
            Token::Comma => write!(f, ","),
            Token::SQuote => write!(f, "'"),
            Token::DQuote => write!(f, "\""),
            Token::Assign => write!(f, "="),
            Token::Eq => write!(f, "=="),
            Token::Neq => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Lte => write!(f, "<="),
            Token::Gte => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Not => write!(f, "!"),
            Token::BitAnd => write!(f, "&"),
            Token::BitOr => write!(f, "|"),
            Token::BitXor => write!(f, "^"),
            Token::BitNot => write!(f, "~"),
            Token::BitLShift => write!(f, "<<"),
            Token::BitRShift => write!(f, ">>"),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Arrow => write!(f, "->"),
            Token::DArrow => write!(f, "=>"),
            Token::Backslash => write!(f, "\\"),
            // Token::Underscore => write!(f, "_"),
            Token::Int(i) => write!(f, "{}", i),
            Token::Float(j) => write!(f, "{}", j),
            Token::Str(s) => write!(f, "{}", s),
            Token::Ident(s) => write!(f, "{}", s),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, ParserError<'src, char>> {
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
        just('=').then_ignore(just('>')).to(Token::DArrow),
        just(':').then_ignore(just(':')).to(Token::Scope),
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
        // just('_').to(Token::Underscore),
    ]));
    let keyword = choice([
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
        text::keyword("enum").to(Token::Enum),
        text::keyword("true").to(Token::True),
        text::keyword("false").to(Token::False),
    ]);
    let num = text::int::<&'src str, char, ParserError<'src, char>>(10)
        .from_str::<i32>()
        .unwrapped()
        .map(Token::Int);
    let float = text::int::<&'src str, char, ParserError<'src, char>>(10)
        .then_ignore(just("."))
        .then(text::int::<&'src str, char, ParserError<'src, char>>(10))
        .map(|(a, b)| {
            let fl = format!("{}.{}", a.to_owned(), b.to_owned());
            fl.parse::<f64>().unwrap()
        })
        .map(Token::Float);

    let r#str = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_slice(Token::Str);

    let ident = text::ident::<&'src str, char, ParserError<'src, char>>().map(Token::Ident);

    let token = float.or(num).or(op).or(keyword).or(ident).or(r#str);

    let single_comment = just::<_, &str, ParserError<'src, char>>("//")
        .ignore_then(none_of("\n").repeated())
        .then_ignore(just("\n"))
        .padded();

    let multi_comment = just::<_, &str, ParserError<'src, char>>("/*")
        .ignore_then(none_of("*/").repeated())
        .then_ignore(just("*/"))
        .padded();

    let comment = single_comment.or(multi_comment);

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect::<Vec<_>>()
}
