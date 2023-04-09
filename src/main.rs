use chumsky::prelude::*;
use std::{borrow::Borrow, collections::HashMap, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    If,
    Then,
    Else,
    Let,
    For,
    While,
    Fn,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
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
    Int(u64),
    Ident(String),
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let tokens = choice::<_, Simple<char>>([
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
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
    ])
    .or(choice::<_, Simple<char>>((
        text::keyword("if").to(Token::If),
        text::keyword("then").to(Token::Then),
        text::keyword("else").to(Token::Else),
        text::keyword("for").to(Token::For),
        text::keyword("while").to(Token::While),
        text::keyword("fn").to(Token::Fn),
        text::keyword("let").to(Token::Let),
        just('!').then_ignore(just('=')).to(Token::Neq),
        just('<').then_ignore(just('=')).to(Token::Lte),
        just('>').then_ignore(just('=')).to(Token::Gte),
        just('&').then_ignore(just('&')).to(Token::And),
        just('|').then_ignore(just('|')).to(Token::Or),
        just('<').then_ignore(just('<')).to(Token::BitLShift),
        just('>').then_ignore(just('>')).to(Token::BitRShift),
        just('-').then_ignore(just('>')).to(Token::Arrow),
        text::int(10).from_str::<u64>().unwrapped().map(Token::Int),
        text::ident().map(Token::Ident),
    )))
    .padded()
    .repeated();
    tokens.then_ignore(end())
}

#[derive(Debug, Clone)]
enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },
}

type SymTable<K, V> = Vec<HashMap<K, V>>;
#[derive(Debug, Clone, Copy)]
struct FuncEntry<'a> {
    name: &'a String,
    args: &'a Vec<String>,
    body: &'a Expr,
}
pub trait SymbolTable<K, V>
where
    K: Eq + Hash,
{
    fn find_symbol<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>;
    fn insert_symbol(&mut self, key: K, value: V) -> Option<V>;
    fn entry(&mut self) -> isize;
    fn exit(&mut self) -> isize;
    fn level(&self) -> isize;
}

impl<K, V> SymbolTable<K, V> for SymTable<K, V>
where
    K: Eq + Hash,
{
    fn find_symbol<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        for scope in self.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }
        None
    }

    fn insert_symbol(&mut self, key: K, value: V) -> Option<V> {
        self.last_mut().unwrap().insert(key, value)
    }

    fn entry(&mut self) -> isize {
        self.push(HashMap::new());
        self.len() as isize
    }

    fn exit(&mut self) -> isize {
        self.pop();
        self.len() as isize
    }

    fn level(&self) -> isize {
        self.len() as isize - 1
    }
}


fn eval<'a>(
    expr: &'a Expr,
    vars: &mut SymTable<&'a String, f64>,
    funcs: &mut SymTable<&'a String, FuncEntry<'a>>,
) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(a) => Ok(-eval(a, vars, funcs)?),
        Expr::Add(a, b) => Ok(eval(a, vars, funcs)? + eval(b, vars, funcs)?),
        Expr::Sub(a, b) => Ok(eval(a, vars, funcs)? - eval(b, vars, funcs)?),
        Expr::Mul(a, b) => Ok(eval(a, vars, funcs)? * eval(b, vars, funcs)?),
        Expr::Div(a, b) => Ok(eval(a, vars, funcs)? / eval(b, vars, funcs)?),
        Expr::Var(name) => {
            if let Some(value) = vars.find_symbol(name) {
                Ok(*value)
            } else {
                Err(format!("undefined variable: {}", name))
            }
        }
        Expr::Let { name, rhs, then } => {
            let value = eval(rhs, vars, funcs)?;
            vars.entry();
            vars.insert_symbol(name, value);
            let output = eval(then, vars, funcs);
            vars.exit();
            output
        }
        Expr::Fn {
            name,
            args,
            body,
            then,
        } => {
            funcs.entry();
            funcs.insert_symbol(name, FuncEntry { name, args, body });
            let output = eval(then, vars, funcs);
            funcs.exit();
            output
        }
        Expr::Call(name, argexprs) => {
            if let Some(FuncEntry { name, args, body }) = funcs.find_symbol(name).copied() {
                if argexprs.len() == args.len() {
                    let mut args = argexprs
                        .iter()
                        .map(|expr| eval(expr, vars, funcs))
                        .zip(args.iter())
                        .map(|(val, name)| Ok((name, val?)))
                        .collect::<Result<Vec<(&String, f64)>, String>>()?;
                    vars.entry();
                    for (name, val) in args {
                        vars.insert_symbol(name, val);
                    }
                    let output = eval(body, vars, funcs);
                    vars.exit();
                    output
                    // println!("args: {:?}", args);

                    // Ok(2.0)
                } else {
                    Err(format!("wrong number of arguments to function: {}", name))
                }
            } else {
                Err(format!("undefined function: {}", name))
            }
        }
        _ => unreachable!(),
    }
}

fn parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
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

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    // // println!("{:?}", parser().parse(src));
    // match parser().parse(src) {
    //     Ok(ast) => {
    //         // println!("{:#?}", ast);
    //         match eval(&ast, &mut vec![HashMap::new()], &mut vec![HashMap::new()]) {
    //             Ok(x) => println!("{}", x),
    //             Err(e) => println!("Evaluation error: {}", e),
    //         }
    //     }
    //     Err(parse_errs) => parse_errs
    //         .into_iter()
    //         .for_each(|e| println!("Parse error: {}", e)),
    // }

    // use Token::*;
    let result = lexer().parse(src).unwrap();
    println!("{:#?}", result);
    // let result = parser().parse(result).unwrap();
    match parser().parse(result) {
        Ok(ast) => {
            println!("{:#?}", ast);
            match eval(&ast, &mut vec![HashMap::new()], &mut vec![HashMap::new()]) {
                Ok(x) => println!("{}", x),
                Err(e) => println!("Evaluation error: {}", e),
            }
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {:?}", e)),
    }
}
