use crate::parser::lexer::{Span, Spanned};

#[derive(Debug)]
pub struct Module<'src> {
    pub func_decls: Vec<Spanned<FuncDecl<'src>>>,
}
// #[derive(Debug)]
// pub struct VarDecl<'src> {
//     pub name: &'src str,
//     pub rhs: Box<Expr<'src>>,
// }
#[derive(Debug)]
pub struct FuncDecl<'src> {
    pub name: &'src str,
    pub args: Vec<&'src str>,
    pub body: Box<Spanned<Block<'src>>>,
}
#[derive(Debug)]
pub struct Block<'src> {
    pub stmts: Option<Vec<Spanned<Stmt<'src>>>>,
    pub return_value: Option<Spanned<Expr<'src>>>,
}

#[derive(Debug)]
pub enum Stmt<'src> {
    Let {
        name: &'src str,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    While {
        cond: Box<Spanned<Expr<'src>>>,
        body: Box<Spanned<Block<'src>>>,
    },
    For {
        var: &'src str,
        start: Box<Spanned<Expr<'src>>>,
        end: Box<Spanned<Expr<'src>>>,
        body: Box<Spanned<Block<'src>>>,
    },
    Return(Option<Box<Spanned<Expr<'src>>>>),
    Break,
    Continue,
    Assign {
        name: &'src str,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    Expr(Box<Spanned<Expr<'src>>>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
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
}
#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug)]
pub enum Expr<'src> {
    Lit(Literal<'src>),
    Var(&'src str),

    BinOpExpr {
        lhs: Box<Spanned<Expr<'src>>>,
        op: BinOp,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    UnOpExpr {
        op: UnOp,
        rhs: Box<Spanned<Expr<'src>>>,
    },

    If {
        cond: Box<Spanned<Expr<'src>>>,
        then: Box<Spanned<Block<'src>>>,
        els: Option<Box<Spanned<Block<'src>>>>,
    },

    Call(&'src str, Vec<Spanned<Expr<'src>>>),

    Bracket(Box<Spanned<Block<'src>>>),
}

#[derive(Debug, Clone)]
pub enum Literal<'src> {
    Int(i32),
    Float(f64),
    Str(&'src str),
    Bool(bool),
    Char(char),
    Unit,
}
