use crate::parser::lexer::{Span, Spanned};

#[derive(Debug)]
pub struct Module<'src> {
    pub func_decls: Vec<Spanned<FuncDecl<'src>>>,
}
#[derive(Debug)]
pub struct FuncDecl<'src> {
    pub name: Spanned<&'src str>,
    pub args: Vec<(Spanned<&'src str>, Spanned<&'src str>)>,
    pub return_ty: Spanned<&'src str>,
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
        name: Spanned<&'src str>,
        ty: Spanned<&'src str>,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    While {
        cond: Box<Spanned<Expr<'src>>>,
        body: Box<Spanned<Block<'src>>>,
    },
    For {
        var: Spanned<&'src str>,
        start: Box<Spanned<Expr<'src>>>,
        end: Box<Spanned<Expr<'src>>>,
        body: Box<Spanned<Block<'src>>>,
    },
    Return(Option<Box<Spanned<Expr<'src>>>>),
    Break,
    Continue,
    Assign {
        name: Spanned<&'src str>,
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
pub struct CtorDecl<'src> {
    pub name: Spanned<&'src str>,
    pub fields: Option<Vec<(Option<Spanned<&'src str>>, Spanned<&'src str>)>>,
}
#[derive(Debug)]
pub struct TypeDecl<'src> {
    pub name: Spanned<&'src str>,
    pub ctors: Vec<Spanned<CtorDecl<'src>>>,
}
#[derive(Debug)]
pub enum Pattern<'src> {
    Lit(Literal<'src>),
    Var(Spanned<&'src str>),
    Tuple(Vec<Spanned<Pattern<'src>>>),
    Ctor {
        ty_name: Spanned<&'src str>,
        name: Spanned<&'src str>,
        fields: Vec<(Option<Spanned<&'src str>>, Spanned<Pattern<'src>>)>,
	}
}

#[derive(Debug)]
pub enum Expr<'src> {
    Lit(Literal<'src>),
    Var(Spanned<&'src str>),
    Tuple(Vec<Spanned<Expr<'src>>>),

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

    Call(Spanned<&'src str>, Vec<Spanned<Expr<'src>>>),
    Ctor {
		ty_name: Spanned<&'src str>,
        name: Spanned<&'src str>,
        fields: Vec<(Option<Spanned<&'src str>>, Spanned<Expr<'src>>)>,
	},

    Block(Box<Spanned<Block<'src>>>),
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
