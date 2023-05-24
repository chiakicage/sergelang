use crate::utils::error::Spanned;

#[derive(Debug)]
pub struct Module<'src> {
    pub decls: Vec<Spanned<Decl<'src>>>,
}

#[derive(Debug)]
pub enum Fields<'src> {
    UnnamedFields(Vec<Spanned<TypeStr<'src>>>),
    NamedFields(Vec<(Spanned<&'src str>, Spanned<TypeStr<'src>>)>),
}

#[derive(Debug)]
pub struct CtorDecl<'src> {
    pub name: Spanned<&'src str>,
    pub fields: Option<Fields<'src>>,
}

#[derive(Debug)]
pub enum Decl<'src> {
    FuncDecl {
        name: Spanned<&'src str>,
        args: Vec<(Spanned<&'src str>, Spanned<TypeStr<'src>>)>,
        return_ty: Option<Spanned<TypeStr<'src>>>,
        body: Box<Spanned<Expr<'src>>>, // only {}
    },
    EnumDecl {
        name: Spanned<&'src str>,
        ctors: Vec<Spanned<CtorDecl<'src>>>,
    },
}

#[derive(Debug)]
pub enum PatternFields<'src> {
    UnnamedFields(Vec<Spanned<Pattern<'src>>>),
    NamedFields(Vec<(Spanned<&'src str>, Option<Spanned<Pattern<'src>>>)>),
}

#[derive(Debug)]
pub enum Pattern<'src> {
    Lit(Literal<'src>),
    Var(Spanned<&'src str>),
    Tuple(Vec<Spanned<Pattern<'src>>>),
    Ctor {
        ty_name: Spanned<&'src str>,
        name: Spanned<&'src str>,
        fields: Option<PatternFields<'src>>,
    },
}

#[derive(Debug)]
pub enum ArgsOrIndex<'src> {
    Args(Vec<Spanned<Expr<'src>>>),
    Index(Box<Spanned<Expr<'src>>>),
}

#[derive(Debug)]
pub enum ExprFields<'src> {
    UnnamedFields(Vec<Spanned<Expr<'src>>>),
    NamedFields(Vec<(Spanned<&'src str>, Option<Spanned<Expr<'src>>>)>),
}

#[derive(Debug)]
pub struct MatchArm<'src> {
    pub pattern: Spanned<Pattern<'src>>,
    pub expr: Spanned<Expr<'src>>,
}

#[derive(Debug)]
pub enum Expr<'src> {
    Lit(Literal<'src>),
    Var(Spanned<&'src str>),
    Tuple(Vec<Spanned<Expr<'src>>>),
    Array(Vec<Spanned<Expr<'src>>>),
    Block(Spanned<Block<'src>>),

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
        then: Box<Spanned<Expr<'src>>>, // only if {} 
        els: Option<Box<Spanned<Expr<'src>>>>, // only else {} or else if 
    },

    Call {
        func: Box<Spanned<Expr<'src>>>,
        args: Vec<Spanned<Expr<'src>>>,
    },
    Index {
        array: Box<Spanned<Expr<'src>>>,
        index: Box<Spanned<Expr<'src>>>,
    },
    Ctor {
        ty_name: Spanned<&'src str>,
        name: Spanned<&'src str>,
        fields: Option<ExprFields<'src>>,
    },
    Match {
        expr: Box<Spanned<Expr<'src>>>,
        arms: Vec<Spanned<MatchArm<'src>>>,
    },
    Closure {
        args: Vec<(Spanned<&'src str>, Spanned<TypeStr<'src>>,)>,
        return_ty: Option<Spanned<TypeStr<'src>>>,
        body: Box<Spanned<Expr<'src>>>, // only {}
    },
    Let {
        name: Spanned<&'src str>,
        ty: Spanned<TypeStr<'src>>,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    While {
        cond: Box<Spanned<Expr<'src>>>,
        body: Box<Spanned<Expr<'src>>>, // only {}
    },
    For {
        var: Spanned<&'src str>,
        start: Box<Spanned<Expr<'src>>>,
        end: Box<Spanned<Expr<'src>>>,
        body: Box<Spanned<Expr<'src>>>, // only {}
    },
    Return(Option<Box<Spanned<Expr<'src>>>>),
    Break,
    Continue,
    Assign {
        name: Spanned<&'src str>,
        rhs: Box<Spanned<Expr<'src>>>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal<'src> {
    Int(i32),
    Float(f64),
    Str(&'src str),
    Bool(bool),
    Char(char),
}
#[derive(Debug)]
pub struct Block<'src>(pub Vec<Spanned<Expr<'src>>>);


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
    And,
    Or,
}
#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug)]
pub enum TypeStr<'src> {
    Tuple(Vec<Spanned<TypeStr<'src>>>),
    Array(Box<Spanned<TypeStr<'src>>>),
    Named(Spanned<&'src str>),
    Func(Vec<Spanned<TypeStr<'src>>>, Box<Spanned<TypeStr<'src>>>),

}
