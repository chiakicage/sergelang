#[derive(Debug, Clone)]
pub struct Module {
    pub func_decls: Vec<FuncDecl>,
}
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub rhs: Box<Expr>,
}
#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub args: Vec<String>,
    pub body: Box<Block>,
}
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Option<Vec<Stmt>>,
    pub return_value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        rhs: Box<Expr>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Block>,
    },
    For {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Box<Block>,
    },
    Return(Option<Box<Expr>>),
    Break,
    Continue,
    Assign,
    Expr(Box<Expr>),
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
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    BitLShift,
    BitRShift,
}
#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone)]
pub enum Expr {

    Num(f64),
    Var(String),

    BinOpExpr {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    UnOpExpr {
        op: UnOp,
        rhs: Box<Expr>,
    },

    If {
        cond: Box<Expr>,
        then: Box<Block>,
        els: Option<Box<Block>>,
    },

    Call(String, Vec<Expr>),

    Braket(Box<Block>)
}

pub enum Literal {
    Int(i32),
    Float(f64),
    Str(String),
    Bool(bool),
    Char(char),
    Unit,
}
