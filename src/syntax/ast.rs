use super::lexer::{BinOp, UnOp};

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Int,
    Float,
    Double,
    Bool,
    String,
    Void,

    // Compound types
    Function(Box<Ty>, Vec<Ty>), // return type, argument types
    Pointer(Box<Ty>),           // *ty
    Array(Box<Ty>),
    Struct(String, Vec<(String, Ty)>), // name, fields
    UserDefined(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Primitive types
    Int(i64),
    Float(f32),
    Double(f64),
    Bool(bool),
    String(String),
    Variable(String),

    Array(Vec<Expr>),

    // Struct constructor
    StructCons { fields: Vec<(String, Expr)> },

    // Operations
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    ArrayIndex { array: Box<Expr>, index: Box<Expr> },
    Call { func: Box<Expr>, args: Vec<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Return(Option<Expr>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<Expr>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        lhs: Expr,
        rhs: Expr,
    },
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    For {
        init: String,
        from: Expr,
        to: Expr,
        body: Block,
    },
    While {
        cond: Expr,
        block: Vec<Stmt>,
    },
}

pub type Block = Vec<Stmt>;
pub type Program = Vec<ToplevelStmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelStmt {
    Stmt(Stmt),

    FunctionDecl {
        name: String,
        return_ty: Ty,
        params: Vec<(String, Ty)>,
        body: Block,
    },
}
