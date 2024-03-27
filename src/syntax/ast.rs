use super::lexer::{BinOp, UnOp};

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Int,
    Float,
    Double,
    Bool,
    String,
    Void,
    Any,

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
        target: Expr,
        value: Expr,
    },
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    For {
        init: String,
        from: Expr,
        to: Expr,
        body: Block,
    },
    While {
        cond: Expr,
        block: Block,
    },
}

pub type Block = Vec<Stmt>;
pub type Program = Vec<ToplevelStmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelStmt {
    Stmt(Stmt),
    CImport(String), // cimport stdio

    Import {
        path: Vec<String>,
        alias: Option<String>,
    },

    FunctionDecl {
        name: String,
        return_ty: Ty,
        params: Vec<(String, Ty)>,
        body: Block,
    },
}
