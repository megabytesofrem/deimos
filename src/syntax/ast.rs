use super::{
    lexer::{BinOp, SourceLoc, UnOp},
    span::*,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Int,
    Float,
    Double,
    Bool,
    String,
    Void,
    Any,
    Unknown,

    // Compound types
    Function(Box<Ty>, Vec<Ty>), // return type, argument types
    Pointer(Box<Ty>),           // *ty
    Array(Box<Ty>),
    Tuple(Vec<Ty>),                    // (ty1, ty2, ...
    Struct(String, Vec<(String, Ty)>), // name, fields
    UserDefined(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f32),
    Double(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Primitive types
    Literal(Literal),
    Variable(String),

    // Operations
    BinOp(Box<Spanned<Expr>>, BinOp, Box<Spanned<Expr>>),
    UnOp(UnOp, Box<Spanned<Expr>>),

    Array(Vec<Spanned<Expr>>),
    Tuple(Vec<Spanned<Expr>>),

    StructCons {
        fields: Vec<(String, Spanned<Expr>)>,
    },
    ArrayIndex {
        array: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },
    Call {
        func: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<Spanned<Expr>>,
    },

    // TODO: Should this be moved to toplevel?
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        target: Expr,
        value: Spanned<Expr>,
    },
    If {
        cond: Spanned<Expr>,
        then_block: Block,
        else_block: Option<Block>,
    },
    For {
        init: String,
        from: Spanned<Expr>,
        to: Spanned<Expr>,
        body: Block,
    },
    While {
        cond: Spanned<Expr>,
        block: Block,
    },
}

pub type Block = Vec<Spanned<Stmt>>;
pub type Program = Vec<Spanned<ToplevelStmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelStmt {
    Stmt(Spanned<Stmt>),
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
