use crate::spanned::Spanned;
use serde::{Deserialize, Serialize};

use super::{
    ast_types::Ty,
    lexer::{BinOp, SourceLoc, UnOp},
};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float32(f32),
    Float64(f64),
    Bool(bool),
    String(String),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Member {
    pub target: Box<Spanned<Expr>>,
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Expr {
    // Primitive types
    Literal(Literal),
    Ident(String),

    // Whereas Ident is a direct reference to a variable by name, Member is more broad
    // and usually refers to a field of a struct or enum variant
    Member(Member),

    Reference(Box<Spanned<Expr>>),

    // Operations
    BinOp(Box<Spanned<Expr>>, BinOp, Box<Spanned<Expr>>),
    UnOp(UnOp, Box<Spanned<Expr>>),

    Array(Vec<Spanned<Expr>>),
    Tuple(Vec<Spanned<Expr>>),

    Cast(Box<Spanned<Expr>>, Ty),

    StructCons {
        fields: Vec<(String, Spanned<Expr>)>,
    },
    ArrayIndex {
        array: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },
    Call {
        callee: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
    BlockTerminator,

    Let {
        name: String,
        ty: Option<Ty>,
        value: Option<Spanned<Expr>>,
    },
    Assign {
        name: Spanned<Expr>,
        value: Spanned<Expr>,
    },
    If {
        cond: Spanned<Expr>,
        then_block: Block,
        elif_blocks: Vec<(Spanned<Expr>, Block)>,
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
        body: Block,
    },
}

pub type Block = Vec<Spanned<Stmt>>;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ToplevelStmt {
    Import {
        path: Vec<String>,
        alias: Option<String>,
    },

    Stmt(Spanned<Stmt>),

    EnumDecl {
        name: String,
        fields: Vec<String>,
    },

    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },

    ExternDecl {
        name: String,
        params: Vec<(String, Ty)>,
        return_ty: Ty,
    },

    FunctionDecl {
        name: String,
        params: Vec<(String, Ty)>,
        return_ty: Ty,
        body: Block,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Ast {
    pub comments: Vec<(SourceLoc, String)>,

    // Collection of top-level nodes that make up the AST
    pub nodes: Vec<Spanned<ToplevelStmt>>,
}
