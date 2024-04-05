// Typed AST transformation pass
// 1. We take the AST
// 2. We lower it, bit by bit into a typed representation

use crate::syntax::ast::{Block, Expr, Literal, ToplevelStmt, Ty};
use crate::syntax::lexer::{BinOp, UnOp};
use crate::syntax::span::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum TExpr {
    // Primitive types
    Literal(Literal, Ty),
    Variable(String),

    // Operations
    BinOp(Box<Spanned<TExpr>>, BinOp, Box<Spanned<TExpr>>),
    UnOp(UnOp, Box<Spanned<TExpr>>),

    Array {
        elems: Vec<Spanned<TExpr>>,
    },
    Tuple {
        elems: Vec<Spanned<TExpr>>,
    },
    StructCons {
        fields: Vec<(String, Spanned<TExpr>)>,
    },
    ArrayIndex {
        array: Box<Spanned<TExpr>>,
        index: Box<Spanned<TExpr>>,
    },
    Call {
        callee: Box<Spanned<TExpr>>,
        args: Vec<Spanned<TExpr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TStmt {
    Expr(TExpr),
    Return(Option<Spanned<TExpr>>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<Spanned<TExpr>>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        target: TExpr,
        value: Spanned<TExpr>,
    },
    If {
        cond: Spanned<TExpr>,
        then_block: TBlock,
        else_block: Option<TBlock>,
    },
    For {
        init: String,
        from: Spanned<TExpr>,
        to: Spanned<TExpr>,
        body: TBlock,
    },
    While {
        cond: Spanned<TExpr>,
        block: TBlock,
    },
}

pub type TBlock = Vec<Spanned<TStmt>>;
pub type TProgram = Vec<Spanned<TToplevelStmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TToplevelStmt {
    Stmt(Spanned<TStmt>),
    ToplevelStmt(ToplevelStmt),
}
