// Typed HIR (THIR) transformation pass
// 1. We take the AST
// 2. We lower it, bit by bit into a typed HIR (THIR)
// 3. The THIR is a more abstract representation of the AST, and has types attached to it

use crate::syntax::ast::{Block, Expr, Literal, ToplevelStmt, Ty};
use crate::syntax::lexer::{BinOp, UnOp};
use crate::syntax::span::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum HIRExpr {
    // Primitive types
    Literal(Literal, Ty),
    Variable(String),

    // Operations
    BinOp(Box<Spanned<HIRExpr>>, BinOp, Box<Spanned<HIRExpr>>),
    UnOp(UnOp, Box<Spanned<HIRExpr>>),

    Array {
        elems: Vec<Spanned<HIRExpr>>,
    },
    Tuple {
        elems: Vec<Spanned<HIRExpr>>,
    },
    StructCons {
        fields: Vec<(String, Spanned<HIRExpr>)>,
    },
    ArrayIndex {
        array: Box<Spanned<HIRExpr>>,
        index: Box<Spanned<HIRExpr>>,
    },
    Call {
        func: Box<Spanned<HIRExpr>>,
        args: Vec<Spanned<HIRExpr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HIRStmt {
    Expr(HIRExpr),
    Return(Option<Spanned<HIRExpr>>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<Spanned<HIRExpr>>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        target: HIRExpr,
        value: Spanned<HIRExpr>,
    },
    If {
        cond: Spanned<HIRExpr>,
        then_block: HIRBlock,
        else_block: Option<HIRBlock>,
    },
    For {
        init: String,
        from: Spanned<HIRExpr>,
        to: Spanned<HIRExpr>,
        body: HIRBlock,
    },
    While {
        cond: Spanned<HIRExpr>,
        block: HIRBlock,
    },
}

pub type HIRBlock = Vec<Spanned<HIRStmt>>;
pub type HIRProgram = Vec<Spanned<HIRToplevelStmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum HIRToplevelStmt {
    Stmt(HIRStmt),
    ToplevelStmt(ToplevelStmt),
}
