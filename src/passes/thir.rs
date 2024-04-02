// Typed HIR (THIR) transformation pass
// 1. We take the AST
// 2. We lower it, bit by bit into a typed HIR (THIR)
// 3. The THIR is a more abstract representation of the AST, and has types attached to it

use crate::syntax::ast::{Block, Expr, Literal, ToplevelStmt, Ty};
use crate::syntax::lexer::{BinOp, UnOp};
use crate::syntax::span::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum THIRExpr {
    // Primitive types
    Literal(Literal, Ty),
    Variable(String),

    // Operations
    BinOp(Box<Spanned<THIRExpr>>, BinOp, Box<Spanned<THIRExpr>>),
    UnOp(UnOp, Box<Spanned<THIRExpr>>),

    Array {
        elems: Vec<Spanned<THIRExpr>>,
    },
    Tuple {
        elems: Vec<Spanned<THIRExpr>>,
    },
    StructCons {
        fields: Vec<(String, Spanned<THIRExpr>)>,
    },
    ArrayIndex {
        array: Box<Spanned<THIRExpr>>,
        index: Box<Spanned<THIRExpr>>,
    },
    Call {
        callee: Box<Spanned<THIRExpr>>,
        args: Vec<Spanned<THIRExpr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum THIRStmt {
    Expr(THIRExpr),
    Return(Option<Spanned<THIRExpr>>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<Spanned<THIRExpr>>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        target: THIRExpr,
        value: Spanned<THIRExpr>,
    },
    If {
        cond: Spanned<THIRExpr>,
        then_block: THIRBlock,
        else_block: Option<THIRBlock>,
    },
    For {
        init: String,
        from: Spanned<THIRExpr>,
        to: Spanned<THIRExpr>,
        body: THIRBlock,
    },
    While {
        cond: Spanned<THIRExpr>,
        block: THIRBlock,
    },
}

pub type THIRBlock = Vec<Spanned<THIRStmt>>;
pub type THIRProgram = Vec<Spanned<THIRToplevelStmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum THIRToplevelStmt {
    Stmt(THIRStmt),
    ToplevelStmt(ToplevelStmt),
}
