pub(crate) use crate::syntax::ast;
pub(crate) use crate::syntax::errors;

use self::ast::{Expr, ToplevelStmt, Ty};

/// Pair an expression up with its type
#[derive(Debug, Clone)]
pub struct TyExpr {
    pub ty: Ty,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum TyStmt {
    Expr(TyExpr),
    Return(Option<TyExpr>),

    Local {
        name: String,
        ty: Option<Ty>,
        value: Option<TyExpr>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    Assign {
        target: Expr,
        value: TyExpr,
    },
    If {
        cond: TyExpr,
        then_block: TyBlock,
        else_block: Option<TyBlock>,
    },
    For {
        init: String,
        from: TyExpr,
        to: TyExpr,
        body: TyBlock,
    },
    While {
        cond: TyExpr,
        block: TyBlock,
    },
}

pub type TyBlock = Vec<TyStmt>;
pub type TyProgram = Vec<TyTopLevelStmt>;

#[derive(Debug, Clone)]
pub enum TyTopLevelStmt {
    Stmt(TyStmt),
    ToplevelStmt(ToplevelStmt),
}
