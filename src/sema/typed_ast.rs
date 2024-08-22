/// Typed AST representation
use crate::syntax::ast::Literal;
use crate::syntax::lexer::{BinOp, UnOp};
use crate::syntax::types::Ty;
use crate::utils::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum TExpr {
    // Primitive types
    Literal(Literal, Ty),
    Name(String, Ty),

    // Operations
    BinOp(Box<Spanned<TExpr>>, BinOp, Box<Spanned<TExpr>>),
    UnOp(UnOp, Box<Spanned<TExpr>>),

    Array(Vec<Spanned<TExpr>>),
    Tuple(Vec<Spanned<TExpr>>),

    Cast(Box<Spanned<TExpr>>, Ty),

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
    BlockTerminator,

    Let {
        name: String,
        ty: Option<Ty>,
        value: Option<Spanned<TExpr>>,
    },
    Assign {
        name: TExpr,
        value: Spanned<TExpr>,
    },
    If {
        cond: Spanned<TExpr>,
        then_block: TBlock,
        elif_blocks: Vec<(Spanned<TExpr>, TBlock)>,
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
        body: TBlock,
    },
}

pub type TBlock = Vec<Spanned<TStmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TToplevelStmt {
    Import {
        path: Vec<String>,
        alias: Option<String>,
    },
    Stmt(Spanned<TStmt>),

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
        return_type: Ty,
    },

    FunctionDecl {
        name: String,
        params: Vec<(String, Ty)>,
        return_ty: Ty,
        body: TBlock,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedAst {
    pub nodes: Vec<Spanned<TToplevelStmt>>,
}
