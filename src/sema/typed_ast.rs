use crate::spanned::Spanned;
/// Typed AST representation
use crate::syntax::ast::{Literal, Member};
use crate::syntax::ast_types::Ty;
use crate::syntax::lexer::Op;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum TExpr {
    // Primitive types
    Literal(Literal, Ty),
    Name(String, Ty),

    // Whereas Ident is a direct reference to a variable by name, Member is more broad
    // and usually refers to a field of a struct or enum variant
    Member(Member, Ty),

    Reference(Box<Spanned<TExpr>>),

    // Operations
    BinOp(Box<Spanned<TExpr>>, Op, Box<Spanned<TExpr>>),
    UnOp(Op, Box<Spanned<TExpr>>),

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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct TypedAst {
    pub nodes: Vec<Spanned<TToplevelStmt>>,
}

impl TypedAst {
    // Dump the AST as a YAML string using serde-yml for debugging
    pub fn dump_yaml(&self) -> String {
        serde_yml::to_string(&self).unwrap()
    }
}
