use crate::utils::Spanned;

use super::lexer::{BinOp, SourceLoc, UnOp};

#[derive(Debug, Clone, PartialEq)]
pub enum Numeric {
    I16,
    I32,
    I64,
    U16,
    U32,
    U64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Number(Numeric),
    Bool,
    Char,
    String,
    Void,
    Unchecked,

    Function(Box<Ty>, Vec<Ty>),
    //Module(ModuleInfo),

    // Arrays decay into pointers ala C
    Pointer(Box<Ty>),
    Array(Box<Ty>),

    // (ty1, ty2, ...)
    Tuple(Vec<Ty>),

    // Optional types are implemented at a compiler level
    Optional(Box<Ty>),

    Struct {
        name: String,
        fields: Vec<(String, Ty)>,
    },

    Enum {
        name: String,
        fields: Vec<String>,
    },

    UserDefined(String),
}

impl Ty {
    pub fn is_primitive(&self) -> bool {
        matches!(self, Ty::Number(_) | Ty::Bool | Ty::Char | Ty::String)
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Ty::Number(_))
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Ty::Pointer(_))
    }

    pub fn is_index_type(&self) -> bool {
        matches!(self, Ty::Number(_) | Ty::UserDefined(_))
    }

    pub fn is_indexable_type(&self) -> bool {
        matches!(self, Ty::Array(_) | Ty::Pointer(_) | Ty::UserDefined(_))
    }

    pub fn can_cast_into(&self, other: &Ty) -> bool {
        match (self, other) {
            (Ty::Number(_), Ty::Number(_)) => true,
            (Ty::Pointer(_), Ty::Pointer(_)) => true,
            (Ty::Array(_), Ty::Array(_)) => true,
            (Ty::UserDefined(_), Ty::UserDefined(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float32(f32),
    Float64(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Primitive types
    Literal(Literal),
    QualifiedName(String),
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

#[derive(Debug, Clone, PartialEq)]
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
        target: Spanned<Expr>,
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
        block: Block,
    },
}

pub type Block = Vec<Spanned<Stmt>>;

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub comments: Vec<(SourceLoc, String)>,
    pub nodes: Vec<Spanned<ToplevelStmt>>,
}
