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

    Function(Box<Ty>, Vec<Ty>),
    // Arrays decay into pointers ala C
    Pointer(Box<Ty>),
    Array(Box<Ty>),

    // (ty1, ty2, ...)
    Tuple(Vec<Ty>),
    Struct(String, Vec<(String, Ty)>),
    Enum(String, Vec<String>),
    UserDefined(String),
}

impl Ty {
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Ty::Int | Ty::Float | Ty::Double | Ty::Bool | Ty::String | Ty::Void
        )
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Ty::Pointer(_))
    }

    pub fn is_index_type(&self) -> bool {
        matches!(
            self,
            Ty::Int | Ty::Float | Ty::Double | Ty::Bool | Ty::UserDefined(_)
        )
    }

    pub fn is_indexable_type(&self) -> bool {
        matches!(self, Ty::Array(_) | Ty::Pointer(_) | Ty::UserDefined(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
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
        callee: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),

    VarDecl {
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
    Import { path: Vec<String>, alias: Option<String> },
    Stmt(Spanned<Stmt>),

    EnumDecl {
        name: String,
        fields: Vec<String>,
    },

    StructDecl {
        name: String,
        fields: Vec<(String, Ty)>,
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
