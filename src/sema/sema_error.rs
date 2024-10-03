use crate::syntax::{ast_types::Ty, lexer::SourceLoc};
use thiserror::Error;

/// An error raised as part of either name resolution or typechecking
#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("{location} Symbol '{name}' not in scope")]
    NotInScope { name: String, location: SourceLoc },

    #[error("{location} Redefinition of '{name}' within the same scope")]
    Redefinition { name: String, location: SourceLoc },

    #[error("{location} Field '{field}' not found in struct/enum '{struct_name}'")]
    FieldNotFound {
        struct_name: String,
        field: String,
        location: SourceLoc,
    },

    #[error("{location} Expected a struct or enum, found '{found:?}'")]
    NotAValidStructure { found: Ty, location: SourceLoc },

    #[error("{location} Expected '{expected:?}', found '{found:?}'")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("{location} Infinite type expansion (occurs check)")]
    InfTypeExpansion { location: SourceLoc },

    #[error("{location} Expected arity of '{expected:?}', found '{found:?}'")]
    InvalidArity {
        expected: usize,
        found: usize,
        location: SourceLoc,
    },

    #[error("{location} Return outside of function")]
    ReturnOutsideOfFunction { location: SourceLoc },

    #[error("{location} Invalid cast from '{from:?}' to '{to:?}'")]
    CannotCast {
        from: Ty,
        to: Ty,
        location: SourceLoc,
    },
}
