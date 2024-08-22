use crate::syntax::{lexer::SourceLoc, types::Ty};
use thiserror::Error;

/// An error raised as part of either name resolution or typechecking
#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("{location} Symbol '{name}' not in scope")]
    NotInScope { name: String, location: SourceLoc },

    #[error("{location} Redefinition of '{name}' within the same scope")]
    Redefinition { name: String, location: SourceLoc },

    #[error("{location} Expected '{expected:?}', found '{found:?}'")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("{location} Expected arity of '{expected:?}', found '{found:?}'")]
    InvalidArity {
        expected: usize,
        found: usize,
        location: SourceLoc,
    },

    #[error("{location} Invalid cast from '{from:?}' to '{to:?}'")]
    CannotCast {
        from: Ty,
        to: Ty,
        location: SourceLoc,
    },
}
