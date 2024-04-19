use thiserror::Error;

use crate::syntax::lexer::{SourceLoc, TokenKind};

#[derive(Error, Debug, Clone)]
pub enum SyntaxError {
    #[error("{location:?} Unexpected token '{token:?}'")]
    UnexpectedToken {
        token: TokenKind,
        location: SourceLoc,
    },

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("{location:?} Expected an expression")]
    ExpectedExpr { location: SourceLoc },

    #[error("{location:?} Expected an identifier")]
    ExpectedIdent { location: SourceLoc },

    #[error("{location:?} Expected a statement")]
    ExpectedStmt { location: SourceLoc },

    #[error("{location:?} Expected an array")]
    ExpectedArray { location: SourceLoc },

    #[error("{location:?} Expected one of '{expected:?}', found '{found:?}'")]
    ExpectedOneOf {
        expected: Vec<TokenKind>,
        found: TokenKind,
        location: SourceLoc,
    },

    #[error("{location:?} Invalid character in path")]
    InvalidPathChar { location: SourceLoc },

    #[error("Cannot index into non-array type '{ty:?}'")]
    CannotIndexIntoNonArray { ty: String },

    #[error("{location:?} Unmatched quotes in string literal")]
    UnmatchedQuotes { location: SourceLoc },

    #[error("{location:?} Unmatched brackets in expression")]
    UnmatchedBrackets { location: SourceLoc },
}

