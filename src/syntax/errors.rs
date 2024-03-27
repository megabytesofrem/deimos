use crate::syntax::lexer::{SourceLoc, TokenKind};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SyntaxError {
    #[error("Unexpected token {token:?} at {location:?}")]
    UnexpectedToken {
        token: TokenKind,
        location: SourceLoc,
    },

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("Expected an expression")]
    ExpectedExpr { location: SourceLoc },

    #[error("Expected an identifier")]
    ExpectedIdent { location: SourceLoc },

    #[error("Expected a statement")]
    ExpectedStmt { location: SourceLoc },

    #[error("Expected an array")]
    ExpectedArray { location: SourceLoc },

    #[error("Expected one of {expected:?}, found {found:?}")]
    ExpectedOneOf {
        expected: Vec<TokenKind>,
        found: TokenKind,
        location: SourceLoc,
    },

    #[error("Invalid character in path")]
    InvalidPathChar { location: SourceLoc },

    #[error("Cannot index into non-array type {ty:?}")]
    CannotIndexIntoNonArray { ty: String },

    #[error("Unmatched quotes in string literal")]
    UnmatchedQuotes { location: SourceLoc },

    #[error("Unmatched brackets in expression")]
    UnmatchedBrackets { location: SourceLoc },
}
