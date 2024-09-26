//! The main parser module for Deimos.

use crate::spanned::spanned;
use crate::spanned::Spanned;
use crate::syntax::ast::{Ast, ToplevelStmt};
use crate::syntax::lexer::{LexerIter, SourceLoc, Token, TokenKind};
use crate::syntax::parser::syntax_error::SyntaxError;

mod expr;
mod stmt;
mod syntax_error;
mod tidbits;

pub mod parser_tests;

/// Result type for parsing
pub(crate) type Return<'p, T> = anyhow::Result<T, SyntaxError>;
pub(crate) type ReturnErrors<'p, T> = anyhow::Result<T, Vec<SyntaxError>>;

#[derive(Clone)]
pub struct Parser<'p> {
    tokens: LexerIter<'p>,
    errors: Vec<SyntaxError>,

    line: usize,
    col: usize,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: LexerIter<'p>) -> Self {
        Parser {
            tokens,
            errors: Vec::new(),
            line: 1,
            col: 0,
        }
    }

    pub fn errors(&self) -> &Vec<SyntaxError> {
        &self.errors
    }

    /// Peek at the next token
    pub(crate) fn peek(&mut self) -> Option<Token<'p>> {
        self.tokens.peek().cloned()
    }

    /// Advance the parser by one token
    pub(crate) fn advance(&mut self) -> Option<Token<'p>> {
        self.col += 1;
        let token = self.tokens.next();
        token
    }

    /// Check if the next token is of the expected kind without advancing
    pub(crate) fn check(&mut self, kind: TokenKind) -> Return<Token<'p>> {
        let token = self.peek().ok_or(SyntaxError::UnexpectedEof)?;
        if token.kind == kind {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // Skip comments and try again
            self.expect(kind)
        } else {
            let err = SyntaxError::UnexpectedToken {
                token: token.kind,
                expected_any: vec![kind],
                location: token.location,
            };

            self.errors.push(err.clone());
            Err(err)
        }
    }

    /// Consume the next token and return it if it matches the expected kind
    pub(crate) fn expect(&mut self, kind: TokenKind) -> Return<Token<'p>> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        if token.kind == kind {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // Skip comments and try again
            self.expect(kind)
        } else {
            let err = SyntaxError::UnexpectedToken {
                token: token.kind,
                expected_any: vec![kind],
                location: token.location,
            };

            self.errors.push(err.clone());
            Err(err)
        }
    }

    pub(crate) fn expect_one_of(&mut self, kinds: Vec<TokenKind>) -> Return<Token<'p>> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        if kinds.contains(&token.kind) {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // Skip comments and try again
            self.expect_one_of(kinds)
        } else {
            let err = SyntaxError::UnexpectedToken {
                token: token.kind,
                expected_any: kinds,
                location: token.location,
            };

            self.errors.push(err.clone());
            Err(err)
        }
    }

    /// Remap the error from `expect` to a custom error passed in
    pub(crate) fn expect_error(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token<'p>> {
        self.expect(kind).map_err(|_| err)
    }

    pub fn parse(src: &'p str) -> ReturnErrors<'p, Ast> {
        let mut parser = Parser::new(crate::syntax::lexer::lex_tokens(src));
        let mut nodes: Vec<Spanned<ToplevelStmt>> = Vec::new();

        let mut comments: Vec<(SourceLoc, String)> = Vec::new();

        while let Some(token) = parser.clone().peek() {
            match token.kind {
                TokenKind::NewLine => {
                    parser.line += 1;
                    parser.col = 0;

                    parser.advance();
                }
                TokenKind::Comment => {
                    // Skip comments while still storing them in the AST
                    let location = token.location;
                    parser.advance();
                    comments.push((location, token.literal.to_string()));
                }
                TokenKind::KwFunction
                | TokenKind::KwStruct
                | TokenKind::KwEnum
                | TokenKind::KwExtern => {
                    // Functions are top-level nodes
                    let location = token.location.clone();
                    let stmt = match parser.parse_toplevel_stmt() {
                        Ok(stmt) => stmt,
                        Err(err) => {
                            parser.errors.push(err);
                            parser.advance();
                            continue;
                        }
                    };

                    nodes.push(spanned(stmt, location));
                }
                _ => {
                    let location = token.location;
                    let stmt = match parser.parse_stmt() {
                        Ok(stmt) => stmt,
                        Err(err) => {
                            parser.errors.push(err);
                            parser.advance();
                            continue;
                        }
                    };

                    nodes.push(spanned(ToplevelStmt::Stmt(stmt), location));
                }
            }
        }

        if parser.errors.is_empty() {
            Ok(Ast { nodes, comments })
        } else {
            Err(parser.errors.clone())
        }
    }
}
