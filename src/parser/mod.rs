//! The main parser module for Deimos.

use crate::syntax::ast::Ast;
use crate::syntax::ast::ToplevelStmt;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::SourceLoc;
use crate::syntax::lexer::{LexerIter, Token, TokenKind};
use crate::utils::spanned;
use crate::utils::Spanned;

mod expr;
mod stmt;
mod tidbits;

#[derive(Clone)]
pub struct Parser<'cx> {
    tokens: LexerIter<'cx>,
    errors: Vec<SyntaxError>,

    line: usize,
    col: usize,
}

/// Result type for parsing
pub(crate) type Return<'cx, T> = anyhow::Result<T, SyntaxError>;
pub(crate) type ReturnBundle<'cx, T> = anyhow::Result<T, Vec<SyntaxError>>;

impl<'cx> Parser<'cx> {
    pub fn new(tokens: LexerIter<'cx>) -> Self {
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
    pub(crate) fn peek(&mut self) -> Option<Token<'cx>> {
        self.tokens.peek().cloned()
    }

    /// Advance the parser by one token
    pub(crate) fn advance(&mut self) -> Option<Token<'cx>> {
        self.col += 1;
        let token = self.tokens.next();
        token
    }

    /// Check if the next token is of the expected kind without advancing
    pub(crate) fn check(&mut self, kind: TokenKind) -> Return<Token<'cx>> {
        let token = self.peek().ok_or(SyntaxError::UnexpectedEof)?;
        if token.kind == kind {
            Ok(token)
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
    pub(crate) fn expect(&mut self, kind: TokenKind) -> Return<Token<'cx>> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        if token.kind == kind {
            Ok(token)
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

    pub(crate) fn expect_one_of(&mut self, kinds: Vec<TokenKind>) -> Return<Token<'cx>> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        if kinds.contains(&token.kind) {
            Ok(token)
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
    pub(crate) fn expect_error(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token<'cx>> {
        self.expect(kind).map_err(|_| err)
    }

    pub fn parse(src: &'cx str) -> ReturnBundle<'cx, Ast> {
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
