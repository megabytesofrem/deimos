use crate::syntax::ast::ToplevelStmt;
use crate::syntax::errors::SyntaxError;

use super::{ParseResult, Parser, SourceLoc, TokenKind};

impl<'a> Parser<'a> {
    fn parse_import_path(&mut self) -> ParseResult<Vec<String>> {
        // Parse a path like "a.b.c" into a vector of strings ["a", "b", "c"]
        let mut path = vec![];

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Ident => {
                    path.push(token.literal.to_string());
                    self.advance();
                }
                TokenKind::Dot => {
                    self.advance();
                }
                _ => {
                    return Err(SyntaxError::InvalidPathChar {
                        location: token.location.clone(),
                    });
                }
            }
        }

        Ok(path)
    }

    pub fn parse_import(&mut self) -> ParseResult<ToplevelStmt> {
        // import foo.bar [as baz]
        let peeked_token = self.expect(TokenKind::KwImport)?;

        let path = self.parse_import_path()?;

        if let Some(token) = self.peek() {
            if token.kind == TokenKind::KwAs {
                // import foo.bar as baz
                self.advance();
                let alias = self.expect(TokenKind::Ident)?.literal.to_string();
                return Ok(ToplevelStmt::Import {
                    path,
                    alias: Some(alias),
                });
            } else {
                // import foo.bar
                return Ok(ToplevelStmt::Import { path, alias: None });
            }
        }

        // Expected an identifier after import
        Err(SyntaxError::ExpectedIdent {
            location: peeked_token.location.clone(),
        })
    }

    pub fn parse_cimport(&mut self) -> ParseResult<ToplevelStmt> {
        // cimport "foo_bar"
        self.expect(TokenKind::KwCImport)?;
        let header_path = self.expect(TokenKind::String)?.literal.to_string();

        Ok(ToplevelStmt::CImport(header_path))
    }
}
