//! A few teeny-tiny parsers used frequently throughout the parser.
//! They don't fit anywhere else so they are in tidbits.rs

use crate::parser;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::TokenKind;
use crate::syntax::types::{Numeric, Ty};

use super::Parser;
type Param = (String, Ty);

#[allow(dead_code)]
impl<'p> Parser<'p> {
    pub(crate) fn parse_type(&mut self) -> parser::Return<Ty> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        match token.kind {
            // TODO: Later on, ?Type will be used to represent values that can fail
            // a.k.a optionals implemented at a compiler level.
            //
            // It will be the preferred way to represent exceptions in the language.
            TokenKind::Name => {
                let ident_or_type = token.literal.to_string();
                match ident_or_type.as_str() {
                    "i32" => Ok(Ty::Number(Numeric::I32)),
                    "i64" => Ok(Ty::Number(Numeric::I64)),
                    "f32" => Ok(Ty::Number(Numeric::F32)),
                    "f64" => Ok(Ty::Number(Numeric::F64)),
                    "char" => Ok(Ty::Char),
                    "bool" => Ok(Ty::Bool),
                    "string" => Ok(Ty::String),
                    "void" => Ok(Ty::Void),
                    _ => Ok(Ty::UserDefined(ident_or_type)),
                }
            }
            TokenKind::Question => {
                // Optional type
                let inner = Box::new(self.parse_type()?);
                Ok(Ty::Optional(inner))
            }
            TokenKind::Star => {
                let inner = Box::new(self.parse_type()?);
                Ok(Ty::Pointer(inner))
            }
            TokenKind::LSquare => {
                self.expect_error(
                    TokenKind::RSquare,
                    SyntaxError::UnmatchedBrackets {
                        location: token.location.clone(),
                    },
                )?;
                let inner = Box::new(self.parse_type()?);
                Ok(Ty::Array(inner))
            }

            // Invalid type
            _ => Err(SyntaxError::UnexpectedToken {
                token: token.kind.clone(),
                expected_any: vec![TokenKind::Name],
                location: token.location.clone(),
            }),
        }
    }

    pub(crate) fn parse_annotated_param(&mut self) -> parser::Return<Param> {
        let ident = self.expect(TokenKind::Name)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok((ident.literal.to_string(), ty))
    }

    pub(crate) fn parse_annotated_params(&mut self) -> parser::Return<Vec<Param>> {
        let mut params = Vec::new();

        // Parse comma separated list of annotated parameters
        loop {
            match self.peek() {
                Some(token) if token.kind == TokenKind::RParen => break,
                Some(_) => {
                    let param = self.parse_annotated_param()?;
                    params.push(param);
                    if let Some(token) = self.peek() {
                        if token.kind == TokenKind::RParen {
                            break;
                        }
                        self.expect(TokenKind::Comma)?;
                    }
                }
                None => break,
            }
        }

        Ok(params)
    }

    pub(crate) fn parse_params(&mut self) -> parser::Return<Vec<String>> {
        let mut params = Vec::new();

        // Parse comma separated list of identifiers.
        loop {
            match self.peek() {
                Some(token) if token.kind == TokenKind::RParen => break,
                Some(_) => {
                    let ident = self.expect(TokenKind::Name)?;
                    params.push(ident.literal.to_string());
                    if let Some(token) = self.peek() {
                        if token.kind == TokenKind::RParen {
                            break;
                        }
                        self.expect(TokenKind::Comma)?;
                    }
                }
                None => break,
            }
        }

        Ok(params)
    }
}
