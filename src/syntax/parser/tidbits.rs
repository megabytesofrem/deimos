//! A few teeny-tiny parsers used frequently throughout the parser.
//! They don't fit anywhere else so they are in tidbits.rs

use crate::syntax::ast_types::{NumericSize, Ty};
use crate::syntax::lexer::{BinOp, TokenKind, UnOp};
use crate::syntax::parser;
use crate::syntax::parser::syntax_error::SyntaxError;

use super::Parser;
type Param = (String, Ty);

impl TokenKind {
    pub fn to_binop(&self) -> BinOp {
        match self {
            TokenKind::Plus => BinOp::Add,
            TokenKind::Minus => BinOp::Sub,
            TokenKind::Star => BinOp::Mul,
            TokenKind::Slash => BinOp::Div,
            TokenKind::DoubleEq => BinOp::Eq,
            TokenKind::BangEq => BinOp::BangEq,
            TokenKind::Less => BinOp::Less,
            TokenKind::LessEq => BinOp::LessEq,
            TokenKind::Greater => BinOp::Greater,
            TokenKind::GreaterEq => BinOp::GreaterEq,
            _ => panic!("Invalid binary operator: {:?}", self),
        }
    }

    pub fn to_unop(&self) -> UnOp {
        match self {
            TokenKind::Minus => UnOp::Neg,
            TokenKind::Bang => UnOp::Bang,
            _ => panic!("Invalid unary operator: {:?}", self),
        }
    }

    pub(crate) fn get_precedence(&self) -> u8 {
        match self {
            TokenKind::Plus | TokenKind::Minus => 10,
            TokenKind::Star | TokenKind::Slash => 20,
            TokenKind::DoubleEq | TokenKind::BangEq => 5,
            TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::Greater
            | TokenKind::GreaterEq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq => 5,
            TokenKind::Bang | TokenKind::KwAnd | TokenKind::KwOr => 2,

            // Either not a binary operator or not implemented yet
            _ => 0,
        }
    }

    pub(crate) fn is_binop(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::DoubleEq
                | TokenKind::BangEq
                | TokenKind::Less
                | TokenKind::LessEq
                | TokenKind::Greater
                | TokenKind::GreaterEq
        )
    }

    pub(crate) fn is_unop(&self) -> bool {
        matches!(self, TokenKind::Minus | TokenKind::Bang)
    }
}

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
                    "i32" => Ok(Ty::Number(NumericSize::I32)),
                    "i64" => Ok(Ty::Number(NumericSize::I64)),
                    "f32" => Ok(Ty::Number(NumericSize::F32)),
                    "f64" => Ok(Ty::Number(NumericSize::F64)),
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
