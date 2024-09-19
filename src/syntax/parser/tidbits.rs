//! A few teeny-tiny parsers used frequently throughout the parser.
//! They don't fit anywhere else so they are in tidbits.rs

use crate::syntax::ast_types::{SizedNumber, Ty};
use crate::syntax::lexer::{Op, TokenKind};
use crate::syntax::parser::{self, syntax_error::SyntaxError};

use super::Parser;
type Param = (String, Ty);

impl TokenKind {
    pub fn to_op(&self) -> Op {
        match self {
            TokenKind::Plus => Op::Add,
            TokenKind::Minus => Op::Sub,
            TokenKind::Star => Op::Mul,
            TokenKind::Slash => Op::Div,
            TokenKind::DoubleEq => Op::Eq,
            TokenKind::BangEq => Op::BangEq,
            TokenKind::Less => Op::Less,
            TokenKind::LessEq => Op::LessEq,
            TokenKind::Greater => Op::Greater,
            TokenKind::GreaterEq => Op::GreaterEq,
	    TokenKind::Bang => Op::Bang,
            _ => panic!("Invalid operator kind: {:?}", self),
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
                    "i32" => Ok(Ty::Number(SizedNumber::I32)),
                    "i64" => Ok(Ty::Number(SizedNumber::I64)),
                    "f32" => Ok(Ty::Number(SizedNumber::F32)),
                    "f64" => Ok(Ty::Number(SizedNumber::F64)),
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
