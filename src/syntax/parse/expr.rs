use super::{Parser, Return, Token};

use crate::spanned;
use crate::syntax::ast::*;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{BinOp, TokenKind, UnOp};
use crate::syntax::span::Spanned;

fn strip_quotes(s: &str) -> &str {
    &s[1..s.len() - 1]
}

fn tokenkind_to_binop(kind: &TokenKind) -> BinOp {
    match kind {
        TokenKind::Plus => BinOp::Add,
        TokenKind::Minus => BinOp::Sub,
        TokenKind::Star => BinOp::Mul,
        TokenKind::Slash => BinOp::Div,
        TokenKind::Equal => BinOp::Eq,
        TokenKind::BangEqual => BinOp::BangEq,
        TokenKind::Less => BinOp::Less,
        TokenKind::LessEqual => BinOp::LessEq,
        TokenKind::Greater => BinOp::Greater,
        TokenKind::GreaterEqual => BinOp::GreaterEq,
        _ => panic!("Not a binary operator: {:?}", kind),
    }
}

fn tokenkind_to_unop(kind: &TokenKind) -> UnOp {
    match kind {
        TokenKind::Minus => UnOp::Neg,
        TokenKind::Bang => UnOp::Bang,
        _ => panic!("Not a unary operator: {:?}", kind),
    }
}

impl<'a> Parser<'a> {
    fn parse_expr_prec(&mut self, min_prec: u8) -> Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();
        let mut lhs = self.parse_value()?;

        while let Some(op) = self.peek() {
            let op = op.clone().kind;

            // If the operator has a lower precedence than the minimum precedence, we're done
            if op.get_precedence() < min_prec {
                break;
            }

            // Consume the operator
            if op.is_op() {
                self.advance();
            } else {
                break;
            }

            // Parse the right-hand side of the expression
            if op.is_op() {
                let rhs = self.parse_expr_prec(op.get_precedence() + 1)?;
                lhs = spanned!(
                    Expr::BinOp(Box::new(lhs), tokenkind_to_binop(&op), Box::new(rhs)),
                    location.clone()
                );
            } else if op.is_unop() {
                let rhs = self.parse_expr_prec(op.get_precedence())?;
                lhs = spanned!(
                    Expr::UnOp(tokenkind_to_unop(&op), Box::new(rhs)),
                    location.clone()
                );
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> Return<Spanned<Expr>> {
        self.parse_expr_prec(0)
    }

    fn parse_starting_ident(&mut self, name: String) -> Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();

        if let Some(TokenKind::LParen) = self.peek().map(|t| &t.kind) {
            // Function call
            todo!();
        }

        if let Some(TokenKind::LSquare) = self.peek().map(|t| &t.kind) {
            // Array indexer
            let expr = self.parse_expr()?;
            return self.parse_array_indexer(expr.target);
        }

        // Just an identifier
        Ok(spanned!(Expr::Variable(name), location))
    }

    fn parse_literal(&mut self, token: &Token) -> Return<Literal> {
        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();

        match token.kind {
            TokenKind::Integer => Ok(Literal::Int(token.to_int_literal())),
            TokenKind::Float => Ok(Literal::Float(
                token.literal.parse().expect("Invalid float literal"),
            )),
            TokenKind::StringLit => Ok(Literal::String(strip_quotes(token.literal).to_string())),
            TokenKind::KwTrue => Ok(Literal::Bool(true)),
            TokenKind::KwFalse => Ok(Literal::Bool(false)),
            _ => panic!("parse_literal"),
        }
    }

    pub fn parse_value(&mut self) -> Return<Spanned<Expr>> {
        // Hardcoded list of expected tokens for error messages
        let expected_tokens = [
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::StringLit,
            TokenKind::Ident,
            TokenKind::LSquare, // for array literals
            TokenKind::LCurly,  // for struct initializers
        ];

        // Get the span location of the token
        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();
        let token = self.advance().ok_or(SyntaxError::ExpectedExpr {
            location: location.clone(),
        })?;

        match &token.kind {
            TokenKind::Minus | TokenKind::Bang => {
                let expr = self.parse_expr()?;
                Ok(spanned!(
                    Expr::UnOp(tokenkind_to_unop(&token.kind), Box::new(expr)),
                    location
                ))
            }

            // Sub-expressions
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                self.expect_error(
                    TokenKind::RParen,
                    SyntaxError::UnmatchedBrackets { location },
                )?;
                Ok(expr)
            }

            TokenKind::LSquare => self.parse_array_literal(),
            TokenKind::LCurly => self.parse_struct_cons(),

            // Primitives
            TokenKind::Integer
            | TokenKind::Float
            | TokenKind::StringLit
            | TokenKind::KwTrue
            | TokenKind::KwFalse => {
                let lit = self.parse_literal(&token)?;
                Ok(spanned!(Expr::Literal(lit), location))
            }

            // Unexpected token, expected one of the above
            err_token => Err(SyntaxError::ExpectedOneOf {
                expected: expected_tokens.to_vec(),
                found: err_token.clone(),
                location,
            }),
        }
    }

    fn parse_array_indexer(&mut self, array: Expr) -> Return<Spanned<Expr>> {
        let mut location = self.peek().map(|t| t.clone().location).unwrap_or_default();
        let arr = self.parse_expr()?;

        // Check that arr is an array
        match arr.target {
            Expr::Array { .. } => (),
            _ => return Err(SyntaxError::ExpectedArray { location }),
        }

        self.expect(TokenKind::LSquare)?;
        let index = self.parse_expr()?;
        location = self.peek().map(|t| t.location.clone()).unwrap_or_default();

        // Check for unmatched brackets
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnmatchedBrackets {
                location: location.clone(),
            },
        )?;

        Ok(spanned!(
            Expr::ArrayIndex {
                array: Box::new(arr),
                index: Box::new(index),
            },
            location
        ))
    }

    fn parse_array_literal(&mut self) -> Return<Spanned<Expr>> {
        // Array syntax: [value1, value2, ...]
        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();

        self.expect(TokenKind::LSquare)?;
        let mut elements = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RSquare {
                break;
            }

            let expr = self.parse_expr()?;
            elements.push(expr);

            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RSquare {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
        }

        let location_ = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnmatchedBrackets {
                location: location_,
            },
        )?;

        Ok(spanned!(
            Expr::Array(elements.iter().map(|e| e.clone()).collect()),
            location
        ))
    }

    fn parse_tuple_literal(&mut self) -> Return<Spanned<Expr>> {
        // Tuple syntax: (value1, value2, ...)
        // Tuples are represented in the backend as anonymous structs and can be returned from functions

        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();

        self.expect(TokenKind::LParen)?;
        let mut elements = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            elements.push(expr);

            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RParen {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
        }

        let loc = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        self.expect_error(
            TokenKind::RParen,
            SyntaxError::UnmatchedBrackets { location: loc },
        )?;

        Ok(spanned!(
            Expr::Tuple(elements.iter().map(|e| e.clone()).collect()),
            location
        ))
    }

    fn parse_struct_cons(&mut self) -> Return<Spanned<Expr>> {
        // Struct constructor syntax: { field1: value1, field2: value2, ... }

        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();
        self.expect(TokenKind::LCurly)?;
        let mut fields = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RCurly {
                break;
            }

            let ident = self.expect(TokenKind::Ident)?;
            self.expect(TokenKind::Colon)?;
            let value = self.parse_expr()?;
            fields.push((ident.literal.to_string(), value));

            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RCurly {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
        }

        let loc = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        self.expect_error(
            TokenKind::RCurly,
            SyntaxError::UnmatchedBrackets { location: loc },
        )?;

        Ok(spanned!(
            Expr::StructCons {
                fields: fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            },
            location
        ))
    }
}
