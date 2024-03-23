use super::{ParseResult, Parser, TokenKind};
use crate::syntax::ast::*;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{BinOp, UnOp};

fn strip_quotes(s: &str) -> &str {
    &s[1..s.len() - 1]
}

fn tokenkind_to_binop(kind: &TokenKind) -> BinOp {
    match kind {
        TokenKind::Plus => BinOp::Add,
        TokenKind::Minus => BinOp::Sub,
        TokenKind::Star => BinOp::Mul,
        TokenKind::Slash => BinOp::Div,
        TokenKind::Equal => BinOp::Equal,
        TokenKind::BangEqual => BinOp::BangEqual,
        TokenKind::Less => BinOp::Less,
        TokenKind::LessEqual => BinOp::LessEqual,
        TokenKind::Greater => BinOp::Greater,
        TokenKind::GreaterEqual => BinOp::GreaterEqual,
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
    fn parse_expr_prec(&mut self, min_prec: u8) -> ParseResult<Expr> {
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
                lhs = Expr::BinOp(Box::new(lhs), tokenkind_to_binop(&op), Box::new(rhs));
            } else if op.is_unop() {
                let rhs = self.parse_expr_prec(op.get_precedence())?;
                lhs = Expr::UnOp(tokenkind_to_unop(&op), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr_prec(0)
    }

    fn parse_starting_ident(&mut self, name: String) -> ParseResult<Expr> {
        if let Some(TokenKind::LParen) = self.peek().map(|t| &t.kind) {
            // Function call
            todo!();
        }

        if let Some(TokenKind::LSquare) = self.peek().map(|t| &t.kind) {
            // Array indexer
            let expr = self.parse_expr()?;
            return self.parse_array_indexer(expr);
        }

        // Just an identifier
        Ok(Expr::Variable(name))
    }

    pub fn parse_value(&mut self) -> ParseResult<Expr> {
        // Hardcoded list of expected tokens for error messages
        let expected_tokens = [
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::String,
            TokenKind::Ident,
            TokenKind::LSquare, // for array literals
            TokenKind::LCurly,  // for struct initializers
        ];

        // Get the span location of the token
        let loc = self.peek().map(|t| t.clone().location).unwrap_or_default();
        let token = self.advance().ok_or(SyntaxError::ExpectedExpr {
            location: loc.clone(),
        })?;

        match &token.kind {
            TokenKind::Minus | TokenKind::Bang => {
                let expr = self.parse_expr()?;
                Ok(Expr::UnOp(tokenkind_to_unop(&token.kind), Box::new(expr)))
            }

            // Sub-expressions
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                self.expect_error(
                    TokenKind::RParen,
                    SyntaxError::UnmatchedBrackets { location: loc },
                )?;
                Ok(expr)
            }

            TokenKind::LSquare => self.parse_array_literal(),
            TokenKind::LCurly => self.parse_struct_cons(),

            // Primitives
            TokenKind::Integer => Ok(Expr::Int(token.to_int_literal())),
            TokenKind::Float => Ok(Expr::Float(token.literal.parse().unwrap())),
            TokenKind::String => Ok(Expr::String(strip_quotes(token.literal).to_string())),
            TokenKind::KwTrue => Ok(Expr::Bool(true)),
            TokenKind::KwFalse => Ok(Expr::Bool(false)),
            TokenKind::Ident => self.parse_starting_ident(token.literal.to_string()),
            // Unexpected token, expected one of the above
            err_token => Err(SyntaxError::ExpectedOneOf {
                expected: expected_tokens.to_vec(),
                found: err_token.clone(),
                location: loc,
            }),
        }
    }

    fn parse_array_indexer(&mut self, array: Expr) -> ParseResult<Expr> {
        let mut loc = self.peek().map(|t| t.clone().location).unwrap_or_default();
        let arr = self.parse_expr()?;

        // Check that arr is an array
        match arr {
            Expr::Array(_) => (),
            _ => return Err(SyntaxError::ExpectedArray { location: loc }),
        }

        self.expect(TokenKind::LSquare)?;
        let index = self.parse_expr()?;
        loc = self.peek().map(|t| t.location.clone()).unwrap_or_default();

        // Check for unmatched brackets
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnmatchedBrackets { location: loc },
        )?;

        Ok(Expr::ArrayIndex {
            array: Box::new(array),
            index: Box::new(index),
        })
    }

    fn parse_array_literal(&mut self) -> ParseResult<Expr> {
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

        let loc = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnmatchedBrackets { location: loc },
        )?;

        Ok(Expr::Array(elements))
    }

    fn parse_struct_cons(&mut self) -> ParseResult<Expr> {
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

        Ok(Expr::StructCons { fields })
    }
}
