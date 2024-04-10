use crate::syntax::ast::{Expr, Literal};
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{BinOp, Token, TokenKind, UnOp};
use crate::syntax::parse;
use crate::syntax::parse::Parser;
use crate::syntax::span::{Spanned, spanned};

fn strip_quotes(s: &str) -> &str {
    &s[1..s.len() - 1]
}

fn tk_to_binop(kind: &TokenKind) -> BinOp {
    match kind {
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
        _ => panic!("Not a binary operator: {:?}", kind),
    }
}

fn tk_to_unop(kind: &TokenKind) -> UnOp {
    match kind {
        TokenKind::Minus => UnOp::Neg,
        TokenKind::Bang => UnOp::Bang,
        _ => panic!("Not a unary operator: {:?}", kind),
    }
}

impl<'cx> Parser<'cx> {
    fn parse_expr_prec(&mut self, min_prec: u8) -> parse::Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        let mut lhs = self.parse_primary_expr()?;

        while let Some(op) = self.peek() {
            let op = op.clone().kind;

            // If the operator has a lower precedence than the minimum precedence, stop parsing
            if op.get_precedence() < min_prec {
                break;
            }

            // Consume the operator
            if op.is_binop() {
                self.next();
            } else {
                break;
            }

            // Parse the RHS of the expression
            if op.is_binop() {
                let rhs = self.parse_expr_prec(op.get_precedence() + 1)?;
                lhs = spanned(
                    Expr::BinOp(Box::new(lhs), tk_to_binop(&op), Box::new(rhs)),
                    location.clone(),
                )
            } else if op.is_unop() {
                let rhs = self.parse_expr_prec(op.get_precedence())?;
                lhs = spanned(
                    Expr::UnOp(tk_to_unop(&op), Box::new(rhs)),
                    location.clone(),
                );
            } else {
                // Not an operator so stop parsing
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_literal(&mut self, token: &Token) -> parse::Return<Literal> {
        match token.kind {
            TokenKind::Integer | TokenKind::HexInteger => {
                Ok(Literal::Int(token.to_int_literal()))
            }
            TokenKind::Float => Ok(Literal::Float(
                token.literal.parse().expect("Invalid float literal")
            )),
            TokenKind::StringLit => Ok(Literal::String(strip_quotes(token.literal).to_string())),
            TokenKind::KwTrue => Ok(Literal::Bool(true)),
            TokenKind::KwFalse => Ok(Literal::Bool(false)),
            _ => panic!("Not a literal: {:?}", token.kind)
        }
    }

    fn parse_primary_expr(&mut self) -> parse::Return<Spanned<Expr>> {
        // Hardcoded list of expected tokens for error messages
        let expected_tokens = vec![
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::StringLit,
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::LSquare,
            TokenKind::LCurly,
        ];

        let location = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        let token = self.next().ok_or(SyntaxError::ExpectedExpr {
            location: location.clone()
        })?;

        match &token.kind {
            TokenKind::Minus | TokenKind::Bang => {
                let expr = self.parse_expr()?;
                Ok(spanned(Expr::UnOp(tk_to_unop(&token.kind), Box::new(expr)), location))
            }
            // Sub-expressions
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                self.expect_error(
                    TokenKind::RParen,
                    SyntaxError::UnbalancedBrackets { location },
                )?;
                Ok(expr)
            }
            // Array literals
            TokenKind::LSquare => self.parse_array_literal(),

            // Literals
            TokenKind::Integer
            | TokenKind::HexInteger
            | TokenKind::Float
            | TokenKind::StringLit
            | TokenKind::KwTrue
            | TokenKind::KwFalse => {
                let literal = self.parse_literal(&token)?;
                Ok(spanned(Expr::Literal(literal), location))
            }
            TokenKind::Ident => self.parse_starting_ident(&token.literal),

            // Unexpected token, expected one of the following
            err_token => Err(SyntaxError::ExpectedOneOf {
                expected: expected_tokens,
                found: err_token.clone(),
                location: location.clone(),
            })
        }
    }

    fn parse_array_literal(&mut self) -> parse::Return<Spanned<Expr>> {
        // [value1, value2, ...]
        let mut location = self.peek().map(|t| t.location.clone()).unwrap_or_default();

        self.expect(TokenKind::LSquare)?;
        let mut elements = vec![];

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RSquare {
                break;
            }

            let expr = self.parse_expr()?;
            elements.push(expr);

            // Check for right square bracket, meaning the end of the array
            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RSquare {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
        }

        location = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnbalancedBrackets {
                location: location.clone(),
            },
        )?;

        Ok(spanned(Expr::Array(elements), location))
    }

    pub(crate) fn parse_array_indexer(&mut self, array_like: &Spanned<Expr>) -> parse::Return<Spanned<Expr>> {
        let mut location = self.peek().map(|t| t.location.clone()).unwrap_or_default();

        // Check that array-like is an array-like type
        match array_like.target {
            Expr::Array(_) | Expr::Variable(_) => (),
            _ => return Err(SyntaxError::ExpectedArray { location })
        }

        self.expect(TokenKind::LSquare)?;
        let index = self.parse_expr()?;
        location = self.peek().map(|t| t.location.clone()).unwrap_or_default();

        // Check for unmatched brackets
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnbalancedBrackets {
                location: location.clone(),
            },
        )?;

        Ok(spanned(
            Expr::ArrayIndex { array: Box::new(array_like.clone()), index: Box::new(index) },
            location,
        ))
    }

    pub(crate) fn parse_starting_ident(&mut self, name: &str) -> parse::Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.location.clone()).unwrap_or_default();

        match self.peek().map(|t| t.kind.clone()) {
            Some(TokenKind::LParen) => {
                // Function call
                unimplemented!()
            }
            Some(TokenKind::LSquare) => {
                // Array indexer
                let expr = self.parse_expr()?;
                self.parse_array_indexer(&expr)
            }
            Some(TokenKind::Dot) => {
                // Field access
                unimplemented!()
            }
            Some(_) => {
                // Just an identifier
                self.expect(TokenKind::Ident)?;
                Ok(spanned(Expr::Variable(name.to_string()), location))
            }
            None => {
                Err(SyntaxError::UnexpectedEof)
            }
        }
    }

    pub fn parse_expr(&mut self) -> parse::Return<Spanned<Expr>> {
        self.parse_expr_prec(0)
    }
}