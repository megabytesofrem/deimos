use super::{ParseResult, Parser, SourceLoc};
use crate::syntax::ast::*;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{BinOp, TokenKind, UnOp};

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
    fn spanned(&mut self, expr: Expr, location: SourceLoc) -> SpannedExpr {
        SpannedExpr {
            raw: expr,
            location,
        }
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> ParseResult<SpannedExpr> {
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
                lhs = self.spanned(
                    Expr::BinOp(
                        Box::new(lhs.raw),
                        tokenkind_to_binop(&op),
                        Box::new(rhs.raw),
                    ),
                    location.clone(),
                );
            } else if op.is_unop() {
                let rhs = self.parse_expr_prec(op.get_precedence())?;
                lhs = self.spanned(
                    Expr::UnOp(tokenkind_to_unop(&op), Box::new(rhs.raw)),
                    location.clone(),
                );
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> ParseResult<SpannedExpr> {
        self.parse_expr_prec(0)
    }

    fn parse_starting_ident(&mut self, name: String) -> ParseResult<SpannedExpr> {
        let location = self.peek().map(|t| t.clone().location).unwrap_or_default();

        if let Some(TokenKind::LParen) = self.peek().map(|t| &t.kind) {
            // Function call
            todo!();
        }

        if let Some(TokenKind::LSquare) = self.peek().map(|t| &t.kind) {
            // Array indexer
            let expr = self.parse_expr()?;
            return self.parse_array_indexer(expr.raw);
        }

        // Just an identifier
        Ok(self.spanned(Expr::Variable(name), location))
    }

    pub fn parse_value(&mut self) -> ParseResult<SpannedExpr> {
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
                Ok(self.spanned(
                    Expr::UnOp(tokenkind_to_unop(&token.kind), Box::new(expr.raw)),
                    location,
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
            TokenKind::Integer => Ok(self.spanned(Expr::Int(token.to_int_literal()), location)),
            TokenKind::Float => {
                Ok(self.spanned(Expr::Float(token.literal.parse().unwrap()), location))
            }
            TokenKind::StringLit => Ok(self.spanned(
                Expr::String(strip_quotes(token.literal).to_string()),
                location,
            )),
            TokenKind::KwTrue => Ok(self.spanned(Expr::Bool(true), location)),
            TokenKind::KwFalse => Ok(self.spanned(Expr::Bool(false), location)),
            TokenKind::Ident => self.parse_starting_ident(token.literal.to_string()),
            // Unexpected token, expected one of the above
            err_token => Err(SyntaxError::ExpectedOneOf {
                expected: expected_tokens.to_vec(),
                found: err_token.clone(),
                location,
            }),
        }
    }

    fn parse_array_indexer(&mut self, array: Expr) -> ParseResult<SpannedExpr> {
        let mut location = self.peek().map(|t| t.clone().location).unwrap_or_default();
        let arr = self.parse_expr()?;

        // Check that arr is an array
        match arr.raw {
            Expr::Array(_) => (),
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

        Ok(self.spanned(
            Expr::ArrayIndex {
                array: Box::new(array),
                index: Box::new(index.raw),
            },
            location,
        ))
    }

    fn parse_array_literal(&mut self) -> ParseResult<SpannedExpr> {
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

        let loc = self.peek().map(|t| t.location.clone()).unwrap_or_default();
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnmatchedBrackets { location: loc },
        )?;

        Ok(self.spanned(
            Expr::Array(elements.iter().map(|e| e.raw.clone()).collect()),
            location,
        ))
    }

    fn parse_tuple_literal(&mut self) -> ParseResult<SpannedExpr> {
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

        Ok(self.spanned(
            Expr::Tuple(elements.iter().map(|e| e.raw.clone()).collect()),
            location,
        ))
    }

    fn parse_struct_cons(&mut self) -> ParseResult<SpannedExpr> {
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

        Ok(self.spanned(
            Expr::StructCons {
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.raw.clone()))
                    .collect(),
            },
            location,
        ))
    }
}
