//! Parsing of expressions
//! The expression parser is split into another file to keep the codebase clean and organized.

use super::Parser;
use crate::spanned::{spanned, Spanned};
use crate::syntax::ast::{Expr, Literal, Member};
use crate::syntax::lexer::{Op, Token, TokenKind};
use crate::syntax::parser::{self, syntax_error::SyntaxError};

fn strip_quotes(s: &str) -> &str {
    &s[1..s.len() - 1]
}

impl<'p> Parser<'p> {
    // TODO: Does this handle left/right associativity at all?
    fn parse_expr_prec(&mut self, min_prec: u8) -> parser::Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let mut lhs = self.parse_main_expr()?;

        while let Some(op) = self.peek() {
            let op = op.clone().kind;

            // If the operator has a lower precedence than the minimum precedence, break
            if op.get_precedence() < min_prec {
                break;
            }

            if op.is_binop() {
                self.advance();
            } else {
                break;
            }

            // Parse the right hand side of the expression
            if op.is_binop() {
                let rhs = self.parse_expr_prec(op.get_precedence() + 1)?;
                lhs = spanned(
                    Expr::BinOp(Box::new(lhs), op.to_op(), Box::new(rhs)),
                    location.clone(),
                );
            } else if op.is_unop() {
                let rhs = self.parse_expr_prec(op.get_precedence())?;
                lhs = spanned(Expr::UnOp(op.to_op(), Box::new(rhs)), location.clone());
            } else {
                // Not a binary or unary operator, break
                break;
            }
        }

        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> parser::Return<Spanned<Expr>> {
        self.parse_expr_prec(0)
    }

    fn parse_main_expr(&mut self) -> parser::Return<Spanned<Expr>> {
        let expected_tokens = [
            TokenKind::Integer,
            TokenKind::Float,
            TokenKind::StringLit,
            TokenKind::KwTrue,
            TokenKind::KwFalse,
            TokenKind::LParen,
            TokenKind::LSquare,
            TokenKind::LCurly,
            TokenKind::Name,
        ];

        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let token = self.peek().ok_or(SyntaxError::ExpectedExpr {
            location: location.clone(),
        })?;

        match &token.kind {
            TokenKind::Minus | TokenKind::Bang => {
                let expr = self.parse_expr()?;
                Ok(spanned(
                    Expr::UnOp(token.kind.to_op(), Box::new(expr)),
                    location,
                ))
            }
            TokenKind::LParen => {
                // Subexpressions
                let expr = self.parse_expr()?;
                self.expect_error(
                    TokenKind::RParen,
                    SyntaxError::UnmatchedBrackets { location },
                )?;
                Ok(expr)
            }

            TokenKind::KwCast => self.parse_cast_expr(),
            TokenKind::LSquare => self.parse_array_literal(),
            TokenKind::LCurly => self.parse_struct_constructor(),
            TokenKind::Ampersand => {
                // Reference (&expr)
                let expr = self.parse_expr()?;
                Ok(spanned(Expr::Reference(Box::new(expr)), location))
            }

            // Primitives
            TokenKind::Integer
            | TokenKind::Float
            | TokenKind::StringLit
            | TokenKind::KwTrue
            | TokenKind::KwFalse => {
                let literal = self.parse_literal(&token)?;
                Ok(spanned(Expr::Literal(literal), location))
            }
            TokenKind::Name => {
                let ident = spanned(Expr::Ident(token.literal.to_string()), location.clone());
                self.advance(); // consume the name
                self.parse_postfix_operators(ident)
            }

            // Unexpected token, return an error
            err => Err(SyntaxError::ExpectedOneOf {
                expected: expected_tokens.to_vec(),
                found: err.clone(),
                location,
            }),
        }
    }

    // Parse postfix operators like function calls, member access, and array
    // access.  If there is not a postfix operator, return the base node.
    pub fn parse_postfix_operators(
        &mut self,
        base_node: Spanned<Expr>,
    ) -> parser::Return<Spanned<Expr>> {
        //let location = self.peek().map(|t| t.location).unwrap_or_default();

        if let Some(next_token) = self.peek() {
            match next_token.kind {
                // Member access: foo.bar
                TokenKind::Dot => {
                    self.advance(); // Consume the dot

                    if let Some(next_token) = self.peek() {
                        match next_token.kind {
                            TokenKind::Name => {
                                let expr = Expr::Member(Member {
                                    target: Box::new(base_node),
                                    name: next_token.literal.to_string(),
                                });
                                let spanned_expr = spanned(expr, next_token.location.clone());
                                self.advance(); // Consume the name

                                // Recurse to handle additional postfix operations
                                self.parse_postfix_operators(spanned_expr)
                            }
                            _ => Err(SyntaxError::UnexpectedToken {
                                token: next_token.kind.clone(),
                                expected_any: vec![TokenKind::Name],
                                location: next_token.location.clone(),
                            }),
                        }
                    } else {
                        Err(SyntaxError::UnexpectedEof)
                    }
                }

                // Function call: foo()
                TokenKind::LParen => {
                    let spanned_expr = self.parse_function_call(base_node)?;

                    // Recurse to handle additional postfix operations
                    self.parse_postfix_operators(spanned_expr)
                }

                // Array index: foo[0]
                TokenKind::LSquare => {
                    let spanned_expr = self.parse_array_index(&base_node.target)?;

                    // Recurse to handle additional postfix operations
                    self.parse_postfix_operators(spanned_expr)
                }

                _ => Ok(base_node), // No more postfix operators
            }
        } else {
            Ok(base_node)
        }
    }

    fn parse_literal(&mut self, token: &Token) -> parser::Return<Literal> {
        let location = self.advance().map(|t| t.location).unwrap_or_default();

        match token.kind {
            TokenKind::Integer | TokenKind::HexInteger => Ok(Literal::Int(token.to_int_literal())),
            TokenKind::Float => Ok(Literal::Float32(token.literal.parse().unwrap())),
            TokenKind::StringLit => Ok(Literal::String(strip_quotes(token.literal).to_string())),
            TokenKind::KwTrue => Ok(Literal::Bool(true)),
            TokenKind::KwFalse => Ok(Literal::Bool(false)),

            _ => panic!("Invalid literal: {:?}", token),
        }
    }

    pub(crate) fn parse_function_call(
        &mut self,
        callee: Spanned<Expr>,
    ) -> parser::Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let mut args = Vec::new();

        self.expect(TokenKind::LParen)?;

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            args.push(expr);

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

        Ok(spanned(
            Expr::Call {
                callee: Box::new(callee),
                args,
            },
            location,
        ))
    }

    fn parse_cast_expr(&mut self) -> parser::Return<Spanned<Expr>> {
        // let ident:type = cast(expr, type)
        self.expect(TokenKind::KwCast)?;

        let location = self.peek().map(|t| t.location).unwrap_or_default();
        self.expect(TokenKind::LParen)?;
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Comma)?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::RParen)?;

        Ok(spanned(Expr::Cast(Box::new(expr), ty), location))
    }

    fn parse_array_literal(&mut self) -> parser::Return<Spanned<Expr>> {
        // [item1, item2, item3]
        let location = self.peek().map(|t| t.location).unwrap_or_default();

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

        Ok(spanned(
            Expr::Array(elements.iter().map(|e| e.clone()).collect()),
            location,
        ))
    }

    fn parse_array_index(&mut self, _array: &Expr) -> parser::Return<Spanned<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let array = self.parse_expr()?;

        // Check that it is an array
        match array.target {
            Expr::Array(_) => (),
            _ => return Err(SyntaxError::ExpectedArray { location }),
        }

        self.expect(TokenKind::LSquare)?;
        let index = self.parse_expr()?;
        self.expect(TokenKind::RSquare)?;

        // Check for unmatched brackets
        self.expect_error(
            TokenKind::RSquare,
            SyntaxError::UnmatchedBrackets {
                location: location.clone(),
            },
        )?;

        Ok(spanned(
            Expr::ArrayIndex {
                array: Box::new(array),
                index: Box::new(index),
            },
            location,
        ))
    }

    fn parse_struct_field(&mut self) -> parser::Return<(String, Spanned<Expr>)> {
        let name = self.expect(TokenKind::Name)?.literal.to_string();
        self.expect(TokenKind::Colon)?;
        let expr = self.parse_expr()?;

        Ok((name, expr))
    }

    fn parse_struct_constructor(&mut self) -> parser::Return<Spanned<Expr>> {
        // {field1: value1, field2: value2}
        self.advance();

        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let mut fields = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RCurly {
                break;
            }

            let field = self.parse_struct_field()?;
            fields.push(field);

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

        Ok(spanned(
            Expr::StructCons {
                fields: fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            },
            location,
        ))
    }
}
