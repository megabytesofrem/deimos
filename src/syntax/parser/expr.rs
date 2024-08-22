//! Parsing of expressions
//! The expression parser is split into another file to keep the codebase clean and organized.

use super::Parser;
use crate::syntax::ast::{Expr, Literal};
use crate::syntax::lexer::{BinOp, SourceLoc, Token, TokenKind, UnOp};
use crate::syntax::parser::{self, syntax_error::SyntaxError};
use crate::utils::{spanned, Spanned};

fn strip_quotes(s: &str) -> &str {
    &s[1..s.len() - 1]
}

impl TokenKind {
    fn to_binop(&self) -> BinOp {
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

    fn to_unop(&self) -> UnOp {
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

impl<'p> Parser<'p> {
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
                    Expr::BinOp(Box::new(lhs), op.to_binop(), Box::new(rhs)),
                    location.clone(),
                );
            } else if op.is_unop() {
                let rhs = self.parse_expr_prec(op.get_precedence())?;
                lhs = spanned(Expr::UnOp(op.to_unop(), Box::new(rhs)), location.clone());
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
                    Expr::UnOp(token.kind.to_unop(), Box::new(expr)),
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
                let location = self.peek().map(|t| t.location).unwrap_or_default();
                let qualified_name = self.parse_qualified_name()?.0;
                match self.peek().map(|t| t.kind) {
                    Some(TokenKind::LParen) => {
                        return self.parse_function_call(qualified_name.to_string());
                    }
                    Some(TokenKind::LSquare) => {
                        let expr = self.parse_expr()?;
                        return self.parse_array_index(&expr.target);
                    }
                    _ => Ok(spanned(
                        Expr::QualifiedName(qualified_name.to_string()),
                        location,
                    )),
                }
            }

            // Unexpected token, return an error
            err => Err(SyntaxError::ExpectedOneOf {
                expected: expected_tokens.to_vec(),
                found: err.clone(),
                location,
            }),
        }
    }

    pub fn parse_qualified_name(&mut self) -> parser::Return<(String, SourceLoc)> {
        let mut qualified_name = Vec::new();
        let mut location = self.peek().map(|t| t.location).unwrap_or_default();

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Name => {
                    qualified_name.push(token.literal.to_string());
                    location = token.location.clone(); // Update location to the last part of the name
                    self.advance(); // Advance after consuming a name
                }
                TokenKind::Dot => {
                    self.advance();

                    // Check that the next token is a name
                    if let Some(next_token) = self.peek() {
                        if next_token.kind != TokenKind::Name {
                            return Err(SyntaxError::UnexpectedToken {
                                token: next_token.kind.clone(),
                                expected_any: vec![TokenKind::Name],
                                location: next_token.location.clone(),
                            });
                        }
                    } else {
                        return Err(SyntaxError::UnexpectedEof);
                    }
                }
                _ => break,
            }
        }

        // if qualified_name.is_empty() {
        //     return Err(SyntaxError::ExpectedQualifiedName { location });
        // }

        Ok((qualified_name.join("."), location))
    }

    fn parse_literal(&mut self, token: &Token) -> parser::Return<Literal> {
        let location = self.advance().map(|t| t.location).unwrap_or_default();

        match token.kind {
            TokenKind::Integer => Ok(Literal::Int(token.to_int_literal())),
            TokenKind::Float => Ok(Literal::Float32(token.literal.parse().unwrap())),
            TokenKind::StringLit => Ok(Literal::String(strip_quotes(token.literal).to_string())),
            TokenKind::KwTrue => Ok(Literal::Bool(true)),
            TokenKind::KwFalse => Ok(Literal::Bool(false)),

            _ => panic!("Invalid literal: {:?}", token),
        }
    }

    pub(crate) fn parse_function_call(&mut self, name: String) -> parser::Return<Spanned<Expr>> {
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
                callee: Box::new(spanned(
                    Expr::QualifiedName(name.to_string()),
                    location.clone(),
                )),
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

    fn parse_array_index(&mut self, array: &Expr) -> parser::Return<Spanned<Expr>> {
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

#[cfg(test)]
mod expr_parse_tests {
    use crate::syntax::parser::Parser;
    use crate::*;

    #[test]
    fn parse_qualified_names() {
        let qual_names = vec![
            "foo".to_string(),
            "foo::bar".to_string(),
            "foo::bar::baz".to_string(),
        ];

        for name in qual_names {
            let mut parser = Parser::new(syntax::lexer::lex_tokens(&name));
            //let result = parser.parse_qualified_name().unwrap();
            //assert_eq!(result.join("::"), name);

            //println!("Parsed qualified name: {}", result.join("::"));
        }
    }
}
