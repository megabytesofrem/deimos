//! Parsing of statements, blocks and top-level statements.
//! The expression parser is split into another file to keep the codebase clean and organized.

use crate::parser;
use crate::syntax::ast::*;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{Token, TokenKind};
use crate::syntax::span::{spanned, Spanned};

use super::Parser;

type Param = (String, Ty);

impl<'cx> Parser<'cx> {
    pub(crate) fn parse_type(&mut self) -> parser::Return<Ty> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        match token.kind {
            // TODO: Later on, ?Type will be used to represent values that can fail
            // a.k.a optionals implemented at a compiler level.
            //
            // It will be the preferred way to represent exceptions in the language.
            TokenKind::Ident => {
                let ident_or_type = token.literal.to_string();
                match ident_or_type.as_str() {
                    "i32" => Ok(Ty::Numeric(Numeric::I32)),
                    "i64" => Ok(Ty::Numeric(Numeric::I64)),
                    "f32" => Ok(Ty::Numeric(Numeric::F32)),
                    "f64" => Ok(Ty::Numeric(Numeric::F64)),
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
                location: token.location.clone(),
            }),
        }
    }

    pub(crate) fn parse_block(&mut self) -> parser::Return<Block> {
        let mut stmts = Vec::new();

        while let Some(token) = self.peek() {
            // Skip comments inside blocks
            if token.kind == TokenKind::Comment {
                self.advance();
                continue;
            }

            if token.kind == TokenKind::KwEnd {
                break;
            }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        self.expect(TokenKind::KwEnd)?;

        Ok(stmts)
    }

    fn parse_annotated_param(&mut self) -> parser::Return<Param> {
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok((ident.literal.to_string(), ty))
    }

    fn parse_annotated_params(&mut self) -> parser::Return<Vec<Param>> {
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

    fn parse_params(&mut self) -> parser::Return<Vec<String>> {
        let mut params = Vec::new();

        // Parse comma separated list of identifiers.
        loop {
            match self.peek() {
                Some(token) if token.kind == TokenKind::RParen => break,
                Some(_) => {
                    let ident = self.expect(TokenKind::Ident)?;
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

    pub(crate) fn parse_stmt(&mut self) -> parser::Return<Spanned<Stmt>> {
        // FIXME: Why is it not erroring on completely moronic tokens?
        //println!("got: {:?}", self.peek());

        let result = match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwLet => self.parse_let_stmt(),
                TokenKind::Ident => self.parse_ident_or_assign(),
                TokenKind::KwIf => self.parse_if_stmt(),
                TokenKind::KwFor => self.parse_for_loop(),
                TokenKind::KwWhile => self.parse_while_loop(),
                TokenKind::KwReturn => self.parse_return(),
                // Unexpected token
                // Commented until I add support for comments
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    location: token.location.clone(),
                }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        };

        if let Err(err) = &result {
            self.errors.push(err.clone());
        }

        result
    }

    fn parse_let_stmt(&mut self) -> parser::Return<Spanned<Stmt>> {
        // let ident:type = expr
        let t = self.expect(TokenKind::KwLet)?;
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;

        Ok(spanned(
            Stmt::Let {
                name: ident.literal.to_string(),
                ty: Some(ty),
                value: Some(expr),
            },
            t.location,
        ))
    }

    fn parse_ident_or_assign(&mut self) -> parser::Return<Spanned<Stmt>> {
        // ident = expr or ident(expr, expr, ...)
        let ident = self.expect(TokenKind::Ident)?;
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::Equal => self.parse_assign_stmt(ident),
                TokenKind::LParen => {
                    let expr = self.parse_function_call(ident.literal.to_string())?;
                    Ok(spanned(Stmt::Expr(expr), ident.location))
                }
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    location: token.location.clone(),
                }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        }
    }

    fn parse_assign_stmt(&mut self, ident: Token) -> parser::Return<Spanned<Stmt>> {
        // ident = expr
        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;
        Ok(spanned(
            Stmt::Assign {
                target: spanned(
                    Expr::Variable(ident.literal.to_string()),
                    ident.location.clone(),
                ),
                value: expr,
            },
            ident.location,
        ))
    }

    fn parse_if_stmt(&mut self) -> parser::Return<Spanned<Stmt>> {
        // if condition then
        //  then_block
        // else
        //  else_block
        // end

        let token = self.expect(TokenKind::KwIf)?;
        let condition = self.parse_expr()?;

        self.expect(TokenKind::KwThen)?;
        let then_block = self.parse_block()?;

        let else_block = if let Some(token) = self.peek() {
            if matches!(token.kind, TokenKind::KwElse) {
                self.advance();
                Some(self.parse_block()?)
            } else {
                None
            }
        } else {
            None
        };

        Ok(spanned(
            Stmt::If {
                cond: condition,
                then_block,
                else_block,
            },
            token.location,
        ))
    }

    fn parse_for_loop(&mut self) -> parser::Return<Spanned<Stmt>> {
        // for counter = start, end do
        //  body
        // end

        let token = self.expect(TokenKind::KwFor)?;
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Equal)?;
        let start = self.parse_expr()?;
        self.expect(TokenKind::Comma)?;
        let end = self.parse_expr()?;

        self.expect(TokenKind::KwDo)?;
        let body = self.parse_block()?;

        Ok(spanned(
            Stmt::For {
                init: ident.literal.to_string(),
                from: start,
                to: end,
                body,
            },
            token.location,
        ))
    }

    fn parse_while_loop(&mut self) -> parser::Return<Spanned<Stmt>> {
        // while condition do
        //  body
        // end

        let token = self.expect(TokenKind::KwWhile)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::KwDo)?;
        let block = self.parse_block()?;
        Ok(spanned(Stmt::While { cond, block }, token.location))
    }

    fn parse_return(&mut self) -> parser::Return<Spanned<Stmt>> {
        // return expr?
        let token = self.expect(TokenKind::KwReturn)?;
        let expr = if let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                None
            } else {
                Some(self.parse_expr()?)
            }
        } else {
            None
        };

        Ok(spanned(Stmt::Return(expr), token.location))
    }

    pub(crate) fn parse_toplevel_stmt(&mut self) -> parser::Return<ToplevelStmt> {
        let result = match self.peek() {
            Some(token) => match token.kind {
                //TokenKind::KwImport => self.parse_import(),
                TokenKind::KwStruct => self.parse_struct_declare(),
                TokenKind::KwEnum => self.parse_enum_declare(),
                TokenKind::KwFunction => self.parse_function_declare(),
                TokenKind::KwExtern => self.parse_extern_declare(),

                // Unexpected token
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    location: token.location.clone(),
                }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        };

        if let Err(err) = &result {
            self.errors.push(err.clone());
        }

        result
    }

    fn parse_extern_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // External functions imported from C

        // extern cfunction_name(param:type, param:type, ...): return_type?
        let mut return_type: Ty = Ty::Void;
        self.expect(TokenKind::KwExtern)?;
        let name = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_annotated_params()?;
        self.expect(TokenKind::RParen)?;

        // Parse return type if it is present
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Colon {
                self.advance();
                return_type = self.parse_type()?;
            }
        }

        Ok(ToplevelStmt::ExternDecl {
            name: name.literal.to_string(),
            params,
            return_ty: return_type,
        })
    }

    fn parse_struct_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // struct Name
        //  field*
        // end
        let token = self.expect(TokenKind::KwStruct)?;
        let name = self.expect(TokenKind::Ident)?;

        let mut fields = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.expect(TokenKind::KwEnd)?;
                break;
            }

            let ident = self.expect(TokenKind::Ident)?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            fields.push((ident.literal.to_string(), ty));

            if let Some(token) = self.peek() {
                if token.kind != TokenKind::KwEnd {
                    self.expect(TokenKind::Comma)?;
                    break;
                }
            }
        }

        Ok(ToplevelStmt::StructDecl {
            name: name.literal.to_string(),
            fields,
        })
    }

    fn parse_enum_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // enum Name
        //   field*
        // end
        let token = self.expect(TokenKind::KwEnum)?;
        let name = self.expect(TokenKind::Ident)?;

        let mut fields = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.expect(TokenKind::KwEnd)?;
                break;
            }

            let ident = self.expect(TokenKind::Ident)?;
            fields.push(ident.literal.to_string());

            if let Some(token) = self.peek() {
                if token.kind != TokenKind::KwEnd {
                    self.expect(TokenKind::Comma)?;
                    break;
                }
            }
        }

        Ok(ToplevelStmt::EnumDecl {
            name: name.literal.to_string(),
            fields,
        })
    }

    fn parse_function_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // function name(param:type, param:type, ...): return_type
        //  body
        // end

        let mut return_type = Ty::Void;
        self.expect(TokenKind::KwFunction)?;
        let name = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_annotated_params()?;
        self.expect(TokenKind::RParen)?;

        // Parse return type if it is present
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Colon {
                self.advance();
                return_type = self.parse_type()?;
            }
        }

        let body = self.parse_block()?;

        Ok(ToplevelStmt::FunctionDecl {
            name: name.literal.to_string(),
            params,
            return_ty: return_type,
            body,
        })
    }
}
