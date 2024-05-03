//! Parsing of statements and blocks.
//! The expression parser is split into another file to keep the codebase clean and organized.

use crate::backend::module_info::ModuleInfo;
use crate::parser;
use crate::syntax::ast::*;
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{Token, TokenKind};
use crate::utils::{spanned, Spanned};

use super::Parser;

impl<'cx> Parser<'cx> {
    pub(crate) fn parse_block(&mut self) -> parser::Return<Block> {
        let mut stmts = Vec::new();

        'parse_stmts: while let Some(token) = self.peek() {
            if matches!(token.kind, TokenKind::KwEnd) {
                break 'parse_stmts;
            }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        self.expect(TokenKind::KwEnd)?;

        Ok(stmts)
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

                TokenKind::Comment => {
                    // Skip comments
                    self.advance();
                    self.parse_stmt()
                }
                // Unexpected token
                // Commented until I add support for comments
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    expected_any: vec![
                        TokenKind::KwLet,
                        TokenKind::Ident,
                        TokenKind::KwIf,
                        TokenKind::KwFor,
                        TokenKind::KwWhile,
                        TokenKind::KwReturn,
                    ],
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
                    expected_any: vec![TokenKind::Equal, TokenKind::LParen],
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

    #[allow(unused_assignments)]
    #[allow(unused_mut)]
    fn parse_if_stmt(&mut self) -> parser::Return<Spanned<Stmt>> {
        let token = self.expect(TokenKind::KwIf)?;
        let condition = self.parse_expr()?;

        self.expect(TokenKind::KwThen)?;

        // HACK: Parse the block inline because I can't be assed to figure out how
        // to handle the else block properly.
        let mut has_else_block = false;

        let mut then_block: Vec<Spanned<Stmt>> = Vec::new();
        let mut else_block: Vec<Spanned<Stmt>> = Vec::new();

        while let Some(_) = self.peek() {
            // Parse as many statements as we can until we reach either end or else
            let stmt = self.parse_stmt()?;
            then_block.push(stmt);

            if let Some(token) = self.peek() {
                match token.kind {
                    TokenKind::KwEnd => break,
                    TokenKind::KwElse => {
                        has_else_block = true;
                        break;
                    }
                    _ => continue,
                }
            }
        }

        if !has_else_block {
            // Expect an end if we don't have an else block
            self.expect(TokenKind::KwEnd)?;
        } else {
            // Parse the else block
            self.expect(TokenKind::KwElse)?;
            while let Some(_) = self.peek() {
                let stmt = self.parse_stmt()?;
                else_block.push(stmt);

                if let Some(token) = self.peek() {
                    if token.kind == TokenKind::KwEnd {
                        break;
                    }
                }
            }
            self.expect(TokenKind::KwEnd)?;
        }

        Ok(spanned(
            Stmt::If {
                cond: condition,
                then_block,
                else_block: if has_else_block {
                    Some(else_block)
                } else {
                    None
                },
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

    // Top-level statement parser
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
                    expected_any: vec![
                        TokenKind::KwStruct,
                        TokenKind::KwEnum,
                        TokenKind::KwFunction,
                        TokenKind::KwExtern,
                    ],
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

    fn parse_module_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // module name
        //    functions
        // end
        let token = self.expect(TokenKind::KwModule)?;
        let name = self.expect(TokenKind::Ident)?;

        let mut module_info = ModuleInfo::new(name.literal.to_string());

        todo!("Implement module parsing")
    }

    fn parse_extern_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // External functions imported from C
        //
        // Expect these to be replaced with modules which support namespacing _very_ soon
        // because they only exist as a quick and dirty FFI hack.

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
