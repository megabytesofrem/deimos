//! Parsing of statements and blocks.
//! The expression parser is split into another file to keep the codebase clean and organized.

use crate::spanned::{spanned, Spanned};
use crate::syntax::ast::*;
use crate::syntax::parser::{self, syntax_error::SyntaxError};
use crate::syntax::{ast_types::Ty, lexer::TokenKind};

use super::Parser;

impl<'p> Parser<'p> {
    pub(crate) fn parse_block(&mut self) -> parser::Return<Block> {
        let mut stmts = Vec::new();

        'parse_stmts: while let Some(token) = self.peek() {
            if matches!(
                token.kind,
                TokenKind::KwElif | TokenKind::KwElse | TokenKind::KwEnd
            ) {
                break 'parse_stmts;
            }

            let stmt = self.parse_stmt()?;

            if stmt.target != Stmt::BlockTerminator {
                // `end` and `else` are a special case since it is a block terminator
                stmts.push(stmt.clone());
            }
        }

        self.expect_one_of(vec![TokenKind::KwElif, TokenKind::KwElse, TokenKind::KwEnd])?;

        Ok(stmts)
    }

    pub(crate) fn parse_stmt(&mut self) -> parser::Return<Spanned<Stmt>> {
        let result = match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwLet => self.parse_let_stmt(),
                TokenKind::Name => self.parse_expr_stmt(),
                TokenKind::KwIf => self.parse_if_stmt(),
                TokenKind::KwFor => self.parse_for_loop(),
                TokenKind::KwWhile => self.parse_while_loop(),
                //TokenKind::KwMatch => self.parse_match(),
                TokenKind::KwReturn => self.parse_return(),

                TokenKind::Comment => {
                    // Skip comments
                    self.advance();
                    self.parse_stmt()
                }
                TokenKind::KwElif | TokenKind::KwElse | TokenKind::KwEnd => {
                    // `end` and `else` are a special case since it is a block terminator
                    //self.advance();
                    Ok(spanned(Stmt::BlockTerminator, token.location.clone()))
                }

                // Unexpected token
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    expected_any: vec![
                        TokenKind::KwLet,
                        TokenKind::Name,
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
        // println!("Parsing let stmt");
        // let ident:type = expr
        let t = self.expect(TokenKind::KwLet)?;
        let ident = self.expect(TokenKind::Name)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let mut value: Option<Spanned<Expr>> = None;

        // Optionally expect an equal sign and an expression
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Equal {
                self.advance();
                let expr = self.parse_expr()?; // Only parse if there is an '='
                value = Some(expr);
            }
        }

        // println!("Peeked: {:#?}", self.peek());
        // println!("value: {:#?}", value);

        Ok(spanned(
            Stmt::Let {
                name: ident.literal.to_string(),
                ty: Some(ty),
                value,
            },
            t.location,
        ))
    }

    // Parses an expression statement which can be one of the following:
    // - An assignment
    // - A function call expression
    // - An array indexer expression
    fn parse_expr_stmt(&mut self) -> parser::Return<Spanned<Stmt>> {
        let token = self.peek().unwrap();
        match &token.kind {
            TokenKind::Name => {
                let location = token.location.clone();
                let ident = spanned(Expr::Ident(token.literal.to_string()), location);
                self.advance(); // consume the name

                let lvalue = self.parse_postfix_operators(ident)?;

                if let Some(token) = self.peek() {
                    match token.kind {
                        TokenKind::Equal => {
                            self.advance(); // consume the equal sign

                            let rvalue = self.parse_expr()?;
                            let location = rvalue.location.clone();
                            Ok(spanned(
                                Stmt::Assign {
                                    name: lvalue,
                                    value: rvalue,
                                },
                                location,
                            ))
                        }
                        // If the next token isn't an equals sign then it's just an expression
                        _ => {
                            let location = lvalue.location.clone();
                            Ok(spanned(Stmt::Expr(lvalue), location))
                        }
                    }
                } else {
                    // If there is no next token then we're at the end of the
                    // file and it's just an expression like a function call or
                    // something like that.
                    let location = lvalue.location.clone();
                    Ok(spanned(Stmt::Expr(lvalue), location))
                }
            }
            _ => Err(SyntaxError::UnexpectedToken {
                token: token.kind.clone(),
                expected_any: vec![TokenKind::Name],
                location: token.location.clone(),
            }),
        }
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
        let mut elif_blocks: Vec<(Spanned<Expr>, Block)> = Vec::new();
        let mut else_block: Vec<Spanned<Stmt>> = Vec::new();

        while let Some(_) = self.peek() {
            // Parse as many statements as we can until we reach either end or else
            let stmt = self.parse_stmt()?;

            if stmt.target != Stmt::BlockTerminator {
                // `end` and `else` are a special case since it is a block terminator
                then_block.push(stmt);
            }

            if let Some(token) = self.peek() {
                match token.kind {
                    TokenKind::KwEnd => {
                        self.advance();
                        break;
                    }
                    TokenKind::KwElif => {
                        let (cond, block) = self.parse_elif_block()?;
                        elif_blocks.push((cond, block));
                    }
                    TokenKind::KwElse => {
                        has_else_block = true;
                        break;
                    }
                    _ => continue,
                }
            }
        }

        if has_else_block {
            else_block = self.parse_else_block()?;
        }

        Ok(spanned(
            Stmt::If {
                cond: condition,
                then_block,
                elif_blocks,
                else_block: if has_else_block {
                    Some(else_block)
                } else {
                    None
                },
            },
            token.location,
        ))
    }

    fn parse_elif_block(&mut self) -> parser::Return<(Spanned<Expr>, Block)> {
        self.expect(TokenKind::KwElif)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::KwThen)?;

        // Parse a block, and then either an elif and a subsequent block, or an else block
        let mut stmts: Vec<Spanned<Stmt>> = Vec::new();

        while let Some(token) = self.peek() {
            // Parse as many statements until we reach either elif or else
            let stmt = self.parse_stmt()?;

            if matches!(
                token.kind,
                TokenKind::KwElif | TokenKind::KwElse | TokenKind::KwEnd
            ) {
                break;
            }

            if stmt.target != Stmt::BlockTerminator {
                // `end` and `else` are a special case since it is a block terminator
                stmts.push(stmt);
            }
        }

        Ok((cond, stmts))
    }

    fn parse_else_block(&mut self) -> parser::Return<Block> {
        self.expect(TokenKind::KwElse)?;
        self.parse_block()
    }

    fn parse_for_loop(&mut self) -> parser::Return<Spanned<Stmt>> {
        // for counter = start, end do
        //  body
        // end

        let token = self.expect(TokenKind::KwFor)?;
        let ident = self.expect(TokenKind::Name)?;
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
        let body = self.parse_block()?;
        Ok(spanned(Stmt::While { cond, body }, token.location))
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
                TokenKind::KwImport => self.parse_import(),
                TokenKind::KwStruct => self.parse_struct_declare(),
                TokenKind::KwEnum => self.parse_enum_declare(),
                TokenKind::KwFunction => self.parse_function_declare(),
                TokenKind::KwExtern => self.parse_extern_declare(),

                // Unexpected token
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    expected_any: vec![
                        TokenKind::KwImport,
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

    fn parse_dotted_path(&mut self) -> parser::Return<Vec<String>> {
        // Collected sum of parts
        let mut parts = Vec::new();

        // HACK: Store the error outside so we can return it since the inner `match`
        // can't return a value directly or it would exit the loop
        //
        // We still need to store the error, hence this
        let mut error: Option<SyntaxError> = None;

        // TODO: Clean this up
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Name => {
                    parts.push(token.literal.to_string());

                    // Consume the token
                    self.advance();
                }

                TokenKind::Dot => {
                    // Consume the dot
                    self.advance();

                    if let Some(next_token) = self.peek() {
                        match next_token.kind {
                            TokenKind::Name => {
                                // Push the name token after the dot
                                let name = next_token.literal.to_string();
                                parts.push(name);

                                // Consume the name token
                                self.advance();
                            }

                            _ => {
                                error = Some(SyntaxError::UnexpectedToken {
                                    token: next_token.kind.clone(),
                                    expected_any: vec![TokenKind::Name],
                                    location: next_token.location.clone(),
                                });

                                // Consume the unexpected token and break out of the loop
                                self.advance();
                                break;
                            }
                        }
                    } else {
                        return Err(SyntaxError::UnexpectedEof);
                    }
                }

                _ => {
                    break;
                }
            }
        }

        if let Some(err) = error {
            Err(err)
        } else {
            Ok(parts)
        }
    }

    fn parse_import(&mut self) -> parser::Return<ToplevelStmt> {
        // import module.path [as alias]
        self.expect(TokenKind::KwImport)?;
        let dotted_path = self.parse_dotted_path()?;

        let mut optional_alias: Option<String> = None;

        if let Some(token) = self.peek() {
            if token.kind == TokenKind::KwAs {
                self.advance();
                let alias = self.expect(TokenKind::Name)?;
                optional_alias = Some(alias.literal.to_string());
            }
        }

        Ok(ToplevelStmt::Import {
            path: dotted_path,
            alias: optional_alias,
        })
    }

    fn parse_extern_declare(&mut self) -> parser::Return<ToplevelStmt> {
        // FIXME/TODO: Remove this now we have import

        // External functions imported from C
        //
        // Expect these to be replaced with modules which support namespacing _very_ soon
        // because they only exist as a quick and dirty FFI hack.

        // extern cfunction_name(param:type, param:type, ...): return_type?
        let mut return_type: Ty = Ty::Void;
        self.expect(TokenKind::KwExtern)?;
        self.expect(TokenKind::KwFunction)?;
        let name = self.expect(TokenKind::Name)?;

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
        let name = self.expect(TokenKind::Name)?;

        let mut fields = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.expect(TokenKind::KwEnd)?;
                break;
            }

            let ident = self.expect(TokenKind::Name)?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            fields.push((ident.literal.to_string(), ty));

            if let Some(token) = self.peek() {
                if token.kind != TokenKind::KwEnd {
                    self.expect(TokenKind::Comma)?;
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
        let name = self.expect(TokenKind::Name)?;

        let mut fields = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.expect(TokenKind::KwEnd)?;
                break;
            }

            let ident = self.expect(TokenKind::Name)?;
            fields.push(ident.literal.to_string());

            if let Some(token) = self.peek() {
                if token.kind != TokenKind::KwEnd {
                    self.expect(TokenKind::Comma)?;
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
        let name = self.expect(TokenKind::Name)?;

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
