use crate::syntax::ast::{Block, Expr, Stmt, ToplevelStmt, Ty};
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::TokenKind;
use crate::syntax::parse;
use crate::syntax::parse::Parser;
use crate::syntax::span::{Spanned, spanned};

type ParameterPair = (String, Ty);

/// Implement parsing statements and top-level statements
///
/// A few functions are marked `pub(crate)` to be used in other modules since they may come in handy
impl<'cx> Parser<'cx> {
    pub(crate) fn parse_type(&mut self) -> parse::Return<Ty> {
        // Parse a type like `int`, `float` or `*int`
        let prev_if_any = self.peek();
        let token = self.next().ok_or(SyntaxError::UnexpectedEof)?;

        match &token.kind {
            TokenKind::Void => Ok(Ty::Void),
            TokenKind::Int => Ok(Ty::Int),
            TokenKind::Float => Ok(Ty::Float),
            TokenKind::Double => Ok(Ty::Double),
            TokenKind::String => Ok(Ty::String),
            TokenKind::Bool => Ok(Ty::Bool),
            TokenKind::Star => { // *type
                let inner = self.parse_type()?;
                Ok(Ty::Pointer(Box::new(inner)))
            }
            TokenKind::LSquare => { // []type
                self.expect_error(
                    TokenKind::RSquare,
                    SyntaxError::UnbalancedBrackets {
                        location: token.location
                    },
                )?;

                let inner = self.parse_type()?;
                Ok(Ty::Array(Box::new(inner)))
            }
            TokenKind::Ident => {
                let ident = token.literal.to_string();
                Ok(Ty::Named(ident))
            }

            // Invalid or unimplemented type
            _ => Err(SyntaxError::UnexpectedToken {
                token: token.kind,
                location: token.location.clone(),
            })
        }
    }

    pub(crate) fn parse_annotated_param(&mut self) -> parse::Return<ParameterPair> {
        let ident = self.expect(TokenKind::Ident)?.literal.to_string();
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok((ident, ty))
    }

    pub(crate) fn parse_annotated_params(&mut self) -> parse::Return<Vec<ParameterPair>> {
        let mut params = vec![];

        // Parse a comma seperated list of annotated parameters i.e. `a:int, b:float`
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Ident => {
                    let param = self.parse_annotated_param()?;
                    params.push(param);
                }
                TokenKind::RParen => {
                    self.next();
                    break;
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        token: token.kind.clone(),
                        location: token.location.clone(),
                    });
                }
            }
        }

        Ok(params)
    }

    pub(crate) fn parse_toplevel_stmt(&mut self) -> parse::Return<ToplevelStmt> {
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwImport => self.parse_import(),
                TokenKind::KwStruct => self.parse_struct_decl(),
                TokenKind::KwEnum => self.parse_enum_decl(),
                TokenKind::KwFunction => self.parse_function_decl(),
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    location: token.location.clone(),
                }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        }
    }

    pub(crate) fn parse_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwLet => self.parse_let_stmt(),
                TokenKind::Ident => self.parse_expr_stmt(),
                TokenKind::KwIf => self.parse_if_stmt(),
                TokenKind::KwFor => self.parse_for_stmt(),
                TokenKind::KwWhile => self.parse_while_stmt(),
                TokenKind::KwReturn => self.parse_return_stmt(),
                // Unexpected token
                // Commented until I add support for comments
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    location: token.location.clone(),
                }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        }
    }

    pub(crate) fn parse_block(&mut self) -> parse::Return<Block> {
        let mut stmts = vec![];

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.next();
                break;
            }
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::KwEnd)?;

        Ok(stmts)
    }

    // Statements

    fn parse_import_path(&mut self) -> parse::Return<Vec<String>> {
        let mut path = vec![];

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Ident => {
                    path.push(token.literal.to_string());
                    self.next();
                }
                TokenKind::Dot => {
                    self.next();
                }
                _ => {
                    return Err(SyntaxError::InvalidPathChar {
                        location: token.location.clone(),
                    });
                }
            }
        }

        Ok(path)
    }

    fn parse_import(&mut self) -> parse::Return<ToplevelStmt> {
        // import foo.bar [as baz]
        let peeked_token = self.expect(TokenKind::KwImport)?;
        let path = self.parse_import_path()?;

        if let Some(token) = self.peek() {
            if token.kind == TokenKind::KwAs {
                // import foo.bar as baz
                self.next();
                let alias = self.expect(TokenKind::Ident)?.literal.to_string();
                return Ok(ToplevelStmt::Import {
                    path,
                    alias: Some(alias),
                });
            } else {
                // import foo.bar
                return Ok(ToplevelStmt::Import { path, alias: None });
            }
        }

        // Expected an identifier after import
        Err(SyntaxError::ExpectedIdent {
            location: peeked_token.location.clone(),
        })
    }

    fn parse_let_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
        let token = self.expect(TokenKind::KwLet)?;
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
            token.location,
        ))
    }

    fn parse_expr_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
        let peeked_token = self.peek().ok_or(SyntaxError::UnexpectedEof)?;

        fn parse_reassign_stmt<'cx>(p: &mut Parser<'cx>) -> parse::Return<'cx, Spanned<Stmt>> {
            let ident = p.expect(TokenKind::Ident)?;
            p.expect(TokenKind::Equal)?;
            let expr = p.parse_expr()?;
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

        fn parse_call_stmt<'cx>(p: &mut Parser<'cx>) -> parse::Return<'cx, Spanned<Stmt>> {
            let expr = p.parse_expr()?;
            Ok(spanned(Stmt::Expr(expr.clone()), expr.location.clone()))
        }

        // Check if it's a re-assignment or a function call
        match peeked_token.kind {
            TokenKind::Equal => parse_reassign_stmt(self),
            TokenKind::LParen => parse_call_stmt(self),

            // Unexpected token
            _ => Err(SyntaxError::UnexpectedToken {
                token: peeked_token.kind.clone(),
                location: peeked_token.location.clone(),
            }),
        }
    }

    fn parse_if_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
        // if cond then
        //   then_block
        // [else]?
        //   [else_block]?
        // end

        let token = self.expect(TokenKind::KwIf)?;
        let condition = self.parse_expr()?;

        self.expect(TokenKind::KwThen)?;
        let then_block = self.parse_block()?;

        let else_block = if let Some(token) = self.peek() {
            if matches!(token.kind, TokenKind::KwElse) {
                self.next();
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

    fn parse_for_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
        // for initializer = start,end do
        //   block
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

    fn parse_while_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
        // while cond do
        //   block
        // end

        let token = self.expect(TokenKind::KwWhile)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::KwDo)?;
        let block = self.parse_block()?;
        Ok(spanned(Stmt::While { cond, block }, token.location))
    }

    fn parse_return_stmt(&mut self) -> parse::Return<Spanned<Stmt>> {
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

    fn parse_struct_decl(&mut self) -> parse::Return<ToplevelStmt> {
        // field = ident : type

        // struct Name
        //   field*
        // end

        let token = self.expect(TokenKind::KwStruct)?;
        let name = self.expect(TokenKind::Ident)?.literal.to_string();

        let mut fields = vec![];
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.expect(TokenKind::KwEnd)?;
                break;
            }

            // Parse as many fields as we can
            let ident = self.expect(TokenKind::Ident)?.literal.to_string();
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            fields.push((ident, ty));

            // Check if there are more fields
            if let Some(token) = self.peek() {
                if token.kind != TokenKind::KwEnd {
                    self.expect(TokenKind::Comma)?;
                }
            }
        }

        Ok(ToplevelStmt::StructDecl { name, fields })
    }

    fn parse_enum_decl(&mut self) -> parse::Return<ToplevelStmt> {
        // field = ident

        // enum Name
        //   field*
        // end

        let token = self.expect(TokenKind::KwEnum)?;
        let name = self.expect(TokenKind::Ident)?.literal.to_string();

        let mut fields = vec![];
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                self.expect(TokenKind::KwEnd)?;
                break;
            }

            // Parse as many fields as we can
            let ident = self.expect(TokenKind::Ident)?.literal.to_string();
            fields.push(ident);

            // Check if there are more fields
            if let Some(token) = self.peek() {
                if token.kind != TokenKind::KwEnd {
                    self.expect(TokenKind::Comma)?;
                }
            }
        }

        Ok(ToplevelStmt::EnumDecl { name, fields })
    }

    fn parse_function_decl(&mut self) -> parse::Return<ToplevelStmt> {
        // param = ident: type

        // function name(params*): return_type?
        //   block
        // end

        let mut return_type = Ty::Void;

        self.expect(TokenKind::KwFunction)?;
        let name = self.expect(TokenKind::Ident)?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_annotated_params()?;
        self.expect(TokenKind::RParen)?;

        // Parse return type if present
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Colon {
                self.next();
                return_type = self.parse_type()?;
            }
        }

        let body = self.parse_block()?;

        Ok(ToplevelStmt::FunctionDecl {
            name: name.literal.to_string(),
            return_ty: return_type,
            params,
            body,
        })
    }
}