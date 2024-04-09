use super::{ast::Stmt, ast::Ty, errors::SyntaxError};
use super::ast::{Ast, Block, Expr, ToplevelStmt};
use super::lexer::*;
use super::span::{spanned, Spanned};

// Sub-parsers
pub mod expr;
pub mod import_stmt;

/// Result type for parsing
type Return<'cx, T> = Result<T, SyntaxError>;
type ParameterPair = (String, Ty);

pub struct Parser<'cx> {
    tokens: LexerIter<'cx>,
    errors: Vec<SyntaxError>,
    pos: usize,
}

impl TokenKind {
    fn is_op(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Equal
            | TokenKind::BangEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => true,
            _ => false,
        }
    }

    fn is_unop(&self) -> bool {
        match self {
            TokenKind::Minus | TokenKind::Bang => true,
            _ => false,
        }
    }

    fn get_precedence(&self) -> u8 {
        match self {
            TokenKind::Plus | TokenKind::Minus => 10,
            TokenKind::Star | TokenKind::Slash => 20,
            TokenKind::Equal | TokenKind::BangEqual => 5,
            TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => 5,

            // Either not a binary operator or not implemented yet
            _ => 0,
        }
    }
}

impl<'cx> Parser<'cx> {
    pub fn new(src: &'cx str) -> Self {
        let tokens = lex_tokens(src);
        Parser {
            tokens,
            errors: Vec::new(),
            pos: 0,
        }
    }

    /// Peek at the next token
    pub fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    /// Advance the parser by one token
    pub fn advance(&mut self) -> Option<Token<'cx>> {
        self.pos += 1;
        let token = self.tokens.next();
        token
    }

    /// Check if the next token is of the expected kind without advancing
    pub fn check(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token> {
        match self.peek() {
            Some(token) if token.kind == kind => Ok(token),
            _ => Err(err),
        }
    }

    /// Consume the next token and return it if it matches the expected kind
    pub fn expect(&mut self, kind: TokenKind) -> Return<Token<'cx>> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        if token.kind == kind {
            Ok(token)
        } else {
            Err(SyntaxError::UnexpectedToken {
                token: token.kind,
                location: token.location,
            })
        }
    }

    /// Remap the error from `expect` to a custom error passed in
    pub fn expect_error(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token<'cx>> {
        self.expect(kind).map_err(|_| err)
    }

    fn parse_type(&mut self) -> Return<Ty> {
        let token = self.advance().ok_or(SyntaxError::UnexpectedEof)?;
        match token.kind {
            TokenKind::Void => Ok(Ty::Void),
            TokenKind::Int => Ok(Ty::Int),
            TokenKind::Float => Ok(Ty::Float),
            TokenKind::String => Ok(Ty::String),
            TokenKind::Bool => Ok(Ty::Bool),
            TokenKind::Star => {
                // *type
                let ty = self.parse_type()?;
                Ok(Ty::Pointer(Box::new(ty)))
            }
            TokenKind::LSquare => {
                // []type
                self.expect_error(
                    TokenKind::RSquare,
                    SyntaxError::UnmatchedBrackets {
                        location: token.location,
                    },
                )?;
                let ty = self.parse_type()?;
                Ok(Ty::Array(Box::new(ty)))
            }
            TokenKind::Ident => {
                // User defined type
                let ident = token.literal.to_string();
                Ok(Ty::UserDefined(ident))
            }

            // Invalid type
            _ => Err(SyntaxError::UnexpectedToken {
                token: token.kind,
                location: token.location,
            }),
        }
    }

    fn parse_annotated_param(&mut self) -> Return<ParameterPair> {
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok((ident.literal.to_string(), ty))
    }

    fn parse_annotated_params(&mut self) -> Return<Vec<ParameterPair>> {
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

    fn parse_params(&mut self) -> Return<Vec<String>> {
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

    fn parse_toplevel_stmt(&mut self) -> Return<ToplevelStmt> {
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwImport => self.parse_import(),
                TokenKind::KwFunction => self.parse_function_declare(),
                _ => Err(SyntaxError::UnexpectedToken {
                    token: token.kind.clone(),
                    location: token.location.clone(),
                }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        }
    }

    // FIXME: change this back to private
    pub fn parse_stmt(&mut self) -> Return<Spanned<Stmt>> {
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwLocal => self.parse_local_declare(),
                TokenKind::KwStruct => self.parse_struct_declare(),
                TokenKind::Ident => self.parse_assignment(),
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
        }
    }

    // Statements
    fn parse_local_declare(&mut self) -> Return<Spanned<Stmt>> {
        // local ident:type = expr
        let t = self.expect(TokenKind::KwLocal)?;
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;

        Ok(spanned(
            Stmt::Local {
                name: ident.literal.to_string(),
                ty: Some(ty),
                value: Some(expr),
            },
            t.location,
        ))
    }

    fn parse_assignment(&mut self) -> Return<Spanned<Stmt>> {
        // ident = expr
        let ident = self.expect(TokenKind::Ident)?;
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

    fn parse_if_stmt(&mut self) -> Return<Spanned<Stmt>> {
        let t = self.expect(TokenKind::KwIf)?;
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
            t.location,
        ))
    }

    fn parse_for_loop(&mut self) -> Return<Spanned<Stmt>> {
        // for counter = start, end do
        //  body
        // end

        let t = self.expect(TokenKind::KwFor)?;
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
            t.location,
        ))
    }

    fn parse_while_loop(&mut self) -> Return<Spanned<Stmt>> {
        // while condition do
        //  body
        // end

        let t = self.expect(TokenKind::KwWhile)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::KwDo)?;
        let block = self.parse_block()?;
        Ok(spanned(Stmt::While { cond, block }, t.location))
    }

    fn parse_return(&mut self) -> Return<Spanned<Stmt>> {
        // return expr?
        let t = self.expect(TokenKind::KwReturn)?;
        let expr = if let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                None
            } else {
                Some(self.parse_value()?)
            }
        } else {
            None
        };

        Ok(spanned(Stmt::Return(expr), t.location))
    }

    // NOTE: Maybe come up with a better syntax for this?
    fn parse_struct_declare(&mut self) -> Return<Spanned<Stmt>> {
        // struct name = { field* }
        let t = self.expect(TokenKind::KwStruct)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Equal)?;

        self.expect(TokenKind::LCurly)?;
        let mut fields = Vec::new();

        while let Some(_token) = self.peek() {
            let ident = self.expect(TokenKind::Ident)?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            fields.push((ident.literal.to_string(), ty));

            if let Some(token) = self.peek() {
                if token.kind == TokenKind::RCurly {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
        }

        self.expect(TokenKind::RCurly)?;
        Ok(spanned(
            Stmt::StructDecl {
                name: name.literal.to_string(),
                fields,
            },
            t.location,
        ))
    }

    fn parse_function_declare(&mut self) -> Return<ToplevelStmt> {
        // function name(params):return_type?
        //  body
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
                self.advance();
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

    // FIXME: change this back to private
    pub fn parse_block(&mut self) -> Return<Block> {
        let mut stmts = Vec::new();

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::KwEnd)?;

        Ok(stmts)
    }

    pub fn parse(src: &'cx str) -> Return<'cx, Ast> {
        // node: (function_declare | stmt)
        // nodes: node*
        let mut parser = Parser::new(src);

        let mut comment_nodes: Vec<(SourceLoc, String)> = Vec::new();
        let mut toplevels: Vec<Spanned<ToplevelStmt>> = Vec::new();
        let mut stmts: Vec<Spanned<Stmt>> = Vec::new();

        while let Some(token) = parser.peek() {
            match token.kind {
                TokenKind::Comment => {
                    // TODO: Skip comments while still storing them in the AST
                    let _location = token.location.clone();
                    parser.advance();

                    // TODO: fix the below code giving me a borrow checker error
                    //comment_nodes.push((location, token.literal.clone().to_string()));
                }
                TokenKind::KwFunction => {
                    // Functions are top-level nodes
                    let location = token.location.clone();
                    toplevels.push(spanned(parser.parse_function_declare()?, location));
                }

                // Statements can exist outside a function or block
                _ => {
                    let location = token.location.clone();
                    stmts.push(spanned(parser.parse_stmt()?.target, location));
                }
            }
        }

        Ok(Ast {
            comments: comment_nodes,
            toplevels,
            stmts,
        })
    }
}

// Tests
#[cfg(test)]
mod parser_tests {
    use crate::syntax::parse::Parser;

    #[test]
    fn test_parser() {
        // Windows uses CRLF which sucks and breaks everything
        let src = std::fs::read_to_string("syntax_tests/parse01.ds").expect("Failed to read file");

        println!("{}", src);

        let nodes = Parser::parse(src.as_str()).unwrap();

        println!("{:#?}", nodes);

        //assert_eq!(nodes.len(), 2);
    }
}
