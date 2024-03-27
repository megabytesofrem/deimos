// Sub-parsers
pub mod expr;
pub mod import_stmt;

use super::ast::{Block, Expr, ToplevelStmt};
use super::lexer::*;
use super::{ast::Stmt, ast::Ty, errors::SyntaxError};

/// Result type for parsing
type ParseResult<'a, T> = Result<T, SyntaxError>;
type ParameterPair = (String, Ty);

pub struct Parser<'a> {
    tokens: LexerIter<'a>,
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

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        let tokens = lex_tokens(src);
        Parser {
            tokens,
            errors: Vec::new(),
            pos: 0,
        }
    }

    /// Peek at the next token
    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    /// Advance the parser by one token
    pub fn advance(&mut self) -> Option<Token<'a>> {
        self.pos += 1;
        let token = self.tokens.next();
        token
    }

    /// Check if the next token is of the expected kind without advancing
    pub fn check(&mut self, kind: TokenKind, err: SyntaxError) -> ParseResult<&Token> {
        match self.peek() {
            Some(token) if token.kind == kind => Ok(token),
            _ => Err(err),
        }
    }

    /// Consume the next token and return it if it matches the expected kind
    pub fn expect(&mut self, kind: TokenKind) -> ParseResult<Token<'a>> {
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
    pub fn expect_error(&mut self, kind: TokenKind, err: SyntaxError) -> ParseResult<Token<'a>> {
        self.expect(kind).map_err(|_| err)
    }

    fn parse_type(&mut self) -> ParseResult<Ty> {
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

    fn parse_annotated_param(&mut self) -> ParseResult<ParameterPair> {
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok((ident.literal.to_string(), ty))
    }

    fn parse_annotated_params(&mut self) -> ParseResult<Vec<ParameterPair>> {
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

    fn parse_params(&mut self) -> ParseResult<Vec<String>> {
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

    fn parse_toplevel_stmt(&mut self) -> ParseResult<ToplevelStmt> {
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::KwImport => self.parse_import(),
                TokenKind::KwFunction => self.parse_function_declare(),
                _ => {
                    let stmt = self.parse_stmt()?;
                    Ok(ToplevelStmt::Stmt(stmt))
                }
            },
            None => Err(SyntaxError::UnexpectedEof),
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
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
                _ => unimplemented!(),
                // Commented until I add support for comments
                // _ => Err(SyntaxError::UnexpectedToken {
                //     token: token.kind.clone(),
                //     location: token.location.clone(),
                // }),
            },
            None => Err(SyntaxError::UnexpectedEof),
        }
    }

    // Statements
    fn parse_local_declare(&mut self) -> ParseResult<Stmt> {
        // local ident:type = expr
        self.expect(TokenKind::KwLocal)?;
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Local {
            name: ident.literal.to_string(),
            ty: Some(ty),
            value: Some(expr),
        })
    }

    fn parse_assignment(&mut self) -> ParseResult<Stmt> {
        // ident = expr
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Assign {
            target: Expr::Variable(ident.literal.to_string()),
            value: expr,
        })
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect(TokenKind::KwIf)?;
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

        Ok(Stmt::If {
            cond: condition,
            then_block,
            else_block,
        })
    }

    fn parse_for_loop(&mut self) -> ParseResult<Stmt> {
        // for counter = start, end do
        // .. body
        // end

        self.expect(TokenKind::KwFor)?;
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Equal)?;
        let start = self.parse_expr()?;
        self.expect(TokenKind::Comma)?;
        let end = self.parse_expr()?;

        self.expect(TokenKind::KwDo)?;
        let body = self.parse_block()?;

        Ok(Stmt::For {
            init: ident.literal.to_string(),
            from: start,
            to: end,
            body,
        })
    }

    fn parse_while_loop(&mut self) -> ParseResult<Stmt> {
        // while condition do
        // .. body
        // end

        self.expect(TokenKind::KwWhile)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::KwDo)?;
        let block = self.parse_block()?;
        Ok(Stmt::While { cond, block })
    }

    fn parse_return(&mut self) -> ParseResult<Stmt> {
        // return expr?
        self.expect(TokenKind::KwReturn)?;
        let expr = if let Some(token) = self.peek() {
            if token.kind == TokenKind::KwEnd {
                None
            } else {
                Some(self.parse_value()?)
            }
        } else {
            None
        };

        Ok(Stmt::Return(expr))
    }

    // NOTE: Maybe come up with a better syntax for this?
    fn parse_struct_declare(&mut self) -> ParseResult<Stmt> {
        // struct name = { field* }
        self.expect(TokenKind::KwStruct)?;
        let name = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Equal)?;

        self.expect(TokenKind::LCurly)?;
        let mut fields = Vec::new();

        while let Some(token) = self.peek() {
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
        Ok(Stmt::StructDecl {
            name: name.literal.to_string(),
            fields,
        })
    }

    fn parse_function_declare(&mut self) -> ParseResult<ToplevelStmt> {
        // function name(params):return_type?
        // .. body
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

    fn parse_block(&mut self) -> ParseResult<Block> {
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

    pub fn parse(&mut self) -> ParseResult<Vec<ToplevelStmt>> {
        // node: (function_declare | stmt)
        // nodes: node*
        let mut nodes = Vec::new();

        while let Some(token) = self.peek() {
            match token.kind {
                // TODO: Skip comments while still storing them in the AST
                TokenKind::Comment => {
                    self.advance();
                }
                TokenKind::KwFunction => {
                    nodes.push(self.parse_function_declare()?);
                }
                _ => {
                    nodes.push(ToplevelStmt::Stmt(self.parse_stmt()?));
                }
            }
        }

        Ok(nodes)
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

        let mut parser = Parser::new(src.as_str());
        let nodes = parser.parse().unwrap();

        println!("{:#?}", nodes);

        //assert_eq!(nodes.len(), 2);
    }
}
