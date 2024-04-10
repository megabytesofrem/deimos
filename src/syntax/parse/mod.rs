use crate::syntax::ast::{Ast, ToplevelStmt};
use crate::syntax::errors::SyntaxError;
use crate::syntax::lexer::{LexerIter, SourceLoc, Token, TokenKind};
use crate::syntax::span::{spanned, Spanned};

mod parse_stmt;
mod parse_expr;

pub struct Parser<'cx> {
    tokens: LexerIter<'cx>,
    pos: usize,

    // Bubble up syntax errors encountered during parsing
    errors: Vec<SyntaxError>,
}

/// Result type for parsing
type Return<'cx, T> = Result<T, SyntaxError>;

impl<'cx> Parser<'cx> {
    pub fn new(tokens: LexerIter<'cx>) -> Self {
        Parser {
            tokens,
            pos: 0,
            errors: vec![],
        }
    }

    pub(crate) fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    /// Advance the parser by one token and return the consumed token.
    pub(crate) fn next(&mut self) -> Option<Token<'cx>> {
        self.pos += 1;
        self.tokens.next()
    }

    /// Check if the next token is of the expected kind without consuming it.
    pub(crate) fn check(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token> {
        match self.peek() {
            Some(token) if token.kind == kind => Ok(token.clone()),
            _ => Err(err.clone()),
        }
    }

    /// Consume the next token and return it, if it is of the expected kind.
    pub(crate) fn expect(&mut self, kind: TokenKind) -> Return<Token<'cx>> {
        let token = self.next().ok_or(SyntaxError::UnexpectedEof)?;
        if token.kind == kind {
            Ok(token.clone())
        } else {
            let err = SyntaxError::UnexpectedToken {
                token: token.kind,
                location: token.location.clone(),
            };

            self.errors.push(err.clone());
            Err(err)
        }
    }

    /// Remap the error from `expect` to a more useful error kind.
    ///
    /// `expect` is used when the parser expects a token of a certain kind, but it yields a generic
    /// error instead.
    pub(crate) fn expect_error(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token<'cx>> {
        self.expect(kind).map_err(|_| err)
    }

    pub fn parse(src: &'cx str) -> Return<'cx, Ast> {
        let mut parser = Parser::new(crate::syntax::lexer::lex_tokens(src));

        let mut comment_nodes: Vec<(SourceLoc, String)> = Vec::new();
        let mut nodes: Vec<Spanned<ToplevelStmt>> = Vec::new();

        while let Some(token) = parser.peek() {
            match token.kind {
                TokenKind::Comment => {
                    // TODO: Skip comments while still storing them in the AST
                    let _location = token.location.clone();
                    parser.next();

                    // TODO: fix the below code giving me a borrow checker error
                    //comment_nodes.push((location, token.literal.clone().to_string()));
                }
                TokenKind::KwFunction | TokenKind::KwStruct | TokenKind::KwEnum => {
                    // Functions are top-level nodes
                    let location = token.location.clone();
                    nodes.push(spanned(parser.parse_toplevel_stmt()?, location));
                }

                // Statements can exist outside a function or block
                _ => {
                    let location = token.location.clone();
                    nodes.push(spanned(ToplevelStmt::Stmt(parser.parse_stmt()?), location));
                }
            }
        }

        Ok(Ast {
            comments: comment_nodes,
            nodes,
        })
    }
}

impl TokenKind {
    fn is_binop(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Equal
            | TokenKind::DoubleEq
            | TokenKind::BangEq
            | TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::Greater
            | TokenKind::GreaterEq => true,
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
            TokenKind::Star | TokenKind::Slash => 30,
            TokenKind::Plus | TokenKind::Minus => 20,

            // Comparison operators
            TokenKind::DoubleEq
            | TokenKind::BangEq
            | TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::Greater
            | TokenKind::GreaterEq => 15,

            // Compound assignment operators
            TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq => 10,

            // Default precedence for all other tokens
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
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