use logos::{Logos, SpannedIter};
use std::iter::Peekable;

pub type SourceLoc = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Logos)]
pub enum TokenKind {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token("!")]
    Bang,
    #[token("==")]
    DoubleEqual,
    #[token("!=")]
    BangEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,

    // Data types
    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("bool")]
    Bool,
    #[token("string")]
    String,
    #[token("void")]
    Void,

    // Keywords
    #[token("local")]
    KwLocal,
    #[token("struct")]
    KwStruct,
    #[token("function")]
    KwFunction,
    #[token("cimport")]
    KwCImport,
    #[token("import")]
    KwImport,
    #[token("as")]
    KwAs,
    #[token("if")]
    KwIf,
    #[token("then")]
    KwThen,
    #[token("else")]
    KwElse,
    #[token("for")]
    KwFor,
    #[token("while")]
    KwWhile,
    #[token("do")]
    KwDo,
    #[token("end")]
    KwEnd,
    #[token("break")]
    KwBreak,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,
    #[token("return")]
    KwReturn,

    // Literals
    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*")]
    Ident,
    #[regex(r"[-]?[0-9][0-9]*")]
    Integer,
    #[regex(r"0[xX][0-9a-fA-F]+")]
    HexInteger,
    #[regex(r#""(\\[\\"]|[^"])*""#)]
    StringLit,

    #[regex(r"--.*\n?")]
    Comment,

    // We ignore whitespace in the lexer
    #[regex(r"[ \r\t\n\f]+", logos::skip)]
    Whitespace,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    BangEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Neg,
    Bang,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub location: SourceLoc,
    pub literal: &'a str,
}

impl Token<'_> {
    pub fn is_int_literal(&self) -> bool {
        matches!(self.kind, TokenKind::Integer | TokenKind::HexInteger)
    }

    pub fn to_int_literal(&self) -> i64 {
        match self.kind {
            TokenKind::Integer => self.literal.parse().unwrap(),
            TokenKind::HexInteger => i64::from_str_radix(&self.literal[2..], 16).unwrap(),
            _ => panic!("Not an integer literal"),
        }
    }
}

// Alias type for TokenIter to be more typing-friendly
pub type LexerIter<'a> = Peekable<Box<TokenIter<'a>>>;

pub struct TokenIter<'a> {
    inner: SpannedIter<'a, TokenKind>,
    src: &'a str,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(kind, span)| {
                Some(Token {
                    kind: kind.ok()?,
                    location: span.start..span.end,
                    literal: &self.src[span],
                })
            })
            .flatten()
    }
}

/// Return an iterator over the tokens in the source string
pub fn lex_tokens<'a>(src: &'a str) -> LexerIter<'a> {
    let iter = TokenIter {
        inner: TokenKind::lexer(src).spanned(),
        src,
    };

    Box::new(iter).peekable()
}
