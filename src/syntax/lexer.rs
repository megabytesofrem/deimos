//! Lexical analysis pass using `logos` crate
use core::fmt;
use std::iter::Peekable;

use logos::{Logos, SpannedIter};
use serde::{Deserialize, Serialize};

/// TODO:
/// instead of storing the line number and column, we can store the start/end
/// position only and then calculate the line/column when needed.  You'll need
/// the original input string to do this.
///
/// We don't actually need the line number and column info of each node, we only
/// need to calculate it when reporting an error for those couple of positions
/// we care about.

/// Report locations in the source code
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
pub struct SourceLoc {
    pub line: usize,

    pub start: usize,
    pub end: usize,
}

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
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token("&")]
    Ampersand,
    #[token("=")]
    Equal,
    #[token("!")]
    Bang,
    #[token("==")]
    DoubleEq,
    #[token("!=")]
    BangEq,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,

    // Keywords
    #[token("public")]
    KwPublic,
    #[token("let")]
    KwLet,
    #[token("struct")]
    KwStruct,
    #[token("enum")]
    KwEnum,
    #[token("function")]
    KwFunction,
    #[token("extern")]
    KwExtern,
    #[token("import")]
    KwImport,
    #[token("cast")]
    KwCast,
    #[token("if")]
    KwIf,
    #[token("then")]
    KwThen,
    #[token("elif")]
    KwElif,
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
    #[token("return")]
    KwReturn,
    #[token("and")]
    KwAnd,
    #[token("or")]
    KwOr,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    // Literals
    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*")]
    Name,
    #[regex(r"[-]?[0-9][0-9]*")]
    Integer,
    #[regex(r"[-]?[0-9]+\.[0-9]+")]
    Float,
    #[regex(r"0[xX][0-9a-fA-F]+")]
    HexInteger,
    #[regex(r#""(\\[\\"]|[^"])*""#)]
    StringLit,

    #[regex(r"--.*\n?")]
    Comment,

    // We ignore whitespace in the lexer
    #[regex(r"[ \t\f]+", logos::skip, priority = 2)]
    Whitespace,

    // We're skipping newlines because, the enum parser (and probably other parts
    // of the parser) don't know how to deal with newlines yet.
    #[regex(r"[\r\n]+", logos::skip)]
    NewLine,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Op {
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

    // Unary operators
    Neg,
    Bang
}

impl Op {
    pub fn to_str(&self) -> &str {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Mod => "%",
            Op::And => "and",
            Op::Or => "or",
            Op::Eq => "==",
            Op::BangEq => "!=",
	    Op::Less => "<",
            Op::LessEq => "<=",
            Op::Greater => ">",
            Op::GreaterEq => ">=",

	    Op::Neg => "-",
	    Op::Bang => "!"
        }
    }
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

    pub fn to_int_literal(&self) -> i32 {
        match self.kind {
            TokenKind::Integer => self.literal.parse().unwrap(),
            TokenKind::HexInteger => i32::from_str_radix(&self.literal[2..], 16).unwrap(),
            _ => panic!("Not an integer literal"),
        }
    }
}

// Alias type for TokenIter to be more typing-friendly
pub type LexerIter<'a> = Peekable<Box<TokenIter<'a>>>;

#[derive(Clone)]
pub struct TokenIter<'a> {
    inner: SpannedIter<'a, TokenKind>,
    src: &'a str,
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(L{}, C{}:{})", self.line, self.start, self.end)
    }
}

impl<'a> TokenIter<'a> {
    // Return the correct location of the token accounting for newlines by using the
    // inner span and directly counting the characters in the source string.
    fn get_location(&self) -> SourceLoc {
        let span = self.inner.span();
        let start = span.start;
        let end = span.end;

        let mut line = 1;
        let mut col = 0;

        for (i, c) in self.src.chars().enumerate() {
            if i == start {
                break;
            }

            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        SourceLoc {
            line,
            start: col,
            end: col + (end - start),
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(kind, span)| {
                Some(Token {
                    kind: kind.ok()?,
                    location: self.get_location(),
                    literal: &self.src[span],
                })
            })
            .flatten()
    }
}

/// Return an iterator over the tokens in the source string
pub fn lex_tokens(src: &str) -> LexerIter {
    let iter = TokenIter {
        inner: TokenKind::lexer(src).spanned(),
        src,
    };

    Box::new(iter).peekable()
}
