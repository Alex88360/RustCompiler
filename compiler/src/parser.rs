use crate::lexer::{Token, TokenKind, TokenIter};
use std::iter::Peekable;
use thiserror::Error;

#[derive(PartialEq, Debug, Error)]
pub enum ParserError {
    #[error("Unable to recognize this type")]
    UnrecognizedType,

    #[error("Invalid literal")]
    InvalidLiteral,

    #[error("Invalid type annotation")]
    InvalidTypeAnnotation,

    #[error("any")]
    Any
}

pub struct Parser<'a, I> where I: Iterator<Item=Token> {
    input: &'a str,
    tokens: Peekable<I>
}

impl <'a> Parser<'a, TokenIter<'a>> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            tokens: TokenIter::new(input).peekable()
        }
    }
}

impl <'a, I> Parser<'a, I> where I: Iterator<Item=Token> {
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens.peek().map(|token| token.kind).unwrap_or(TokenKind::EOF)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub(crate) fn try_consume(&mut self, expected: TokenKind) -> Token {
        let token = self.next().expect(&format!("Expect token {} but found not token", expected));
        assert_eq!(token.kind, expected);
        token
    }

    pub(crate) fn text(&self, token: Token) -> &'a str {
        token.text(self.input)
    }
} 