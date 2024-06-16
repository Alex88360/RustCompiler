extern crate thiserror;
extern crate readonly;

use core::panic;
use std::iter::*;
use std::str::*;
use std::fmt;
use std::fmt::*;
use std::result::Result;
use thiserror::Error;
use std::collections::*;



#[derive(PartialEq, Debug, Error)]
pub enum LexerError {
    // #[error("")]
    // FileIO(#[from] io::Error),

    #[error("Was expecting {expected:?}, found {found:?}")]
    MissingExpectedSymbol {
        expected: &'static str,
        found: TokenKind
    },

    #[error("missing open {open:?} for this delimiter {symbol:?}")]
    MisBalancedSymbol {
        symbol: char,
        open: char
    },

    #[error("Find an invalid character {raw:?} for a numeric literal")]
    NumericLiteralInvalidChar {
        raw: String,
        invalid: char
    },

    #[error("")]
    UnknownSymbol {
        symbol: char
    }
}
 
// pub type Token = TokenType;
pub type BalancingDepthType = i32;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    None,
    EOF,
    Punctuation(PunctuationKind),
    Operator(OperatorKind),
    Identifier,
    Num(NumericHint),
    Str,
    Return,
    Function,
    If,
    Else,
    While,
    For,
    True,
    False,
    // types annotations
    Boolean,
    String,
    Number,
    Void
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self { 
                TokenKind::Operator(OperatorKind::Plus) => "+",
                TokenKind::Operator(OperatorKind::Minus) => "-",
                TokenKind::Operator(OperatorKind::Equals) => "=",
                TokenKind::Operator(OperatorKind::ForwardSlash) => "/",
                TokenKind::Operator(OperatorKind::Asterisk) => "*",
                TokenKind::Operator(OperatorKind::Modulo) => "%",
                TokenKind::Operator(OperatorKind::Bang) => "!",
                TokenKind::Operator(OperatorKind::PlusEquals) => "+=",
                TokenKind::Operator(OperatorKind::MinusEquals) => "-=",
                TokenKind::Operator(OperatorKind::DoubleEquals) => "==",
                TokenKind::Operator(OperatorKind::LogicalAnd) => "ET",
                TokenKind::Operator(OperatorKind::LogicalOr) => "OU",
                TokenKind::Operator(OperatorKind::GreaterThan) => ">",
                TokenKind::Operator(OperatorKind::GreaterThanOrEqual) => ">=",
                TokenKind::Operator(OperatorKind::LessThan) => "<", 
                TokenKind::Operator(OperatorKind::LessThanOrEqual) => "<=",
                TokenKind::Punctuation(PunctuationKind::Comma) => ",",
                TokenKind::Punctuation(PunctuationKind::SemiColon) => ";",
                TokenKind::Punctuation(PunctuationKind::Colon) => ":",
                TokenKind::Punctuation(PunctuationKind::LParenthesize) => "(",
                TokenKind::Punctuation(PunctuationKind::RParenthesize) => ")",
                TokenKind::Punctuation(PunctuationKind::LCurlyBracket) => "{",
                TokenKind::Punctuation(PunctuationKind::RCurlyBracket) => "}",
                TokenKind::Identifier => "identifier",
                TokenKind::Function => "fonction",
                TokenKind::True => "true",
                TokenKind::False => "false",
                TokenKind::Number => "entier",
                TokenKind::Boolean => "bool",
                TokenKind::Void => "void",
                TokenKind::String => "string",
                TokenKind::If => "si",
                TokenKind::Else => "sinon",
                TokenKind::While => "tanque",
                TokenKind::Return => "retourner",
                TokenKind::EOF => "eof",
                _ => unreachable!()
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token {
    pub span: Option<Span>,
    pub kind: TokenKind
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self {
            span: Some(span),
            kind
        }
    }
    pub fn len(&self) -> usize {
        match self.span.clone() {
            Some(span) => (span.end - span.start) as usize,
            None => 0
        }
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        match self.span.clone() {
            Some(span) =>{
                &input[span.start..span.end]
            },
            None => ""
        }
    }
}

impl Default for TokenKind {
    fn default() -> Self {
        Self::None
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumericHint {
    FloatingPoint,
    Integer
}

impl Display for NumericHint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self { 
                NumericHint::FloatingPoint => "Float",
                NumericHint::Integer => "Integer",
                _ => unimplemented!()
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PunctuationKind {
    LSquareBracket,
    RSquareBracket,
    LCurlyBracket,
    RCurlyBracket,
    LParenthesize,
    RParenthesize,
    Colon,
    Comma,
    SemiColon
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OperatorKind {
    Plus,
    DoublePlus,
    PlusEquals,
    Minus,
    DoubleMinus,
    MinusEquals,
    Asterisk,
    ForwardSlash,
    Equals,
    DoubleEquals,
    Bang,
    BangEquals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Modulo,
    LogicalAnd,
    LogicalOr
}


#[readonly::make]
pub struct Lexer<'a> {
    #[readonly]
    pub curr_line: usize,
    #[readonly]
    pub curr_col: usize,
    #[readonly]
    pub codepoint_offset: usize,
    chars: Peekable<Chars<'a>>,
    balancing_state: HashMap<char, BalancingDepthType>
}

pub struct TokenIter<'a> {
    lexer: Lexer<'a>
}

impl <'a> TokenIter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input)
        }
    }
}

impl <'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next_token().ok()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &'a str) -> Lexer<'a> {
        Lexer {
            curr_col: 1,
            curr_line: 1,
            codepoint_offset: 0,
            chars: chars.chars().peekable(),
            balancing_state: HashMap::new()
        }
    }

    fn push_open(&mut self, c: &char) -> BalancingDepthType {
        if let Some(v) = self.balancing_state.get_mut(&c) {
            *v += 1;
            *v - 1
        } else {
            self.balancing_state.insert(*c, 1);
            0
        }
    }

    fn pop_close(&mut self, c: &char) -> Result<BalancingDepthType, LexerError> {
        if let Some(v) = self.balancing_state.get_mut(&Lexer::map_balance(c)) {
            if *v >= 1 {
                *v -= 1;
                Ok(*v)
            } else {
                Err(LexerError::MisBalancedSymbol { symbol: *c, open: Lexer::map_balance(c) })
            }
        } else {
            Err(LexerError::MisBalancedSymbol { symbol: *c, open: Lexer::map_balance(c) })
        }
    }

    fn map_balance(c: &char) -> char {
        match c {
            '}' => '{',
            '{' => '}',
            ')' => '(',
            '(' => ')',
            ']' => '[',
            '[' => ']',
            _ => panic!("Misuse of map_balance with a character that is not a valid balancing character")
        }
    }

    fn build_span(&self, start: usize, end: usize) -> Option<Span> {
        Some(
            Span {
                start,
                end
            }
        )
    }



    fn transform_to_token(&mut self, c: char, start_offset: usize) -> Result<Token, LexerError> {
        match c {
            '(' => {
                self.push_open(&c);
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Punctuation(
                            PunctuationKind::LParenthesize 
                        )
                    }
                )
            },
            ')' => {
                self.pop_close(&c)?;
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Punctuation(
                            PunctuationKind::RParenthesize
                        )
                    }
                )
            },
            '{' => {
                self.push_open(&c);
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Punctuation(
                            PunctuationKind::LCurlyBracket 
                        )
                    }
                )
            },
            '}' => {
                self.pop_close(&c)?;
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Punctuation(
                            PunctuationKind::RCurlyBracket 
                        )
                    }
                )
            },
            '[' => {
                self.push_open(&c);
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Punctuation(
                            PunctuationKind::LSquareBracket 
                        )
                    }
                )
            },
            ']' => {
                self.pop_close(&c)?;
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Punctuation(
                            PunctuationKind::RSquareBracket 
                        )
                    }
                )
            },
            ',' => Ok(
                Token {
                    span: self.build_span(start_offset, self.codepoint_offset + 1),
                    kind: TokenKind::Punctuation(PunctuationKind::Comma)
                }
            ),
            ':' => Ok(
                Token {
                    span: self.build_span(start_offset, self.codepoint_offset + 1),
                    kind: TokenKind::Punctuation(PunctuationKind::Colon)
                }
            ),
            ';' => Ok(
                Token {
                    span: self.build_span(start_offset, self.codepoint_offset + 1),
                    kind: TokenKind::Punctuation(PunctuationKind::SemiColon)
                }
            ),
            '=' | '!' | '+' | '-' | '*' | '/' | '>' | '<' | '%' | 'E' | 'O' => self.parse_operator(c, &['=', '!', '+', '-', 'T', 'U'], start_offset),
            '0'..='9' | '.' => self.parse_number(c, start_offset),
            '"' => self.parse_string(start_offset),
            'a'..='z' | 'A'..='Z' | '_' => self.parse_ident(c, start_offset),
            _ => Err(LexerError::UnknownSymbol { symbol: c })
        }
    }

    fn match_chars(&self, c: char, kinds: &[char]) -> Option<char> {
        for kind in kinds {
            if c == *kind {
                return Some(c);
            }
        }
        None
    }

    fn parse_operator(&mut self, c: char, operators: &[char], start_offset: usize) -> Result<Token, LexerError> {
        if let Some(next) = self.chars.peek() {
            let nxt = *next;
            if let Some(m) = self.match_chars(nxt, operators) {
                self.consume_char();
                Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset),
                        kind: TokenKind::Operator(self.map_operator(format!("{}{}", c, m)))
                    }
                )
            } else {
                Ok(
                    Token {
                        span: self.build_span(self.codepoint_offset, self.codepoint_offset + 1),
                        kind: TokenKind::Operator(self.map_operator(c.to_string()))
                    }
                )
            }
        } else {
            Ok(Token { span: None, kind: TokenKind::EOF })
        }
    }

    fn map_operator(&self, op: String) -> OperatorKind {
        match op.as_ref() {
            "=" => OperatorKind::Equals,
            "+" => OperatorKind::Plus,
            "-" => OperatorKind::Minus,
            "*" => OperatorKind::Asterisk,
            "/" => OperatorKind::ForwardSlash,
            "!" => OperatorKind::Bang,
            "%" => OperatorKind::Modulo,
            "==" => OperatorKind::DoubleEquals,
            "!=" => OperatorKind::BangEquals,
            "--" => OperatorKind::DoubleMinus,
            "++" => OperatorKind::DoublePlus,
            "+=" => OperatorKind::PlusEquals,
            "-=" => OperatorKind::MinusEquals,
            ">" => OperatorKind::GreaterThan,
            ">=" => OperatorKind::GreaterThanOrEqual,
            "<" => OperatorKind::LessThan,
            "<=" => OperatorKind::LessThanOrEqual,
            "ET" => OperatorKind::LogicalAnd,
            "OU" => OperatorKind::LogicalOr,
            _ => panic!("You provide a wrong operator")
        }
    }

    fn parse_string(&mut self, start_offset: usize) -> Result<Token, LexerError> {
        let mut raw = String::new();

        loop {
            match self.chars.next() {
                Some('"') => break Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset),
                        kind: TokenKind::Str
                    }
                ),
                Some(c) => raw.push(c),
                None => break Err(LexerError::MissingExpectedSymbol { expected: "\"", found: TokenKind::EOF })
            }
        }
    }

    fn parse_ident(&mut self, start: char, start_offset: usize) -> Result<Token, LexerError> {
        let mut raw = start.to_string();

        loop {          
            match self.chars.peek() {
                Some(c) if c.is_alphabetic() || c.is_digit(10) || *c == '_' => raw.push(self.consume_char().unwrap()),
                _ => break Ok(
                    Token {
                        span: self.build_span(start_offset, self.codepoint_offset),
                        kind: self.tag_ident(raw)
                    }
                )
            }
        }
    }

    fn tag_ident(&self, ident: String) -> TokenKind {
        match ident.as_ref() {
            "fonction" => TokenKind::Function,
            "retourner" => TokenKind::Return,
            "tantque" => TokenKind::While,
            "si" => TokenKind::If,
            "sinon" => TokenKind::Else,
            "pour" => TokenKind::For,
            "entier" => TokenKind::Number,
            "bool" => TokenKind::Boolean,
            "string" => TokenKind::String,
            "rien" => TokenKind::Void,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier
        }
    }


    fn parse_digits(&mut self, radix: u32, allow_empty: bool) -> Result<String, LexerError> {
         let mut raw = String::new();

         loop {
            match self.chars.peek() {
                None => {
                    if allow_empty || raw.len() > 0 {
                        break Ok(raw);
                    } else {
                        break Err(LexerError::MissingExpectedSymbol { 
                            expected: "0-9<digit>",
                            found: TokenKind::EOF 
                        });
                    }
                },
                Some(c) if *c == 'e' || *c == 'E' => {
                    if !allow_empty && raw.len() == 0 {
                        break Err(LexerError::NumericLiteralInvalidChar { raw, invalid: *c })
                    }
                    break Ok(raw)
                },
                Some(c) if c.is_digit(radix) || (*c == '_' && raw.len() > 0) => {
                    raw.push(*c);
                    self.consume_char();
                },
                Some(c) if !c.is_ascii_alphabetic() && *c != '_' => break Ok(raw),
                Some(c) => {
                    break Err(LexerError::NumericLiteralInvalidChar { raw, invalid: *c })
                }
            }
         }
    }


    fn parse_number(&mut self, start: char, start_offset: usize) -> Result<Token, LexerError> {
        let mut raw = start.to_string();
        let radix = 10;
        let mut hint = NumericHint::Integer;

        if start == '.' {
            raw += &self.parse_digits(radix, false)?;
            hint = NumericHint::FloatingPoint;
        } else if start.is_digit(radix) {
            raw += &self.parse_digits( radix, true)?;

            if let Some(c) = try_consume_char!(self, '.') {
                raw.push(c);
                raw += &self.parse_digits(radix, false)?;
                hint = NumericHint::FloatingPoint;
            }
        } else {
            return Err(LexerError::NumericLiteralInvalidChar { 
                raw, 
                invalid: start 
            })
        }

        if let Some(c) = try_consume_char!(self, 'e', 'E') {
            raw.push(c);
            hint = NumericHint::FloatingPoint;

            if let Some(c) = try_consume_char!(self, '-', '+') {
                raw.push(c);
            }

            raw += &self.parse_digits(radix, false)?;
        }
        Ok(
            Token {
                span: self.build_span(start_offset, self.codepoint_offset),
                kind: TokenKind::Num(hint)
            }
        )
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        let start_offset = self.codepoint_offset;

        if let Some(c) = self.consume_char() {
            self.transform_to_token(c, start_offset)
        } else {
            Ok(Token { span: None, kind: TokenKind::EOF })
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.next_if(|c| c.is_ascii_whitespace()) {
            self.codepoint_offset += 1;

            match c {
                '\n' => { self.curr_col += 1; self.curr_line += 1},
                _ => self.curr_col += 1
            }
        }
    }

    fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.curr_col += 1;
                self.codepoint_offset += 1;
                Some(c)
            },
            None => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        Lexer, 
        TokenKind,
        NumericHint, 
        OperatorKind
    };

    #[test]
    fn parse_number_with_dot() {
        let mut lexer = Lexer::new(".2 2.2");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Num(NumericHint::FloatingPoint));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Num(NumericHint::FloatingPoint));
    }

    #[test]
    fn parse_number_with_exp_and_dot() {
        let mut lexer = Lexer::new("2.2e2 .2e2 2.e2");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Num(NumericHint::FloatingPoint));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Num(NumericHint::FloatingPoint));
        // assert_eq!(lexer.next_token().unwrap_err(), LexerError::NumericLiteralInvalidChar { raw: "".to_string(), invalid: 'e' });
    }

    #[test]
    fn parse_number_without_exp_and_dot() {
        let mut lexer = Lexer::new("125 234a3");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Num(NumericHint::Integer));
        // assert_eq!(lexer.next_token().unwrap_err(), LexerError::NumericLiteralInvalidChar { raw: "234a3".to_string(), invalid: 'a' })
    }

    #[test]
    fn parse_number_with_exp_and_without_dot() {
        let mut lexer = Lexer::new("2e2 2e");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Num(NumericHint::FloatingPoint));
        // assert_eq!(lexer.next_token().unwrap_err(), LexerError::NumericLiteralInvalidChar { raw: "2e".to_string(), invalid: 'e' })
    }

    #[test]
    fn parse_string() {
        let mut lexer = Lexer::new("\"Hello World\"");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Str);
    }

    #[test]
    fn parse_variable_name_ident() {
        let mut lexer = Lexer::new("test _test test644 call(a, b)");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Identifier);
    }

    #[test]
    fn parse_reserved_keyword_ident() {
        let mut lexer = Lexer::new("si sinon si tantque fonction retourner");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::If);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Else);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::If);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::While);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Function);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Return);
    }

    #[test]
    fn parse_operators() {
        let mut lexer = Lexer::new("+ - = += -= ==");

        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Operator(OperatorKind::Plus));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Operator(OperatorKind::Minus));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Operator(OperatorKind::Equals));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Operator(OperatorKind::PlusEquals));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Operator(OperatorKind::MinusEquals));
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Operator(OperatorKind::DoubleEquals));
    }
}