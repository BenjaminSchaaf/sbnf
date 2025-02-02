use crate::{TextLocation, util::str_from_iterators};
use super::common::{LookaheadChars, is_identifier_char, is_whitespace_char};
use std::str::Chars;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Whitespace,
    LineComment,
    Identifier,
    LiteralTerminal,
    RegexTerminal,
    OptionString,
    Asterisk,
    Tilde,
    QuestionMark,
    Pipe,
    Equals,
    Colon,
    SemiColon,
    Comma,
    Percent,
    OpenParenthesis,
    CloseParenthesis,
    OpenSquareBracket,
    CloseSquareBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    Error,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub value: &'a str,
    pub type_: TokenType,
    pub location: TextLocation,
}

pub struct Tokenizer<'a> {
    source: &'a str,
    iterator: LookaheadChars<'a>,

    // Tokenization state. Only options currently change how tokenization works.
    in_options: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Tokenizer {
            source,
            iterator: LookaheadChars::new(source),
            in_options: false,
        }
    }

    fn make_token(&self, start: Chars<'a>, type_: TokenType, location: TextLocation) -> Token<'a> {
        Token {
            value: str_from_iterators(self.source, start, self.iterator.current.clone()),
            type_,
            location,
        }
    }

    fn next_in_options(&mut self) -> Option<Token<'a>> {
        let (start_str, location) = self.iterator.start_point();

        Some(match self.iterator.next()? {
            '}' => {
                self.in_options = false;

                self.make_token(start_str, TokenType::CloseCurlyBracket, location)
            }
            ',' => self.make_token(start_str, TokenType::Comma, location),
            ':' => self.make_token(start_str, TokenType::Colon, location),
            _ => {
                while let Some(c) = self.iterator.peek() {
                    match c {
                        '}' | ',' | ':' => break,
                        _ => self.iterator.next().unwrap(),
                    };
                }

                self.make_token(start_str, TokenType::OptionString, location)
            }
        })
    }

    fn next_general(&mut self) -> Option<Token<'a>> {
        let (start_str, location) = self.iterator.start_point();

        Some(match self.iterator.next()? {
            '#' => {
                while self.iterator.next().unwrap_or('\n') != '\n' {}
                self.make_token(start_str, TokenType::LineComment, location)
            }
            '\'' => {
                let mut has_escape = false;
                while let Some(c) = self.iterator.next() {
                    if has_escape {
                        has_escape = false;
                    } else if c == '\'' {
                        break;
                    } else if c == '\\' {
                        has_escape = true;
                    }
                }
                self.make_token(start_str, TokenType::RegexTerminal, location)
            }
            '`' => {
                while self.iterator.next().unwrap_or('`') != '`' {}
                self.make_token(start_str, TokenType::LiteralTerminal, location)
            }
            '*' => self.make_token(start_str, TokenType::Asterisk, location),
            '~' => self.make_token(start_str, TokenType::Tilde, location),
            '?' => self.make_token(start_str, TokenType::QuestionMark, location),
            '|' => self.make_token(start_str, TokenType::Pipe, location),
            '=' => self.make_token(start_str, TokenType::Equals, location),
            ':' => self.make_token(start_str, TokenType::Colon, location),
            ';' => self.make_token(start_str, TokenType::SemiColon, location),
            ',' => self.make_token(start_str, TokenType::Comma, location),
            '%' => self.make_token(start_str, TokenType::Percent, location),
            '(' => self.make_token(start_str, TokenType::OpenParenthesis, location),
            ')' => self.make_token(start_str, TokenType::CloseParenthesis, location),
            '[' => self.make_token(start_str, TokenType::OpenSquareBracket, location),
            ']' => self.make_token(start_str, TokenType::CloseSquareBracket, location),
            '{' => {
                self.in_options = true;
                self.make_token(start_str, TokenType::OpenCurlyBracket, location)
            }
            '}' => self.make_token(start_str, TokenType::CloseCurlyBracket, location),
            c if is_identifier_char(c) => {
                while let Some(c) = self.iterator.peek() {
                    if !is_identifier_char(c) {
                        break;
                    }
                    self.iterator.next();
                }
                self.make_token(start_str, TokenType::Identifier, location)
            }
            c if is_whitespace_char(c) => {
                while let Some(c) = self.iterator.peek() {
                    if !is_whitespace_char(c) {
                        break;
                    }
                    self.iterator.next();
                }
                self.make_token(start_str, TokenType::Whitespace, location)
            }
            _ => self.make_token(start_str, TokenType::Error, location),
        })
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.in_options {
            self.next_in_options()
        } else {
            self.next_general()
        }
    }
}
