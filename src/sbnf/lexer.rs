use super::common::{is_identifier_char, str_from_iterators};
use std::str::Chars;

pub struct Grammar<'a> {
    pub source: &'a str,
    pub tokens: Vec<Token<'a>>,
}

impl<'a> std::fmt::Display for Grammar<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Token<'a> {
    Whitespace(&'a str),
    Newline,
    Comment(&'a str),
    Identifier(&'a str),
    Literal(&'a str),
    Regex(&'a str),
    HeaderString(&'a str),
    OptionString(&'a str),
    OpenParenthesis,
    CloseParenthesis,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    Comma,
    Colon,
    SemiColon,
    Pipe,
    Asterisk,
    QuestionMark,
    ExclamationPoint,
    Percent,
    Tilde,
    Equal,
    Invalid,
}

impl<'a> Token<'a> {
    pub fn is_insignificant(&self) -> bool {
        match self {
            Token::Whitespace(_)
            | Token::Comment(_)
            | Token::Invalid => true,
            _ => false,
        }
    }

    pub fn is_whitespace_or_newline(&self) -> bool {
        match self {
            Token::Whitespace(_)
            | Token::Newline => true,
            _ => false,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Token::Whitespace(text)
            | Token::Comment(text)
            | Token::Identifier(text)
            | Token::Literal(text)
            | Token::Regex(text)
            | Token::HeaderString(text)
            | Token::OptionString(text) => text.len(),
            Token::Newline
            | Token::OpenParenthesis
            | Token::CloseParenthesis
            | Token::OpenCurlyBracket
            | Token::CloseCurlyBracket
            | Token::OpenSquareBracket
            | Token::CloseSquareBracket
            | Token::Comma
            | Token::Colon
            | Token::SemiColon
            | Token::Pipe
            | Token::Asterisk
            | Token::QuestionMark
            | Token::ExclamationPoint
            | Token::Percent
            | Token::Tilde
            | Token::Equal => 1,
            Token::Invalid => "INVALID_TOKEN".len(),
        }
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Whitespace(text)
            | Token::Comment(text)
            | Token::Identifier(text)
            | Token::Literal(text)
            | Token::Regex(text)
            | Token::HeaderString(text)
            | Token::OptionString(text) => write!(f, "{}", text),
            Token::Newline => write!(f, "\n"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenCurlyBracket => write!(f, "{{"),
            Token::CloseCurlyBracket => write!(f, "}}"),
            Token::OpenSquareBracket => write!(f, "["),
            Token::CloseSquareBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Pipe => write!(f, "|"),
            Token::Asterisk => write!(f, "*"),
            Token::QuestionMark => write!(f, "?"),
            Token::ExclamationPoint => write!(f, "!"),
            Token::Percent => write!(f, "%"),
            Token::Tilde => write!(f, "~"),
            Token::Equal => write!(f, "="),
            Token::Invalid => write!(f, "INVALID_TOKEN"),
        }
    }
}

struct Lexer<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,

    current: Chars<'a>,
    peeked_char: Option<char>,

    token_start: Chars<'a>,
}

impl<'a> Lexer<'a> {
    fn peek(&mut self) -> Option<char> {
        if let Some(chr) = self.peeked_char {
            Some(chr)
        } else {
            let mut ahead = self.current.clone();
            self.peeked_char = ahead.next();
            self.peeked_char
        }
    }

    fn next(&mut self) -> Option<char> {
        self.peeked_char = None;
        self.current.next()
    }

    fn skip_token(&mut self) -> Option<char> {
        let c = self.next();
        self.token_start = self.current.clone();
        c
    }

    fn collect_token(&mut self) -> &'a str {
        let s =
            str_from_iterators(self.source, &self.token_start, &self.current);

        self.token_start = self.current.clone();

        s
    }
}

// This lexer is separate to the parser. The parser is designed to efficiently
// lex/parse and generate an AST in one go for compilation, whereas this lexer
// does not create an AST nor does it do any error handling, but it does handle
// comments and white space therefore lending itself to other kinds of tooling.
pub fn lex<'a>(source: &'a str) -> Grammar<'a> {
    let mut lexer = Lexer {
        source,
        tokens: vec![],
        current: source.chars(),
        peeked_char: None,
        token_start: source.chars(),
    };

    loop {
        match lexer.peek() {
            Some(' ') | Some('\t') | Some('\n') => {
                lex_white_space(&mut lexer);
            }
            Some('#') => {
                lex_comment(&mut lexer);
            }
            Some(':') => {
                lexer.next();

                loop {
                    match lexer.peek() {
                        Some('\n') | None => break,
                        Some(_) => {
                            lexer.next();
                        }
                    }
                }

                let token = Token::HeaderString(lexer.collect_token());
                lexer.tokens.push(token);
            }
            Some('{') => {
                lexer.skip_token();
                lexer.tokens.push(Token::OpenCurlyBracket);

                // Options change the way `:` is lexed, so we need a separate state
                // for that.
                lex_options(&mut lexer);
            }
            Some('\'') => {
                let mut has_escape = false;

                lexer.next();

                loop {
                    match lexer.next() {
                        Some('\'') if !has_escape => break,
                        None => break,
                        Some('\\') if !has_escape => {
                            has_escape = true;
                        }
                        Some(_) => {
                            has_escape = false;
                        }
                    }
                }

                let token = Token::Regex(lexer.collect_token());
                lexer.tokens.push(token);
            }
            Some('`') => {
                lexer.next();

                loop {
                    match lexer.next() {
                        Some('`') | None => break,
                        Some(_) => {}
                    }
                }

                let token = Token::Literal(lexer.collect_token());
                lexer.tokens.push(token);
            }
            Some(chr) if is_identifier_char(chr) => {
                lexer.next();

                loop {
                    match lexer.peek() {
                        Some(chr) if is_identifier_char(chr) => {
                            lexer.next();
                        }
                        _ => break,
                    }
                }

                let token = Token::Identifier(lexer.collect_token());
                lexer.tokens.push(token);
            }
            Some(_) => {
                let token_type = match lexer.skip_token().unwrap() {
                    '(' => Token::OpenParenthesis,
                    ')' => Token::CloseParenthesis,
                    '[' => Token::OpenSquareBracket,
                    ']' => Token::CloseSquareBracket,
                    ',' => Token::Comma,
                    ';' => Token::SemiColon,
                    '|' => Token::Pipe,
                    '*' => Token::Asterisk,
                    '?' => Token::QuestionMark,
                    '!' => Token::ExclamationPoint,
                    '%' => Token::Percent,
                    '~' => Token::Tilde,
                    '=' => Token::Equal,
                    _ => Token::Invalid,
                };
                lexer.tokens.push(token_type);
            }
            None => break,
        }
    }

    Grammar { source, tokens: lexer.tokens }
}

fn lex_white_space<'a>(lexer: &mut Lexer<'a>) {
    if lexer.next() == Some('\n') {
        lexer.collect_token();
        let token = Token::Newline;
        lexer.tokens.push(token);
        return;
    }

    loop {
        match lexer.peek() {
            Some(' ') | Some('\t') => {
                lexer.next();
            }
            _ => break,
        }
    }

    let token = Token::Whitespace(lexer.collect_token());
    lexer.tokens.push(token);
}

fn lex_comment<'a>(lexer: &mut Lexer<'a>) {
    lexer.next();

    loop {
        match lexer.peek() {
            Some('\n') | None => break,
            _ => {
                lexer.next();
            }
        }
    }

    let token = Token::Comment(lexer.collect_token());
    lexer.tokens.push(token);
}

fn lex_options<'a, 'b>(lexer: &'b mut Lexer<'a>) {
    loop {
        match lexer.peek() {
            Some(' ') | Some('\t') | Some('\n') => {
                lex_white_space(lexer);
            }
            Some(',') => {
                lexer.skip_token();
                lexer.tokens.push(Token::Comma);
            }
            Some('}') => {
                lexer.skip_token();
                lexer.tokens.push(Token::CloseCurlyBracket);
                break;
            }
            Some(_) => {
                loop {
                    match lexer.peek() {
                        Some(' ') | Some('\t') | Some('\n') | Some(',')
                        | Some('}') | None => break,
                        Some(_) => {
                            lexer.next();
                        }
                    }
                }

                let token = Token::OptionString(lexer.collect_token());
                lexer.tokens.push(token);
            }
            None => break,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_lexer(source: &str, num_tokens: usize) {
        let grammar = lex(source);
        assert!(grammar.tokens.len() == num_tokens);
        assert!(format!("{}", grammar) == source);
    }

    #[test]
    fn lexer() {
        check_lexer("", 0);
        check_lexer(" ", 1);
        check_lexer("\t", 1);
        check_lexer(" \n\t", 3);
        check_lexer("a:b", 2);
        check_lexer("name: foo", 2);
        check_lexer("# foo\n", 2);
        check_lexer(" name\n :foo \n", 6);
        check_lexer("a = 'foo\\'' | `bar`*\n  ; bar = ( ''{foo ,bar}[asd]", 30);
        check_lexer("a\n= 'foo'\n  # bar\n  'bar'\n;\n", 14);
    }
}
