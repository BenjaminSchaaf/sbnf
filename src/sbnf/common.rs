use crate::TextLocation;
use std::str::Chars;

#[derive(Debug)]
pub struct ParseError {
    location: TextLocation,
    message: String,
}

impl ParseError {
    pub fn new(location: TextLocation, message: String) -> ParseError {
        ParseError { location, message }
    }

    pub fn with_source<'a>(
        &'a self,
        origin: &'a str,
        source: &'a str,
    ) -> ParseErrorWithSource<'a> {
        ParseErrorWithSource { error: self, origin, source }
    }
}

pub struct ParseErrorWithSource<'a> {
    error: &'a ParseError,
    origin: &'a str,
    source: &'a str,
}

impl std::fmt::Display for ParseErrorWithSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parser Error: {} ({}:{})\n{}",
            self.error.message,
            self.origin,
            self.error.location,
            self.error.location.with_source(self.source)
        )
    }
}

// Iterator-like Chars with lookahead and location tracking. Peekable<Chars>
// doesn't provide the required insight into the underlying iterator.
pub struct LookaheadChars<'a> {
    pub location: TextLocation,
    pub current: Chars<'a>,
    pub peek: Option<(char, Chars<'a>)>,
}

impl<'a> LookaheadChars<'a> {
    pub fn new(source: &'a str) -> LookaheadChars<'a> {
        LookaheadChars {
            location: TextLocation::INITIAL,
            current: source.chars(),
            peek: None,
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        if let Some((chr, _)) = &self.peek {
            Some(*chr)
        } else {
            let mut ahead = self.current.clone();
            let chr = ahead.next()?;
            self.peek = Some((chr, ahead));
            Some(chr)
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let chr = if let Some((chr, iter)) = self.peek.take() {
            self.current = iter;
            chr
        } else {
            self.current.next()?
        };

        self.location.increment(chr);
        Some(chr)
    }

    pub fn start_point(&self) -> (Chars<'a>, TextLocation) {
        (self.current.clone(), self.location)
    }
}

pub fn is_identifier_char(chr: char) -> bool {
    chr.is_alphanumeric() || chr == '_' || chr == '-' || chr == '.'
}

pub fn is_whitespace_char(chr: char) -> bool {
    chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r'
}
