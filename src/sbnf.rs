/// This file implements a parser for the SBNF grammar
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug)]
pub struct Grammar<'a> {
    pub source: &'a str,
    pub nodes: Vec<Node<'a>>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct Node<'a> {
    pub text: &'a str,
    pub location: TextLocation,
    pub data: NodeData<'a>,
}

impl<'a> Node<'a> {
    pub fn new(text: &'a str, location: TextLocation, data: NodeData<'a>) -> Node<'a> {
        Node { text: text, location: location, data: data }
    }

    pub fn get_regex(&self) -> &'a str {
        match &self.data {
            NodeData::RegexTerminal { .. } => self.text,
            NodeData::LiteralTerminal { .. } => panic!(),
            _ => panic!(),
        }
    }

    pub fn get_arguments(&'a self) -> &'a Vec<Node<'a>> {
        match &self.data {
            NodeData::RegexTerminal { arguments }
            | NodeData::LiteralTerminal { arguments } => arguments,
            _ => panic!(),
        }
    }

    fn fmt_inner(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.data {
            NodeData::Header(value) => {
                write!(f, "{}: ", self.text)?;
                value.fmt_inner(f)
            },
            NodeData::HeaderValue
            | NodeData::Variable
            | NodeData::PositionalArgument
            | NodeData::KeywordArgumentValue => {
                write!(f, "{}", self.text)
            },
            NodeData::Rule { arguments, node } => {
                write!(f, "{}", self.text)?;
                if !arguments.is_empty() {
                    write!(f, "{{")?;
                    arguments[0].fmt_inner(f)?;
                    for arg in &arguments[1..] {
                        write!(f, ", ")?;
                        arg.fmt_inner(f)?;
                    }
                    write!(f, "}}")?;
                }
                write!(f, " ")?;
                node.fmt_inner(f)?;
                write!(f, " ;")
            },
            NodeData::RegexTerminal { arguments } => {
                write!(f, "'{}'", self.text)?;
                if !arguments.is_empty() {
                    write!(f, "{{")?;
                    arguments[0].fmt_inner(f)?;
                    for arg in &arguments[1..] {
                        write!(f, ", ")?;
                        arg.fmt_inner(f)?;
                    }
                    write!(f, "}}")?;
                }
                Ok(())
            },
            NodeData::LiteralTerminal { arguments } => {
                write!(f, "`{}`", self.text)?;
                if !arguments.is_empty() {
                    write!(f, "{{")?;
                    arguments[0].fmt_inner(f)?;
                    for arg in &arguments[1..] {
                        write!(f, ", ")?;
                        arg.fmt_inner(f)?;
                    }
                    write!(f, "}}")?;
                }
                Ok(())
            },
            NodeData::Embed { arguments } => {
                write!(f, "%")?;
                if !arguments.is_empty() {
                    write!(f, "{{")?;
                    arguments[0].fmt_inner(f)?;
                    for arg in &arguments[1..] {
                        write!(f, ", ")?;
                        arg.fmt_inner(f)?;
                    }
                    write!(f, "}}")?;
                }
                Ok(())
            },
            NodeData::Passive(node) => {
                write!(f, "~(")?;
                node.fmt_inner(f)?;
                write!(f, ")")
            },
            NodeData::Repetition(node) => {
                write!(f, "(")?;
                node.fmt_inner(f)?;
                write!(f, ")*")
            },
            NodeData::Optional(node) => {
                write!(f, "(")?;
                node.fmt_inner(f)?;
                write!(f, ")?")
            },
            NodeData::Alternation(nodes) => {
                write!(f, "(")?;
                nodes[0].fmt_inner(f)?;
                for node in &nodes[1..] {
                    write!(f, " | ")?;
                    node.fmt_inner(f)?;
                }
                write!(f, ")")
            },
            NodeData::Concatenation(nodes) => {
                write!(f, "(")?;
                nodes[0].fmt_inner(f)?;
                for node in &nodes[1..] {
                    write!(f, " ")?;
                    node.fmt_inner(f)?;
                }
                write!(f, ")")
            },
            NodeData::KeyworkArgument(node) => {
                write!(f, "{}: ", self.text)?;
                node.fmt_inner(f)
            },
            _ => panic!(),
        }
    }
}

impl<'a> std::fmt::Debug for Node<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<")?;
        self.fmt_inner(f)?;
        write!(f, ">(")?;
        self.location.fmt(f)?;
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum NodeData<'a> {
    // Header: Value
    Header(Box<Node<'a>>),
    HeaderValue,
    // Rule{arguments} = node
    Rule {
        arguments: Vec<Node<'a>>,
        node: Box<Node<'a>>,
    },
    // variable
    Variable,
    // "\r\e\g\e\x"
    RegexTerminal {
        arguments: Vec<Node<'a>>,
    },
    // `literal`
    LiteralTerminal {
        arguments: Vec<Node<'a>>,
    },
    // %{args}
    Embed {
        arguments: Vec<Node<'a>>,
    },
    // ~a
    Passive(Box<Node<'a>>),
    // a*
    Repetition(Box<Node<'a>>),
    // a?
    Optional(Box<Node<'a>>),
    // a | b
    Alternation(Vec<Node<'a>>),
    // a b
    Concatenation(Vec<Node<'a>>),
    // !
    Capture(Box<Node<'a>>),
    // {positional-argument}
    PositionalArgument,
    // {keyword: argument}
    KeyworkArgument(Box<Node<'a>>),
    KeywordArgumentValue,

}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TextLocation {
    pub line: u32,
    pub column: u32,
}

impl TextLocation {
    const INITIAL: TextLocation = TextLocation { line: 0, column: 0 };

    pub fn new(line: u32, column: u32) -> TextLocation {
        TextLocation { line: line, column: column }
    }

    pub fn from_tuple((line, column): (u32, u32)) -> TextLocation {
        TextLocation::new(line, column)
    }

    fn increment(&mut self, chr: char) {
        if chr == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn extract_line<'a>(&self, source: &'a str) -> &'a str {
        source.split('\n').nth(self.line as usize).unwrap()
    }

    pub fn fmt_marker(&self) -> String {
        let mut s = String::new();
        for _ in 0..self.column {
            s.push(' ');
        }
        s.push('^');
        s
    }

    pub fn fmt_source(&self, source: &str) -> String {
        let line_number = format!("{}", self.line + 1);
        let mut spacing = String::new();
        for _ in 0..line_number.len() {
            spacing.push(' ');
        }

        format!("{} | {}\n{} | {}", line_number, self.extract_line(source), spacing, self.fmt_marker())
    }
}

impl std::fmt::Display for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl std::fmt::Debug for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{:?}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct ParseError {
    location: TextLocation,
    error: String,
}

impl ParseError {
    fn new(location: TextLocation, error: String) -> ParseError {
        ParseError { location: location, error: error }
    }

    pub fn fmt(&self, origin: &str, source: &str) -> String {
        format!("Parser Error: {} ({}:{})\n\n{}", self.error, origin, self.location, self.location.fmt_source(source))
    }
}

struct Parser<'a> {
    source: &'a str,
    nodes: Vec<Node<'a>>,
    location: TextLocation,
    iterator: Peekable<CharIndices<'a>>,
}

impl Parser<'_> {
    fn peek_char(&mut self) -> Option<char> {
        self.peek().map(|(_, c)| c)
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.iterator.peek().map(|p| *p)
    }

    fn next(&mut self) -> Option<(usize, char)> {
        let result = self.iterator.next();
        if let Some((_, chr)) = result {
            self.location.increment(chr);
        }
        result
    }

    fn char_error(&self, message: String) -> ParseError {
        ParseError::new(self.location.clone(), message)
    }
}

pub fn parse(source: &str) -> Result<Grammar, ParseError> {
    let mut parser = Parser {
        source: source,
        nodes: vec!(),
        location: TextLocation::INITIAL,
        iterator: source.char_indices().peekable(),
    };

    loop {
        skip_whitespace(&mut parser);

        if parser.peek_char().is_none() {
            break;
        } else {
            let node = parse_item(&mut parser)?;
            parser.nodes.push(node);
        }
    }

    Ok(Grammar { source: parser.source, nodes: parser.nodes })
}

fn is_identifier_char(chr: char) -> bool {
    chr.is_alphanumeric() || chr == '_' || chr == '-' || chr == '.'
}

fn skip_whitespace(parser: &mut Parser) {
    while let Some(chr) = parser.peek_char() {
        if chr == ' ' || chr == '\t' || chr == '\n' {
            parser.next();
        } else if chr == '#' {
            while parser.next().map_or('\n', |(_, c)| c) != '\n' {}
        } else {
            break;
        }
    }
}

fn parse_item<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let (location, ident) = parse_identifier(parser)?;

    skip_whitespace(parser);

    let data =
        match parser.peek_char() {
            Some(':') => {
                parser.next();

                NodeData::Header(parse_header_value(parser)?)
            },
            Some('{') => {
                let arguments = parse_arguments(parser)?;

                skip_whitespace(parser);

                if let Some(chr) = parser.peek_char() {
                    if chr != '=' {
                        return Err(parser.char_error(format!(
                            "Expecting the start of a rule '=', got {:?} instead", chr)));
                    }
                } else {
                    return Err(parser.char_error(
                        "Expecting the start of a rule '=', got EOF instead".to_string()));
                }
                parser.next();

                NodeData::Rule {
                    arguments: arguments,
                    node: parse_rule(parser)?
                }
            },
            Some('=') => {
                parser.next();

                NodeData::Rule {
                    arguments: vec!(),
                    node: parse_rule(parser)?
                }
            },
            Some(rchr) => {
                return Err(parser.char_error(format!(
                    "Expecting the start of a rule or header, got {:?} instead", rchr)));
            },
            None => {
                return Err(parser.char_error(format!(
                    "Expecting the start of a rule or header but got EOF instead")));
            },
        };

    Ok(Node::new(ident, location, data))
}

fn parse_identifier<'a>(parser: &mut Parser<'a>) -> Result<(TextLocation, &'a str), ParseError> {
    skip_whitespace(parser);

    let start: usize;
    if let Some((loc, chr)) = parser.peek() {
        if is_identifier_char(chr) {
            start = loc;
        } else {
            return Err(parser.char_error(
                format!("Expected an identifier, got {:?} instead", chr)))
        }
    } else {
        return Err(parser.char_error(
            "Expected an identifier, got EOF instead".to_string()))
    }

    let location = parser.location.clone();

    while let Some((end, chr)) = parser.peek() {
        if is_identifier_char(chr) {
            parser.next();
        } else {
            return Ok((location, &parser.source[start..end]));
        }
    }

    Ok((location, &parser.source[start..]))
}

fn parse_header_value<'a>(parser: &mut Parser<'a>) -> Result<Box<Node<'a>>, ParseError> {
    let start: usize;
    if let Some((loc, _)) = parser.peek() {
        start = loc;
    } else {
        return Err(parser.char_error(
            "Expected a header value, got EOF instead".to_string()))
    }

    let location = parser.location.clone();

    let mut end = parser.source.len();

    while let Some((loc, chr)) = parser.peek() {
        if chr != '\n' {
            parser.next();
        } else {
            end = loc;
            break;
        }
    }

    Ok(Box::new(Node::new(&parser.source[start..end], location, NodeData::HeaderValue)))
}

fn parse_arguments<'a>(parser: &mut Parser<'a>) -> Result<Vec<Node<'a>>, ParseError> {
    if parser.peek_char() != Some('{') {
        return Err(parser.char_error(
            "Expected a '{' to start arguments".to_string()));
    }

    parser.next();

    let mut arguments = vec!(parse_argument(parser)?);

    loop {
        match parser.peek_char() {
            Some('}') => {
                parser.next();
                break;
            },
            Some(',') => {
                parser.next();
                arguments.push(parse_argument(parser)?);
            },
            None => {
                return Err(parser.char_error(
                    "Expected '}', the end of an argument list. Got EOF instead".to_string()));
            },
            _ => panic!(), // parse_argument should eat all other characters
        }
    }

    Ok(arguments)
}

fn parse_argument<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let loc = parser.location.clone();
    let start: usize;
    if let Some((loc, _)) = parser.peek() {
        start = loc;
    } else {
        return Err(parser.char_error(
            "Expected an argument, got EOF instead".to_string()));
    }

    loop {
        match parser.peek() {
            Some((end, '}')) | Some((end, ',')) => {
                return Ok(Node::new(
                    &parser.source[start..end], loc, NodeData::PositionalArgument));
            },
            Some((end, ':')) => {
                parser.next();

                return Ok(Node::new(
                    &parser.source[start..end],
                    loc,
                    NodeData::KeyworkArgument(Box::new(parse_kwarg_value(parser)?))));
            },
            Some(_) => {},
            None => {
                // Let the caller handle it
                return Ok(Node::new("", loc, NodeData::PositionalArgument))
            },
        }
        parser.next();
    }
}

fn parse_kwarg_value<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let loc = parser.location.clone();
    let start: usize;
    if let Some((loc, _)) = parser.peek() {
        start = loc;
    } else {
        return Err(parser.char_error(
            "Expected a keyword argument value, got EOF instead".to_string()));
    }

    loop {
        match parser.peek() {
            Some((end, '}')) | Some((end, ',')) => {
                return Ok(Node::new(
                    &parser.source[start..end], loc, NodeData::KeywordArgumentValue));
            },
            Some(_) => {},
            None => {
                // Let the caller handle it
                return Ok(Node::new("", loc, NodeData::KeywordArgumentValue))
            }
        }
        parser.next();
    }
}

fn parse_rule<'a>(parser: &mut Parser<'a>) -> Result<Box<Node<'a>>, ParseError> {
    let node = parse_rule_alternation(parser)?;

    skip_whitespace(parser);

    if let Some((_, chr)) = parser.peek() {
        if chr != ';' {
            return Err(parser.char_error(
                format!("Expected end of rule ';', but got {:?} instead", chr)));
        }
    } else {
        return Err(parser.char_error(
            "Expected end of rule ';', but got EOF instead".to_string()));
    }

    parser.next();

    Ok(Box::new(node))
}

fn parse_rule_alternation<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let location = parser.location.clone();

    let first_element = parse_rule_concatenation(parser)?;

    skip_whitespace(parser);

    if let Some(chr) = parser.peek_char() {
        if chr == ')' || chr == ';' {
            return Ok(first_element);
        } else if chr == '|' {
            parser.next();
        } else {
            return Err(parser.char_error(
                format!("Expected end of rule ';', but got {:?} instead", chr)));
        }
    } else {
        return Err(parser.char_error(
            "Expected end of rule ';', but got EOF instead".to_string()));
    }

    let mut elements = vec!(first_element);

    loop {
        elements.push(parse_rule_concatenation(parser)?);

        skip_whitespace(parser);

        if let Some(chr) = parser.peek_char() {
            if chr == ')' || chr == ';' {
                break;
            } else if chr == '|' {
                parser.next();
            } else {
                return Err(parser.char_error(
                    format!("Expected end of rule ';', but got {:?} instead", chr)));
            }
        } else {
            return Err(parser.char_error(
                "Expected end of rule ';', but got EOF instead".to_string()));
        }
    }

    Ok(Node::new("", location, NodeData::Alternation(elements)))
}


fn parse_rule_concatenation<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let location = parser.location.clone();

    let first_element = parse_rule_element(parser)?;

    skip_whitespace(parser);

    if let Some(chr) = parser.peek_char() {
        if chr == ')' || chr == ';' || chr == '|' {
            return Ok(first_element);
        }
    } else {
        return Err(parser.char_error(
            "Expected end of rule ';', but got EOF instead".to_string()));
    }

    let mut elements = vec!(first_element);

    loop {
        elements.push(parse_rule_element(parser)?);

        skip_whitespace(parser);

        if let Some(chr) = parser.peek_char() {
            if chr == ')' || chr == ';' || chr == '|' {
                break;
            }
        } else {
            return Err(parser.char_error(
                "Expected end of rule ';', but got EOF instead".to_string()));
        }
    }

    Ok(Node::new("", location, NodeData::Concatenation(elements)))
}

fn parse_rule_element<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    skip_whitespace(parser);

    let location = parser.location.clone();

    if let Some((_, chr)) = parser.peek() {
        if chr == '!' {
            parser.next();

            Ok(Node::new(
                "",
                location,
                NodeData::Capture(Box::new(
                    parse_rule_element_contents(parser)?))))
        } else if chr == '~' {
            parser.next();

            Ok(Node::new(
                "",
                location,
                NodeData::Passive(Box::new(
                    parse_rule_element_contents(parser)?))))
        } else {
            parse_rule_element_contents(parser)
        }
    } else {
        Err(parser.char_error(
            "Expected a terminal, variable or group, got EOF instead".to_string()))
    }

}

fn parse_rule_element_contents<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    skip_whitespace(parser);

    let element: Node<'a>;

    if let Some((_, first_char)) = parser.peek() {
        if first_char == '(' {
            parser.next();

            element = parse_rule_alternation(parser)?;

            if let Some(chr) = parser.peek_char() {
                if chr == ')' {
                    parser.next();
                } else {
                    return Err(parser.char_error(
                        format!("Expected end of a group ')', got {:?} instead", chr)));
                }
            } else {
                return Err(parser.char_error(
                    "Expected end of a group ')', got EOF instead".to_string()));
            }
        } else if first_char == '\'' {
            element = parse_regex_terminal(parser)?;
        } else if first_char == '`' {
            element = parse_literal_terminal(parser)?;
        } else if first_char == '%' {
            let location = parser.location.clone();

            parser.next();

            element = Node::new(
                "",
                location,
                NodeData::Embed { arguments: parse_arguments(parser)? });
        } else if is_identifier_char(first_char) {
            let (location, ident) = parse_identifier(parser)?;

            element = Node::new(ident, location, NodeData::Variable);
        } else {
            return Err(parser.char_error(format!(
                "Expected a terminal, variable or group, got {:?} instead", first_char)));
        }
    } else {
        return Err(parser.char_error(
            "Expected a terminal, variable or group, got EOF instead".to_string()));
    }

    skip_whitespace(parser);

    Ok(if let Some(chr) = parser.peek_char() {
        let location = parser.location.clone();

        if chr == '*' {
            parser.next();

            Node::new("", location,
                    NodeData::Repetition(Box::new(element)))
        } else if chr == '?' {
            parser.next();

            Node::new("", location,
                    NodeData::Optional(Box::new(element)))
        } else {
            element
        }
    } else {
        element
    })
}

fn parse_regex_terminal<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    // Assume we've parsed the first character
    let (first_pos, first) = parser.next().unwrap();
    assert!(first == '\'');

    // The node should contain only terminal contents
    let start = first_pos + 1;
    let location = parser.location.clone();

    let mut has_escape = false;
    loop {
        let chr = parser.peek_char().ok_or(parser.char_error(
            "Expected the end of the terminal '\"' but got EOF instead".to_string()))?;

        if has_escape {
            has_escape = false;
        } else {
            if chr == '\'' {
                break;
            } else if chr == '\\' {
                has_escape = true;
            }
        }

        parser.next();
    }

    let (end, end_chr) = parser.next().unwrap();
    assert!(end_chr == '\''); // sanity

    skip_whitespace(parser);

    let arguments =
        if parser.peek_char() == Some('{') {
            parse_arguments(parser)?
        } else {
            vec!()
        };

    Ok(Node::new(&parser.source[start..end], location,
                 NodeData::RegexTerminal { arguments: arguments }))
}

fn parse_literal_terminal<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    // Assume we've parsed the first character
    let (first_pos, first) = parser.next().unwrap();
    assert!(first == '`');

    // The node should contain only terminal contents
    let start = first_pos + 1;
    let location = parser.location.clone();

    while parser.peek_char().unwrap_or('`') != '`' {
        parser.next();
    }

    let (end, end_chr) = parser.next()
        .ok_or(parser.char_error(
            "Expected the end of the terminal '`', but got EOF instead".to_string()))?;
    assert!(end_chr == '`'); // sanity

    skip_whitespace(parser);

    let arguments =
        if parser.peek_char() == Some('{') {
            parse_arguments(parser)?
        } else {
            vec!()
        };

    Ok(Node::new(&parser.source[start..end], location,
                 NodeData::LiteralTerminal { arguments: arguments }))
}

#[cfg(test)]
mod tests {
    use crate::sbnf::*;

    #[test]
    fn parse_empty() {
        assert!(parse("").unwrap().nodes.is_empty());
    }

    fn header<'a>(header: &'a str, header_loc: (u32, u32), value: &'a str, value_loc: (u32, u32)) -> Node<'a> {
        Node::new(header, TextLocation::from_tuple(header_loc), NodeData::Header(Box::new(
            Node::new(value, TextLocation::from_tuple(value_loc), NodeData::HeaderValue))))
    }

    #[test]
    fn parse_headers() {
        assert!(parse("a").is_err());
        assert!(parse("a:").is_err());
        assert!(parse("a :").is_err());
        assert!(parse(" a :").is_err());
        assert!(parse("a:b").unwrap().nodes == vec!(
            header("a", (0, 0), "b", (0, 2)),
        ));
        assert!(parse("a:\nb: f\t\n\nbar :foo\n").unwrap().nodes == vec!(
            header("a", (0, 0), "", (0, 2)),
            header("b", (1, 0), " f\t", (1, 2)),
            header("bar", (3, 0), "foo", (3, 5)),
        ));
        assert!(parse("a\n:b").unwrap().nodes == vec!(
            header("a", (0, 0), "b", (1, 1)),
        ));
    }

    fn rule<'a>(name: &'a str, loc: (u32, u32), arguments: Vec<Node<'a>>, node: Node<'a>) -> Node<'a> {
        Node::new(name, TextLocation::from_tuple(loc),
                  NodeData::Rule { arguments: arguments, node: Box::new(node) })
    }

    fn concat<'a>(loc: (u32, u32), nodes: Vec<Node<'a>>) -> Node<'a> {
        Node::new(
            "", TextLocation::from_tuple(loc), NodeData::Concatenation(nodes))
    }

    fn alt<'a>(loc: (u32, u32), nodes: Vec<Node<'a>>) -> Node<'a> {
        Node::new(
            "", TextLocation::from_tuple(loc), NodeData::Alternation(nodes))
    }

    fn var<'a>(name: &'a str, loc: (u32, u32)) -> Node<'a> {
        Node::new(name, TextLocation::from_tuple(loc), NodeData::Variable)
    }

    fn literal<'a>(contents: &'a str, loc: (u32, u32), arguments: Vec<Node<'a>>) -> Node<'a> {
        Node::new(contents, TextLocation::from_tuple(loc),
                  NodeData::LiteralTerminal { arguments: arguments })
    }

    fn regex<'a>(contents: &'a str, loc: (u32, u32), arguments: Vec<Node<'a>>) -> Node<'a> {
        Node::new(contents, TextLocation::from_tuple(loc),
                  NodeData::RegexTerminal { arguments: arguments })
    }

    fn passive<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new("", TextLocation::from_tuple(loc),
                  NodeData::Passive(Box::new(node)))
    }

    fn capture<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new("", TextLocation::from_tuple(loc),
                  NodeData::Capture(Box::new(node)))
    }

    fn repetition<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new("", TextLocation::from_tuple(loc),
                  NodeData::Repetition(Box::new(node)))
    }

    fn optional<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new("", TextLocation::from_tuple(loc),
                  NodeData::Optional(Box::new(node)))
    }

    fn keyarg<'a>(key: &'a str, key_loc: (u32, u32), value: &'a str, value_loc: (u32, u32)) -> Node<'a> {
        Node::new(key, TextLocation::from_tuple(key_loc), NodeData::KeyworkArgument(Box::new(
            Node::new(value, TextLocation::from_tuple(value_loc), NodeData::KeywordArgumentValue))))
    }

    fn arg<'a>(value: &'a str, loc: (u32, u32)) -> Node<'a> {
        Node::new(value, TextLocation::from_tuple(loc), NodeData::PositionalArgument)
    }

    #[test]
    fn parse_rules() {
        assert!(parse("a=").is_err());
        assert!(parse("a=a").is_err());
        assert!(parse("a=(").is_err());
        assert!(parse("a=(a").is_err());
        assert!(parse("a=(a;").is_err());
        assert!(parse("a=b;").unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(), var("b", (0, 2))),
        ));
        assert!(parse("a =b c |d ;b=`a`;").unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(), alt((0, 3), vec!(
                concat((0, 3), vec!(
                    var("b", (0, 3)),
                    var("c", (0, 5)),
                )),
                var("d", (0, 8)),
            ))),
            rule("b", (0, 11), vec!(), literal("a", (0, 14), vec!())),
        ));
        assert!(parse("a=~(b c)? (d|(e)|f) !g*;").unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(), concat((0, 2), vec!(
                passive((0, 2), optional((0, 8), concat((0, 4), vec!(
                    var("b", (0, 4)),
                    var("c", (0, 6)),
                )))),
                alt((0, 11), vec!(
                    var("d", (0, 11)),
                    var("e", (0, 14)),
                    var("f", (0, 17)),
                )),
                capture((0, 20), repetition((0, 22), var("g", (0, 21)))),
            ))),
        ));
        assert!(parse("a{b c, 2:d, e}=a;").unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(
                    arg("b c", (0, 2)),
                    keyarg(" 2", (0, 6), "d", (0, 9)),
                    arg(" e", (0, 11)),
                ), var("a", (0, 15)))
        ));
    }

    #[test]
    fn parse_terminals() {
        assert!(parse("a=`;").is_err());
        assert!(parse("a=\';").is_err());
        assert!(parse("a=`\\`;").unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(), literal("\\", (0, 3), vec!()))
        ));
        assert!(parse(r#"a='\';"#).is_err());
        assert!(parse(r#"a='\'';"#).unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(), regex("\\'", (0, 3), vec!()))
        ));
        assert!(parse(r#"a='b(c)'{d, 1 :d e}?;"#).unwrap().nodes == vec!(
            rule("a", (0, 0), vec!(),
                optional((0, 19), regex("b(c)", (0, 3), vec!(
                    arg("d", (0, 9)),
                    keyarg(" 1 ", (0, 11), "d e", (0, 15)),
                )))
            )
        ));
    }
}
