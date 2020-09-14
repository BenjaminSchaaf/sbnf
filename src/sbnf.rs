/// This file implements a parser for the SBNF grammar
use std::str::{from_utf8_unchecked, Chars};

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
    pub fn new(
        text: &'a str,
        location: TextLocation,
        data: NodeData<'a>,
    ) -> Node<'a> {
        Node { text: text, location: location, data: data }
    }

    pub fn get_regex(&'a self) -> &'a str {
        match &self.data {
            NodeData::RegexTerminal { .. } => self.text,
            NodeData::LiteralTerminal { regex, .. } => regex,
            _ => panic!(),
        }
    }

    pub fn get_options(&'a self) -> &'a Vec<Node<'a>> {
        match &self.data {
            NodeData::RegexTerminal { options, .. }
            | NodeData::LiteralTerminal { options, .. } => options,
            _ => panic!(),
        }
    }

    fn fmt_inner_parameters(
        &self,
        f: &mut std::fmt::Formatter,
        params: &Vec<Node<'a>>,
        start: &str,
        end: &str,
    ) -> std::fmt::Result {
        if !params.is_empty() {
            write!(f, "{}", start)?;
            params[0].fmt_inner(f)?;
            for arg in &params[1..] {
                write!(f, ", ")?;
                arg.fmt_inner(f)?;
            }
            write!(f, "{}", end)?;
        }
        Ok(())
    }

    fn fmt_inner(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.data {
            NodeData::SyntaxParameters(parameters) => {
                self.fmt_inner_parameters(f, &parameters, "[", "]")
            }
            NodeData::Header(value) => {
                write!(f, "{}: ", self.text)?;
                value.fmt_inner(f)
            }
            NodeData::HeaderValue
            | NodeData::PositionalArgument
            | NodeData::KeywordArgumentValue => write!(f, "{}", self.text),
            NodeData::Variable { parameters } => {
                write!(f, "{}", self.text)?;

                self.fmt_inner_parameters(f, &parameters, "[", "]")
            }
            NodeData::Rule { parameters, options, node } => {
                write!(f, "{}", self.text)?;
                self.fmt_inner_parameters(f, &parameters, "[", "]")?;
                self.fmt_inner_parameters(f, &options, "{", "}")?;
                write!(f, " = ")?;
                node.fmt_inner(f)?;
                write!(f, " ;")
            }
            NodeData::RegexTerminal { options, embed } => {
                write!(f, "'{}'", self.text)?;
                self.fmt_inner_parameters(f, &options, "{", "}")?;
                if let Some(embed) = embed {
                    write!(f, " ")?;
                    embed.fmt_inner(f)?;
                }
                Ok(())
            }
            NodeData::LiteralTerminal { options, .. } => {
                write!(f, "`{}`", self.text)?;
                self.fmt_inner_parameters(f, &options, "{", "}")
            }
            NodeData::Passive(node) => {
                write!(f, "~(")?;
                node.fmt_inner(f)?;
                write!(f, ")")
            }
            NodeData::Repetition(node) => {
                write!(f, "(")?;
                node.fmt_inner(f)?;
                write!(f, ")*")
            }
            NodeData::Optional(node) => {
                write!(f, "(")?;
                node.fmt_inner(f)?;
                write!(f, ")?")
            }
            NodeData::Alternation(nodes) => {
                write!(f, "(")?;
                nodes[0].fmt_inner(f)?;
                for node in &nodes[1..] {
                    write!(f, " | ")?;
                    node.fmt_inner(f)?;
                }
                write!(f, ")")
            }
            NodeData::Concatenation(nodes) => {
                write!(f, "(")?;
                nodes[0].fmt_inner(f)?;
                for node in &nodes[1..] {
                    write!(f, " ")?;
                    node.fmt_inner(f)?;
                }
                write!(f, ")")
            }
            NodeData::KeywordArgument(node) => {
                write!(f, "{}: ", self.text)?;
                node.fmt_inner(f)
            }
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
    // [parameters]
    SyntaxParameters(Vec<Node<'a>>),
    // Header: Value
    Header(Box<Node<'a>>),
    HeaderValue,
    // Rule[parameters]{options} = node
    Rule {
        parameters: Vec<Node<'a>>,
        options: Vec<Node<'a>>,
        node: Box<Node<'a>>,
    },
    // variable[parameters]
    Variable {
        parameters: Vec<Node<'a>>,
    },
    // "\r\e\g\e\x"
    RegexTerminal {
        options: Vec<Node<'a>>,
        embed: Option<Box<Node<'a>>>,
    },
    // `literal`
    LiteralTerminal {
        regex: String,
        options: Vec<Node<'a>>,
        embed: Option<Box<Node<'a>>>,
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
    KeywordArgument(Box<Node<'a>>),
    KeywordArgumentValue,
    // %embed[]{}
    Embed {
        parameters: Vec<Node<'a>>,
        options: Vec<Node<'a>>,
    },
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

        format!(
            "{} | {}\n{} | {}",
            line_number,
            self.extract_line(source),
            spacing,
            self.fmt_marker()
        )
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
        format!(
            "Parser Error: {} ({}:{})\n{}",
            self.error,
            origin,
            self.location,
            self.location.fmt_source(source)
        )
    }
}

// Can't use Peekable here, as you can't access the underlying iterator, which
// is required for str_from_iterators. Why‽
struct Parser<'a> {
    source: &'a str,
    location: TextLocation,

    current: Chars<'a>,
    peeked_char: Option<char>,
}

// A fast way to convert an interval of iterators to a substring. Rust should
// at least have an easy way to get byte indices from Chars :(
fn str_from_iterators<'a>(
    string: &'a str,
    start: Chars<'a>,
    end: Chars<'a>,
) -> &'a str {
    // Convert start and end into byte offsets
    let bytes_start = string.as_bytes().len() - start.as_str().as_bytes().len();
    let bytes_end = string.as_bytes().len() - end.as_str().as_bytes().len();

    // SAFETY: As long as the iterators are from the string the byte offsets
    // will always be valid.
    unsafe { from_utf8_unchecked(&string.as_bytes()[bytes_start..bytes_end]) }
}

impl<'a> Parser<'a> {
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
        let result = self.current.next();

        if let Some(chr) = result {
            self.location.increment(chr);
        }

        result
    }

    fn char_error(&self, message: String) -> ParseError {
        ParseError::new(self.location.clone(), message)
    }

    fn start_node_collection(&mut self) -> NodeCollector<'a> {
        let location = self.location.clone();
        let start = self.current.clone();

        NodeCollector { location, start }
    }
}

struct NodeCollector<'a> {
    location: TextLocation,
    start: Chars<'a>,
}

impl<'a> NodeCollector<'a> {
    fn build_from_parser(
        self,
        parser: &Parser<'a>,
        data: NodeData<'a>,
    ) -> Node<'a> {
        self.end_from_parser(parser).build(data)
    }

    fn build_from_text(self, text: &'a str, data: NodeData<'a>) -> Node<'a> {
        self.end_from_text(text).build(data)
    }

    fn end_from_parser(self, parser: &Parser<'a>) -> CollectedNode<'a> {
        let text = str_from_iterators(
            parser.source,
            self.start.clone(),
            parser.current.clone(),
        );

        self.end_from_text(text)
    }

    fn end_from_iterators(
        self,
        parser: &Parser<'a>,
        start: Chars<'a>,
        end: Chars<'a>,
    ) -> CollectedNode<'a> {
        let text = str_from_iterators(parser.source, start, end);

        self.end_from_text(text)
    }

    fn end_from_text(self, text: &'a str) -> CollectedNode<'a> {
        CollectedNode { location: self.location, text }
    }
}

struct CollectedNode<'a> {
    location: TextLocation,
    text: &'a str,
}

impl<'a> CollectedNode<'a> {
    fn build(self, data: NodeData<'a>) -> Node<'a> {
        Node::new(self.text, self.location, data)
    }
}

pub fn parse(source: &str) -> Result<Grammar, ParseError> {
    let mut parser = Parser {
        source,
        location: TextLocation::INITIAL,
        current: source.chars(),
        peeked_char: None,
    };

    let mut nodes = vec![];

    loop {
        skip_whitespace(&mut parser);

        if parser.peek().is_none() {
            break;
        } else {
            nodes.push(parse_item(&mut parser)?);
        }
    }

    Ok(Grammar { source: parser.source, nodes: nodes })
}

pub fn is_identifier_char(chr: char) -> bool {
    chr.is_alphanumeric() || chr == '_' || chr == '-' || chr == '.'
}

fn skip_whitespace(parser: &mut Parser) {
    while let Some(chr) = parser.peek() {
        if chr == ' ' || chr == '\t' || chr == '\n' {
            parser.next();
        } else if chr == '#' {
            while parser.next().unwrap_or('\n') != '\n' {}
        } else {
            break;
        }
    }
}

fn parse_identifier<'a>(
    parser: &mut Parser<'a>,
) -> Result<CollectedNode<'a>, ParseError> {
    let col = parser.start_node_collection();

    if let Some(chr) = parser.peek() {
        if !is_identifier_char(chr) {
            return Err(parser.char_error(format!(
                "Expected an identifier, got {:?} instead",
                chr
            )));
        }
    } else {
        return Err(parser.char_error(
            "Expected an identifier, got EOF instead".to_string(),
        ));
    }

    while let Some(chr) = parser.peek() {
        if is_identifier_char(chr) {
            parser.next();
        } else {
            break;
        }
    }

    Ok(col.end_from_parser(parser))
}

fn parse_item<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    skip_whitespace(parser);

    if parser.peek() == Some('[') {
        let node = parser.start_node_collection();

        let params = parse_parameters(parser)?;

        return Ok(node.build_from_text("", NodeData::SyntaxParameters(params)));
    }

    let ident = parse_identifier(parser)?;

    skip_whitespace(parser);

    let data = match parser.peek() {
        Some(':') => {
            parser.next();

            NodeData::Header(parse_header_value(parser)?)
        }
        Some(_) => {
            let parameters = if parser.peek() == Some('[') {
                let params = parse_parameters(parser)?;

                skip_whitespace(parser);

                params
            } else {
                vec![]
            };

            let options = if parser.peek() == Some('{') {
                let params = parse_options(parser)?;

                skip_whitespace(parser);

                params
            } else {
                vec![]
            };

            match parser.next() {
                Some('=') => {}
                Some(chr) => {
                    return Err(parser.char_error(format!(
                        "Expected the start of a rule '=', got {:?} instead",
                        chr
                    )))
                }
                None => {
                    return Err(parser.char_error(
                        "Expecting the start of a rule '=', got EOF instead"
                            .to_string(),
                    ))
                }
            }

            NodeData::Rule { parameters, options, node: parse_rule(parser)? }
        }
        None => {
            return Err(parser.char_error(format!(
                "Expecting the start of a rule or header but got EOF instead"
            )));
        }
    };

    Ok(ident.build(data))
}

fn parse_header_value<'a>(
    parser: &mut Parser<'a>,
) -> Result<Box<Node<'a>>, ParseError> {
    let col = parser.start_node_collection();

    if parser.peek().is_none() {
        return Err(parser.char_error(
            "Expected a header value, got EOF instead".to_string(),
        ));
    }

    while let Some(chr) = parser.peek() {
        if chr != '\n' {
            parser.next();
        } else {
            break;
        }
    }

    Ok(Box::new(col.build_from_parser(parser, NodeData::HeaderValue)))
}

fn parse_parameters<'a>(
    parser: &mut Parser<'a>,
) -> Result<Vec<Node<'a>>, ParseError> {
    if parser.peek() != Some('[') {
        return Err(
            parser.char_error("Expected a '[' to start parameters".to_string())
        );
    }

    parser.next();

    let mut parameters = vec![];

    loop {
        skip_whitespace(parser);

        match parser.peek() {
            Some('\'') => {
                let regex = parse_regex(parser)?;

                parameters.push(regex.build(NodeData::RegexTerminal {
                    options: vec![],
                    embed: None,
                }));
            }
            Some('`') => {
                let (literal, regex) = parse_literal(parser)?;

                parameters.push(literal.build(NodeData::LiteralTerminal {
                    regex,
                    options: vec![],
                    embed: None,
                }));
            }
            Some(_) => {
                let variable = parse_identifier(parser)?;

                parameters.push(
                    variable.build(NodeData::Variable { parameters: vec![] }),
                );
            }
            None => {
                return Err(parser.char_error(
                    "Expected an argument like a string or variable. Got EOF instead".to_string()));
            }
        }

        skip_whitespace(parser);

        match parser.next() {
            Some(',') => continue,
            Some(']') => break,
            Some(chr) => {
                return Err(parser.char_error(
                    format!("Expected end of arguments ']' or continuation ',', but got {:?} instead", chr)));
            }
            None => {
                return Err(parser.char_error(
                    "Expected either ',' or ']' for the end of parameters. Got EOF instead".to_string()));
            }
        }
    }

    Ok(parameters)
}

fn parse_options<'a>(
    parser: &mut Parser<'a>,
) -> Result<Vec<Node<'a>>, ParseError> {
    if parser.peek() != Some('{') {
        return Err(parser.char_error(
            "Expected a '{' to start meta-parameters".to_string(),
        ));
    }

    parser.next();

    let mut arguments = vec![parse_argument(parser)?];

    loop {
        match parser.next() {
            Some('}') => {
                break;
            }
            Some(',') => {
                arguments.push(parse_argument(parser)?);
            }
            None => {
                return Err(parser.char_error(
                    "Expected '}', the end of an argument list. Got EOF instead".to_string()));
            }
            _ => panic!(), // parse_argument should eat all other characters
        }
    }

    Ok(arguments)
}

fn parse_argument<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

    if parser.peek().is_none() {
        return Err(parser
            .char_error("Expected an argument, got EOF instead".to_string()));
    }

    loop {
        match parser.peek() {
            // When None let the caller handle it
            Some('}') | Some(',') | None => {
                return Ok(
                    col.build_from_parser(parser, NodeData::PositionalArgument)
                );
            }
            Some(':') => {
                let node = col.end_from_parser(parser);

                parser.next();

                let kwarg = parse_kwarg_value(parser)?;

                return Ok(
                    node.build(NodeData::KeywordArgument(Box::new(kwarg)))
                );
            }
            Some(_) => {}
        }
        parser.next();
    }
}

fn parse_kwarg_value<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

    if parser.peek().is_none() {
        return Err(parser.char_error(
            "Expected a keyword argument value, got EOF instead".to_string(),
        ));
    }

    loop {
        match parser.peek() {
            // When None let the caller handle it
            Some('}') | Some(',') | None => {
                return Ok(col.build_from_parser(
                    parser,
                    NodeData::KeywordArgumentValue,
                ));
            }
            Some(_) => {}
        }
        parser.next();
    }
}

fn parse_rule<'a>(
    parser: &mut Parser<'a>,
) -> Result<Box<Node<'a>>, ParseError> {
    let node = parse_rule_alternation(parser)?;

    skip_whitespace(parser);

    if let Some(chr) = parser.peek() {
        if chr != ';' {
            return Err(parser.char_error(format!(
                "Expected end of rule ';', but got {:?} instead",
                chr
            )));
        }
    } else {
        return Err(parser.char_error(
            "Expected end of rule ';', but got EOF instead".to_string(),
        ));
    }

    parser.next();

    Ok(Box::new(node))
}

fn parse_rule_alternation<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

    let first_element = parse_rule_concatenation(parser)?;

    skip_whitespace(parser);

    if let Some(chr) = parser.peek() {
        if chr == ')' || chr == ';' {
            return Ok(first_element);
        } else if chr == '|' {
            parser.next();
        } else {
            return Err(parser.char_error(format!(
                "Expected end of rule ';', but got {:?} instead",
                chr
            )));
        }
    } else {
        return Err(parser.char_error(
            "Expected end of rule ';', but got EOF instead".to_string(),
        ));
    }

    let mut elements = vec![first_element];

    loop {
        elements.push(parse_rule_concatenation(parser)?);

        skip_whitespace(parser);

        if let Some(chr) = parser.peek() {
            if chr == ')' || chr == ';' {
                break;
            } else if chr == '|' {
                parser.next();
            } else {
                return Err(parser.char_error(format!(
                    "Expected end of rule ';', but got {:?} instead",
                    chr
                )));
            }
        } else {
            return Err(parser.char_error(
                "Expected end of rule ';', but got EOF instead".to_string(),
            ));
        }
    }

    Ok(col.build_from_text("", NodeData::Alternation(elements)))
}

fn parse_rule_concatenation<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

    let first_element = parse_rule_element(parser)?;

    skip_whitespace(parser);

    if let Some(chr) = parser.peek() {
        if chr == ')' || chr == ';' || chr == '|' {
            return Ok(first_element);
        }
    } else {
        return Err(parser.char_error(
            "Expected end of rule ';', but got EOF instead".to_string(),
        ));
    }

    let mut elements = vec![first_element];

    loop {
        elements.push(parse_rule_element(parser)?);

        skip_whitespace(parser);

        if let Some(chr) = parser.peek() {
            if chr == ')' || chr == ';' || chr == '|' {
                break;
            }
        } else {
            return Err(parser.char_error(
                "Expected end of rule ';', but got EOF instead".to_string(),
            ));
        }
    }

    Ok(col.build_from_text("", NodeData::Concatenation(elements)))
}

fn parse_rule_element<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    skip_whitespace(parser);

    let col = parser.start_node_collection();

    if let Some(chr) = parser.peek() {
        if chr == '!' {
            parser.next();

            let element = Box::new(parse_rule_element_contents(parser)?);

            Ok(col.build_from_parser(parser, NodeData::Capture(element)))
        } else if chr == '~' {
            parser.next();

            let element = Box::new(parse_rule_element_contents(parser)?);

            Ok(col.build_from_parser(parser, NodeData::Passive(element)))
        } else {
            parse_rule_element_contents(parser)
        }
    } else {
        Err(parser.char_error(
            "Expected a terminal, variable or group, got EOF instead"
                .to_string(),
        ))
    }
}

fn parse_rule_element_contents<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    skip_whitespace(parser);

    let element: Node<'a>;

    if let Some(first_char) = parser.peek() {
        if first_char == '(' {
            parser.next();

            element = parse_rule_alternation(parser)?;

            if let Some(chr) = parser.peek() {
                if chr == ')' {
                    parser.next();
                } else {
                    return Err(parser.char_error(format!(
                        "Expected end of a group ')', got {:?} instead",
                        chr
                    )));
                }
            } else {
                return Err(parser.char_error(
                    "Expected end of a group ')', got EOF instead".to_string(),
                ));
            }
        } else if first_char == '\'' {
            element = parse_regex_terminal(parser)?;
        } else if first_char == '`' {
            element = parse_literal_terminal(parser)?;
        } else if is_identifier_char(first_char) {
            let ident = parse_identifier(parser)?;

            skip_whitespace(parser);

            let parameters = if parser.peek() == Some('[') {
                parse_parameters(parser)?
            } else {
                vec![]
            };

            element = ident.build(NodeData::Variable { parameters });
        } else {
            return Err(parser.char_error(format!(
                "Expected a terminal, variable or group, got {:?} instead",
                first_char
            )));
        }
    } else {
        return Err(parser.char_error(
            "Expected a terminal, variable or group, got EOF instead"
                .to_string(),
        ));
    }

    skip_whitespace(parser);

    Ok(if let Some(chr) = parser.peek() {
        let col = parser.start_node_collection();

        if chr == '*' {
            parser.next();

            col.build_from_parser(
                parser,
                NodeData::Repetition(Box::new(element)),
            )
        } else if chr == '?' {
            parser.next();

            col.build_from_parser(parser, NodeData::Optional(Box::new(element)))
        } else {
            element
        }
    } else {
        element
    })
}

fn parse_embed<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let first = parser.next().unwrap();
    assert!(first == '%');

    skip_whitespace(parser);

    let word = parse_identifier(parser)?;

    let parameters = parse_parameters(parser)?;

    let options = parse_options(parser)?;

    Ok(word.build(NodeData::Embed { parameters, options }))
}

fn parse_regex<'a>(
    parser: &mut Parser<'a>,
) -> Result<CollectedNode<'a>, ParseError> {
    let col = parser.start_node_collection();

    // Assume we've parsed the first character
    let first = parser.next().unwrap();
    assert!(first == '\'');

    let start = parser.current.clone();

    let mut has_escape = false;
    loop {
        let chr = parser.peek().ok_or(
            parser.char_error(
                "Expected the end of the regex `\'` but got EOF instead"
                    .to_string(),
            ),
        )?;

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

    let end = parser.current.clone();

    let end_chr = parser.next().unwrap();
    assert!(end_chr == '\''); // sanity

    Ok(col.end_from_iterators(parser, start, end))
}

fn parse_regex_terminal<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    let regex = parse_regex(parser)?;

    skip_whitespace(parser);

    let options = if parser.peek() == Some('{') {
        parse_options(parser)?
    } else {
        vec![]
    };

    skip_whitespace(parser);

    let embed = if parser.peek() == Some('%') {
        Some(Box::new(parse_embed(parser)?))
    } else {
        None
    };

    Ok(regex.build(NodeData::RegexTerminal { options, embed }))
}

fn parse_literal<'a>(
    parser: &mut Parser<'a>,
) -> Result<(CollectedNode<'a>, String), ParseError> {
    let col = parser.start_node_collection();

    // Assume we've parsed the first character
    let first = parser.next().unwrap();
    assert!(first == '`');

    let start = parser.current.clone();

    while parser.peek().unwrap_or('`') != '`' {
        parser.next();
    }

    let end = parser.current.clone();

    let end_chr = parser.next().ok_or(parser.char_error(
        "Expected the end of the terminal '`', but got EOF instead".to_string(),
    ))?;
    assert!(end_chr == '`'); // sanity

    let node = col.end_from_iterators(parser, start, end);

    // Convert the literal to a regex now. This makes further compilation much
    // simpler.
    let regex = literal_to_regex(node.text);

    Ok((node, regex))
}

fn parse_literal_terminal<'a>(
    parser: &mut Parser<'a>,
) -> Result<Node<'a>, ParseError> {
    let (text, regex) = parse_literal(parser)?;

    skip_whitespace(parser);

    let options = if parser.peek() == Some('{') {
        parse_options(parser)?
    } else {
        vec![]
    };

    skip_whitespace(parser);

    let embed = if parser.peek() == Some('%') {
        Some(Box::new(parse_embed(parser)?))
    } else {
        None
    };

    Ok(text.build(NodeData::LiteralTerminal { regex, options, embed }))
}

fn literal_to_regex(literal: &str) -> String {
    const ESCAPE_CHARACTERS: &str = "^$\\'.*+?()[]{}|";

    let mut result = String::new();
    for chr in literal.chars() {
        if ESCAPE_CHARACTERS.find(chr).is_some() {
            result.push('\\');
        }

        result.push(chr);
    }
    result
}

#[cfg(test)]
mod tests {
    use crate::sbnf::*;

    #[test]
    fn parse_empty() {
        assert!(parse("").unwrap().nodes.is_empty());
    }

    fn header<'a>(
        header: &'a str,
        header_loc: (u32, u32),
        value: &'a str,
        value_loc: (u32, u32),
    ) -> Node<'a> {
        Node::new(
            header,
            TextLocation::from_tuple(header_loc),
            NodeData::Header(Box::new(Node::new(
                value,
                TextLocation::from_tuple(value_loc),
                NodeData::HeaderValue,
            ))),
        )
    }

    fn syn_params<'a>(loc: (u32, u32), nodes: Vec<Node<'a>>) -> Node<'a> {
        Node::new(
            "",
            TextLocation::from_tuple(loc),
            NodeData::SyntaxParameters(nodes),
        )
    }

    fn rule<'a>(
        name: &'a str,
        loc: (u32, u32),
        parameters: Vec<Node<'a>>,
        options: Vec<Node<'a>>,
        node: Node<'a>,
    ) -> Node<'a> {
        Node::new(
            name,
            TextLocation::from_tuple(loc),
            NodeData::Rule { parameters, options, node: Box::new(node) },
        )
    }

    fn concat<'a>(loc: (u32, u32), nodes: Vec<Node<'a>>) -> Node<'a> {
        Node::new(
            "",
            TextLocation::from_tuple(loc),
            NodeData::Concatenation(nodes),
        )
    }

    fn alt<'a>(loc: (u32, u32), nodes: Vec<Node<'a>>) -> Node<'a> {
        Node::new(
            "",
            TextLocation::from_tuple(loc),
            NodeData::Alternation(nodes),
        )
    }

    fn var<'a>(
        name: &'a str,
        loc: (u32, u32),
        parameters: Vec<Node<'a>>,
    ) -> Node<'a> {
        Node::new(
            name,
            TextLocation::from_tuple(loc),
            NodeData::Variable { parameters },
        )
    }

    fn literal<'a>(
        literal: &'a str,
        loc: (u32, u32),
        regex: &str,
        options: Vec<Node<'a>>,
    ) -> Node<'a> {
        Node::new(
            literal,
            TextLocation::from_tuple(loc),
            NodeData::LiteralTerminal {
                regex: regex.to_string(),
                options,
                embed: None,
            },
        )
    }

    fn regex<'a>(
        contents: &'a str,
        loc: (u32, u32),
        options: Vec<Node<'a>>,
    ) -> Node<'a> {
        Node::new(
            contents,
            TextLocation::from_tuple(loc),
            NodeData::RegexTerminal { options, embed: None },
        )
    }

    fn passive<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new(
            "~",
            TextLocation::from_tuple(loc),
            NodeData::Passive(Box::new(node)),
        )
    }

    fn capture<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new(
            "!",
            TextLocation::from_tuple(loc),
            NodeData::Capture(Box::new(node)),
        )
    }

    fn repetition<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new(
            "*",
            TextLocation::from_tuple(loc),
            NodeData::Repetition(Box::new(node)),
        )
    }

    fn optional<'a>(loc: (u32, u32), node: Node<'a>) -> Node<'a> {
        Node::new(
            "?",
            TextLocation::from_tuple(loc),
            NodeData::Optional(Box::new(node)),
        )
    }

    fn keyarg<'a>(
        key: &'a str,
        key_loc: (u32, u32),
        value: &'a str,
        value_loc: (u32, u32),
    ) -> Node<'a> {
        Node::new(
            key,
            TextLocation::from_tuple(key_loc),
            NodeData::KeywordArgument(Box::new(Node::new(
                value,
                TextLocation::from_tuple(value_loc),
                NodeData::KeywordArgumentValue,
            ))),
        )
    }

    fn arg<'a>(value: &'a str, loc: (u32, u32)) -> Node<'a> {
        Node::new(
            value,
            TextLocation::from_tuple(loc),
            NodeData::PositionalArgument,
        )
    }

    #[test]
    fn parse_headers() {
        assert!(parse("a").is_err());
        assert!(parse("a:").is_err());
        assert!(parse("a :").is_err());
        assert!(parse(" a :").is_err());
        assert!(
            parse("a:b").unwrap().nodes
                == vec!(header("a", (0, 0), "b", (0, 2)),)
        );
        assert!(
            parse("a:\nb: f\t\n\nbar :foo\n").unwrap().nodes
                == vec!(
                    header("a", (0, 0), "", (0, 2)),
                    header("b", (1, 0), " f\t", (1, 2)),
                    header("bar", (3, 0), "foo", (3, 5)),
                )
        );
        assert!(
            parse("a\n:b").unwrap().nodes
                == vec!(header("a", (0, 0), "b", (1, 1)),)
        );
    }

    #[test]
    fn parse_syntax_parameters() {
        assert!(parse("[").is_err());
        assert!(parse("]").is_err());
        assert!(parse("[,]").is_err());
        assert!(
            parse("  [ A ] ").unwrap().nodes
                == vec!(syn_params((0, 2), vec!(var("A", (0, 4), vec!()))))
        );
        assert!(
            parse("[B, `bar`]").unwrap().nodes
                == vec!(syn_params(
                    (0, 0),
                    vec!(
                        var("B", (0, 1), vec!()),
                        literal("bar", (0, 4), "bar", vec!())
                    )
                ))
        );
    }

    #[test]
    fn parse_rules() {
        assert!(parse("a=").is_err());
        assert!(parse("a=a").is_err());
        assert!(parse("a=(").is_err());
        assert!(parse("a=(a").is_err());
        assert!(parse("a=(a;").is_err());
        assert!(
            parse("a=b;").unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(),
                    vec!(),
                    var("b", (0, 2), vec!())
                ),)
        );
        assert!(
            parse("a =b c |d ;b=`a`;").unwrap().nodes
                == vec!(
                    rule(
                        "a",
                        (0, 0),
                        vec!(),
                        vec!(),
                        alt(
                            (0, 3),
                            vec!(
                                concat(
                                    (0, 3),
                                    vec!(
                                        var("b", (0, 3), vec!()),
                                        var("c", (0, 5), vec!()),
                                    )
                                ),
                                var("d", (0, 8), vec!()),
                            )
                        )
                    ),
                    rule(
                        "b",
                        (0, 11),
                        vec!(),
                        vec!(),
                        literal("a", (0, 13), "a", vec!())
                    ),
                )
        );
        assert!(
            parse("a=~(b c)? (d|(e)|f) !g*;").unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(),
                    vec!(),
                    concat(
                        (0, 2),
                        vec!(
                            passive(
                                (0, 2),
                                optional(
                                    (0, 8),
                                    concat(
                                        (0, 4),
                                        vec!(
                                            var("b", (0, 4), vec!()),
                                            var("c", (0, 6), vec!()),
                                        )
                                    )
                                )
                            ),
                            alt(
                                (0, 11),
                                vec!(
                                    var("d", (0, 11), vec!()),
                                    var("e", (0, 14), vec!()),
                                    var("f", (0, 17), vec!()),
                                )
                            ),
                            capture(
                                (0, 20),
                                repetition((0, 22), var("g", (0, 21), vec!()))
                            ),
                        )
                    )
                ),)
        );
        assert!(
            parse("a{b c, 2:d, e}=a;").unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(),
                    vec!(
                        arg("b c", (0, 2)),
                        keyarg(" 2", (0, 6), "d", (0, 9)),
                        arg(" e", (0, 11)),
                    ),
                    var("a", (0, 15), vec!())
                ))
        );
        assert!(
            parse("a[`b`]=a['c'] b[c, d];").unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(literal("b", (0, 2), "b", vec!()),),
                    vec!(),
                    concat(
                        (0, 7),
                        vec!(
                            var("a", (0, 7), vec!(regex("c", (0, 9), vec!()))),
                            var(
                                "b",
                                (0, 14),
                                vec!(
                                    var("c", (0, 16), vec!()),
                                    var("d", (0, 19), vec!())
                                )
                            ),
                        )
                    )
                ))
        );
    }

    #[test]
    fn parse_terminals() {
        assert!(parse("a=`;").is_err());
        assert!(parse("a=\';").is_err());
        assert!(
            parse("a=`\\`;").unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(),
                    vec!(),
                    literal("\\", (0, 2), "\\\\", vec!())
                ))
        );
        assert!(parse(r#"a='\';"#).is_err());
        assert!(
            parse(r#"a='\'';"#).unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(),
                    vec!(),
                    regex("\\'", (0, 2), vec!())
                ))
        );
        assert!(
            parse(r#"a='b(c)'{d, 1 :d e}?;"#).unwrap().nodes
                == vec!(rule(
                    "a",
                    (0, 0),
                    vec!(),
                    vec!(),
                    optional(
                        (0, 19),
                        regex(
                            "b(c)",
                            (0, 2),
                            vec!(
                                arg("d", (0, 9)),
                                keyarg(" 1 ", (0, 11), "d e", (0, 15)),
                            )
                        )
                    )
                ))
        );
    }
}
