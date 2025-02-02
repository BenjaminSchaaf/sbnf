use crate::TextLocation;
use crate::util::str_from_iterators;
use super::common::{ParseError, LookaheadChars, is_identifier_char, is_whitespace_char};
/// This file implements a parser for the SBNF grammar
use bumpalo::Bump;
use std::str::Chars;

#[derive(Debug)]
pub struct Grammar<'a> {
    pub source: &'a str,
    pub nodes: Vec<&'a Node<'a>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Node<'a> {
    pub text: &'a str,
    pub location: TextLocation,
    pub data: NodeData<'a>,
}

fn fmt_inner_parameters(
    f: &mut std::fmt::Formatter,
    params: &[Node<'_>],
    start: &str,
    end: &str,
) -> std::fmt::Result {
    if !params.is_empty() {
        write!(f, "{}", start)?;
        write!(f, "{:?}", params[0])?;
        for arg in &params[1..] {
            write!(f, ", ")?;
            write!(f, "{:?}", arg)?;
        }
        write!(f, "{}", end)?;
    }
    Ok(())
}

fn fmt_inner_optional(
    param: &Option<&Node<'_>>,
    f: &mut std::fmt::Formatter,
) -> std::fmt::Result {
    if let Some(p) = param {
        write!(f, "{:?}", p)
    } else {
        Ok(())
    }
}

impl<'a> Node<'a> {
    pub fn new(
        text: &'a str,
        location: TextLocation,
        data: NodeData<'a>,
    ) -> Node<'a> {
        Node { text, location, data }
    }

    pub fn get_regex(&'a self) -> &'a str {
        match &self.data {
            NodeData::RegexTerminal { .. } => self.text,
            NodeData::LiteralTerminal { regex, .. } => regex,
            _ => panic!(),
        }
    }

    fn type_name(&self) -> &'static str {
        match &self.data {
            NodeData::Parameters(_) => "pars",
            NodeData::Options(_) => "opts",
            NodeData::Variable { .. } => "var",
            NodeData::Rule { .. } => "rule",
            NodeData::Reference { .. } => "ref",
            NodeData::RegexTerminal { .. } => "rterm",
            NodeData::LiteralTerminal { .. } => "lterm",
            NodeData::Passive(_) => "pass",
            NodeData::Repetition(_) => "rep",
            NodeData::Optional(_) => "opt",
            NodeData::Alternation(_) => "alt",
            NodeData::Concatenation(_) => "cat",
            NodeData::KeywordOption(_) => "kwopt",
            NodeData::PositionalOption => "popt",
            NodeData::KeywordOptionValue => "kopt",
            NodeData::Embed { .. } => "emb",
        }
    }
}

impl std::fmt::Debug for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<")?;

        match &self.data {
            NodeData::Parameters(parameters) => {
                fmt_inner_parameters(f, parameters, "[", "]")?;
            }
            NodeData::Options(options) => {
                fmt_inner_parameters(f, options, "{", "}")?;
            }
            NodeData::Variable { parameters, value } => {
                write!(f, "{}", self.text)?;
                fmt_inner_optional(parameters, f)?;
                write!(f, " = {:?}", value)?;
            }
            NodeData::Rule { parameters, options, node } => {
                write!(f, "{}", self.text)?;
                fmt_inner_optional(parameters, f)?;
                fmt_inner_optional(options, f)?;
                write!(f, " : {:?} ;", node)?;
            }
            NodeData::Reference { parameters, options } => {
                write!(f, "{}", self.text)?;
                fmt_inner_optional(parameters, f)?;
                fmt_inner_optional(options, f)?;
            }
            NodeData::RegexTerminal { options, embed } => {
                write!(f, "'{}'", self.text)?;
                fmt_inner_optional(options, f)?;
                if let Some(embed) = embed {
                    write!(f, " {:?}", embed)?;
                }
            }
            NodeData::LiteralTerminal { options, .. } => {
                write!(f, "`{}`", self.text)?;
                fmt_inner_optional(options, f)?;
            }
            NodeData::Passive(node) => {
                write!(f, "~({:?})", node)?;
            }
            NodeData::Repetition(node) => {
                write!(f, "({:?})*", node)?;
            }
            NodeData::Optional(node) => {
                write!(f, "({:?})?", node)?;
            }
            NodeData::Alternation(nodes) => {
                write!(f, "({:?}", nodes[0])?;
                for node in &nodes[1..] {
                    write!(f, " | {:?}", node)?;
                }
                write!(f, ")")?;
            }
            NodeData::Concatenation(nodes) => {
                write!(f, "({:?}", nodes[0])?;
                for node in &nodes[1..] {
                    write!(f, " {:?}", node)?;
                }
                write!(f, ")")?;
            }
            NodeData::KeywordOption(node) => {
                write!(f, "{}: {:?}", self.text, node)?;
            }
            NodeData::PositionalOption | NodeData::KeywordOptionValue => {
                write!(f, "{}", self.text)?;
            }
            NodeData::Embed { parameters, options } => {
                write!(f, "%{}{:?}{:?}", self.text, parameters, options)?;
            }
        }

        write!(f, ">({}:{:?})", self.type_name(), self.location)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NodeData<'a> {
    // [parameters]
    Parameters(&'a [Node<'a>]),
    // {options}
    Options(&'a [Node<'a>]),
    // Variable[parameters] = Value
    Variable {
        parameters: Option<&'a Node<'a>>,
        value: &'a Node<'a>,
    },
    // Rule[parameters]{options} > node
    Rule {
        parameters: Option<&'a Node<'a>>,
        options: Option<&'a Node<'a>>,
        node: &'a Node<'a>,
    },
    // Reference[parameters]{options}
    Reference {
        parameters: Option<&'a Node<'a>>,
        options: Option<&'a Node<'a>>,
    },
    // "\r\e\g\e\x"
    RegexTerminal {
        options: Option<&'a Node<'a>>,
        embed: Option<&'a Node<'a>>,
    },
    // `literal`
    LiteralTerminal {
        regex: &'a str,
        options: Option<&'a Node<'a>>,
        embed: Option<&'a Node<'a>>,
    },
    // ~a
    Passive(&'a Node<'a>),
    // a*
    Repetition(&'a Node<'a>),
    // a?
    Optional(&'a Node<'a>),
    // a | b
    Alternation(&'a [Node<'a>]),
    // a b
    Concatenation(&'a [Node<'a>]),
    // {positional-option}
    PositionalOption,
    // {keyword: option}
    KeywordOption(&'a Node<'a>),
    KeywordOptionValue,
    // %embed[]{}
    Embed {
        parameters: &'a Node<'a>,
        options: &'a Node<'a>,
    },
}

// Can't use Peekable here, as you can't access the underlying iterator, which
// is required for str_from_iterators. Why‽
struct Parser<'a> {
    source: &'a str,
    allocator: &'a Bump,
    iterator: LookaheadChars<'a>,
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Option<char> {
        self.iterator.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.iterator.next()
    }

    fn char_error(&self, message: String) -> ParseError {
        ParseError::new(self.iterator.location, message)
    }

    fn start_node_collection(&mut self) -> NodeCollector<'a> {
        let (start, location) = self.iterator.start_point();

        NodeCollector { location, start }
    }

    fn alloc(&self, node: Node<'a>) -> &'a Node<'a> {
        self.allocator.alloc(node)
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
            parser.iterator.current.clone(),
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

pub fn parse<'a>(
    source: &'a str,
    allocator: &'a Bump,
) -> Result<Grammar<'a>, ParseError> {
    let result = parse_impl(source, allocator);

    // Ensure the tokenizer and parser match up
    if cfg!(debug_assertions) {
        use super::tokenizer::{Tokenizer, TokenType};

        let tokens: Vec<_> = Tokenizer::new(source).collect();
        assert!(tokens.iter().flat_map(|t| t.value.chars()).collect::<String>() == source);
        assert!(result.is_err() || tokens.iter().all(|t| t.type_ != TokenType::Error));
    }

    result
}

fn parse_impl<'a>(
    source: &'a str,
    allocator: &'a Bump,
) -> Result<Grammar<'a>, ParseError> {
    let mut parser = Parser {
        source,
        allocator,
        iterator: LookaheadChars::new(source),
    };

    let mut nodes = vec![];

    loop {
        skip_whitespace(&mut parser);

        if parser.peek().is_none() {
            break;
        } else {
            nodes.push(&*allocator.alloc(parse_item(&mut parser, 0)?));
        }
    }

    Ok(Grammar { source: parser.source, nodes })
}

fn skip_whitespace(parser: &mut Parser) {
    while let Some(chr) = parser.peek() {
        if is_whitespace_char(chr) {
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

fn check_parse_depth(
    parser: &Parser<'_>,
    depth: usize,
) -> Result<usize, ParseError> {
    const MAXIMUM_PARSER_DEPTH: usize = 512;

    if depth > MAXIMUM_PARSER_DEPTH {
        Err(parser
            .char_error("Parser recursion depth limit reached".to_string()))
    } else {
        Ok(depth + 1)
    }
}

fn parse_item<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let depth = check_parse_depth(parser, depth)?;

    skip_whitespace(parser);

    if parser.peek() == Some('[') {
        return parse_parameters(parser, depth);
    }

    let ident = parse_identifier(parser)?;

    skip_whitespace(parser);

    let parameters = if parser.peek() == Some('[') {
        let params = parse_parameters(parser, depth)?;

        skip_whitespace(parser);

        Some(parser.alloc(params))
    } else {
        None
    };

    let data = match parser.peek() {
        Some('=') => {
            parser.next();

            skip_whitespace(parser);

            let first_char = match parser.peek() {
                Some(c) => c,
                None => {
                    return Err(parser.char_error(
                        "Expected a terminal or variable, got EOF instead"
                            .to_string(),
                    ));
                }
            };

            let value = if first_char == '\'' {
                let regex = parse_regex(parser)?;

                regex.build(NodeData::RegexTerminal {
                    options: None,
                    embed: None,
                })
            } else if first_char == '`' {
                let (text, regex) = parse_literal(parser)?;

                text.build(NodeData::LiteralTerminal {
                    regex,
                    options: None,
                    embed: None,
                })
            } else if is_identifier_char(first_char) {
                let ident = parse_identifier(parser)?;

                skip_whitespace(parser);

                let parameters = if parser.peek() == Some('[') {
                    let node = parse_parameters(parser, depth)?;
                    Some(parser.alloc(node))
                } else {
                    None
                };

                ident.build(NodeData::Reference { parameters, options: None })
            } else {
                return Err(parser.char_error(format!(
                    "Expected a terminal, variable or group, got {:?} instead",
                    first_char
                )));
            };

            NodeData::Variable { parameters, value: parser.alloc(value) }
        }
        Some(_) => {
            let options = if parser.peek() == Some('{') {
                let options = parse_options(parser)?;

                skip_whitespace(parser);

                Some(parser.alloc(options))
            } else {
                None
            };

            match parser.next() {
                Some(':') => {}
                Some(chr) => {
                    return Err(parser.char_error(format!(
                        "Expected the start of a rule ':', got {:?} instead",
                        chr
                    )))
                }
                None => {
                    return Err(parser.char_error(
                        "Expecting the start of a rule ':', got EOF instead"
                            .to_string(),
                    ))
                }
            }

            NodeData::Rule {
                parameters,
                options,
                node: parse_rule(parser, depth)?,
            }
        }
        None => {
            return Err(parser.char_error(
                "Expecting the start of a rule or header but got EOF instead"
                    .to_string(),
            ));
        }
    };

    Ok(ident.build(data))
}

fn parse_parameters<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let depth = check_parse_depth(parser, depth)?;

    let col = parser.start_node_collection();

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
                    options: None,
                    embed: None,
                }));
            }
            Some('`') => {
                let (literal, regex) = parse_literal(parser)?;

                parameters.push(literal.build(NodeData::LiteralTerminal {
                    regex,
                    options: None,
                    embed: None,
                }));
            }
            Some(_) => {
                let variable = parse_identifier(parser)?;

                skip_whitespace(parser);

                let params = if parser.peek() == Some('[') {
                    let node = parse_parameters(parser, depth)?;
                    Some(parser.alloc(node))
                } else {
                    None
                };

                parameters.push(variable.build(NodeData::Reference {
                    parameters: params,
                    options: None,
                }));
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

    Ok(col.build_from_text(
        "",
        NodeData::Parameters(parser.allocator.alloc_slice_clone(&parameters)),
    ))
}

fn parse_options<'a>(parser: &mut Parser<'a>) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

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

    Ok(col.build_from_text(
        "",
        NodeData::Options(parser.allocator.alloc_slice_clone(&arguments)),
    ))
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
                    col.build_from_parser(parser, NodeData::PositionalOption)
                );
            }
            Some(':') => {
                let node = col.end_from_parser(parser);

                parser.next();

                let kwarg = parse_kwarg_value(parser)?;

                return Ok(
                    node.build(NodeData::KeywordOption(parser.alloc(kwarg)))
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
                return Ok(
                    col.build_from_parser(parser, NodeData::KeywordOptionValue)
                );
            }
            Some(_) => {}
        }
        parser.next();
    }
}

fn parse_rule<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<&'a Node<'a>, ParseError> {
    let node = parse_rule_alternation(parser, depth)?;

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

    Ok(parser.alloc(node))
}

fn parse_rule_alternation<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

    let first_element = parse_rule_concatenation(parser, depth)?;

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
        elements.push(parse_rule_concatenation(parser, depth)?);

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

    Ok(col.build_from_text(
        "",
        NodeData::Alternation(parser.allocator.alloc_slice_clone(&elements)),
    ))
}

fn parse_rule_concatenation<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let col = parser.start_node_collection();

    let first_element = parse_rule_element(parser, depth)?;

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
        elements.push(parse_rule_element(parser, depth)?);

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

    Ok(col.build_from_text(
        "",
        NodeData::Concatenation(parser.allocator.alloc_slice_clone(&elements)),
    ))
}

fn parse_rule_element<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let depth = check_parse_depth(parser, depth)?;

    skip_whitespace(parser);

    let col = parser.start_node_collection();

    if let Some(chr) = parser.peek() {
        if chr == '~' {
            parser.next();

            let node = col.end_from_parser(parser);

            let contents = parse_rule_element_contents(parser, depth)?;
            Ok(node.build(NodeData::Passive(parser.alloc(contents))))
        } else {
            parse_rule_element_contents(parser, depth)
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
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    skip_whitespace(parser);

    let element: Node<'a>;

    if let Some(first_char) = parser.peek() {
        if first_char == '(' {
            parser.next();

            element = parse_rule_alternation(parser, depth)?;

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
            element = parse_regex_terminal(parser, depth)?;
        } else if first_char == '`' {
            element = parse_literal_terminal(parser, depth)?;
        } else if is_identifier_char(first_char) {
            let ident = parse_identifier(parser)?;

            skip_whitespace(parser);

            let parameters = if parser.peek() == Some('[') {
                let node = parse_parameters(parser, depth)?;
                Some(parser.alloc(node))
            } else {
                None
            };

            skip_whitespace(parser);

            let options = if parser.peek() == Some('{') {
                let node = parse_options(parser)?;
                Some(parser.alloc(node))
            } else {
                None
            };

            element = ident.build(NodeData::Reference { parameters, options });
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
                NodeData::Repetition(parser.alloc(element)),
            )
        } else if chr == '?' {
            parser.next();

            col.build_from_parser(
                parser,
                NodeData::Optional(parser.alloc(element)),
            )
        } else {
            element
        }
    } else {
        element
    })
}

fn parse_embed<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let first = parser.next().unwrap();
    assert!(first == '%');

    skip_whitespace(parser);

    let word = parse_identifier(parser)?;

    let parameters = parse_parameters(parser, depth)?;

    let options = parse_options(parser)?;

    Ok(word.build(NodeData::Embed {
        parameters: parser.alloc(parameters),
        options: parser.alloc(options),
    }))
}

fn parse_regex<'a>(
    parser: &mut Parser<'a>,
) -> Result<CollectedNode<'a>, ParseError> {
    let col = parser.start_node_collection();

    // Assume we've parsed the first character
    let first = parser.next().unwrap();
    assert!(first == '\'');

    let start = parser.iterator.current.clone();

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
        } else if chr == '\'' {
            break;
        } else if chr == '\\' {
            has_escape = true;
        }

        parser.next();
    }

    let end = parser.iterator.current.clone();

    let end_chr = parser.next().unwrap();
    assert!(end_chr == '\''); // sanity

    Ok(col.end_from_iterators(parser, start, end))
}

fn parse_regex_terminal<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let regex = parse_regex(parser)?;

    skip_whitespace(parser);

    let options = if parser.peek() == Some('{') {
        let node = parse_options(parser)?;
        Some(parser.alloc(node))
    } else {
        None
    };

    skip_whitespace(parser);

    let embed = if parser.peek() == Some('%') {
        let node = parse_embed(parser, depth)?;
        Some(parser.alloc(node))
    } else {
        None
    };

    Ok(regex.build(NodeData::RegexTerminal { options, embed }))
}

fn parse_literal<'a>(
    parser: &mut Parser<'a>,
) -> Result<(CollectedNode<'a>, &'a str), ParseError> {
    let col = parser.start_node_collection();

    // Assume we've parsed the first character
    let first = parser.next().unwrap();
    assert!(first == '`');

    let start = parser.iterator.current.clone();

    while parser.peek().unwrap_or('`') != '`' {
        parser.next();
    }

    let end = parser.iterator.current.clone();

    let end_chr = parser.next().ok_or(parser.char_error(
        "Expected the end of the terminal '`', but got EOF instead".to_string(),
    ))?;
    assert!(end_chr == '`'); // sanity

    let node = col.end_from_iterators(parser, start, end);

    // Convert the literal to a regex now. This makes further compilation much
    // simpler.
    let regex = literal_to_regex(node.text, parser.allocator);

    Ok((node, regex))
}

fn parse_literal_terminal<'a>(
    parser: &mut Parser<'a>,
    depth: usize,
) -> Result<Node<'a>, ParseError> {
    let (text, regex) = parse_literal(parser)?;

    skip_whitespace(parser);

    let options = if parser.peek() == Some('{') {
        let node = parse_options(parser)?;
        Some(parser.alloc(node))
    } else {
        None
    };

    skip_whitespace(parser);

    let embed = if parser.peek() == Some('%') {
        let node = parse_embed(parser, depth)?;
        Some(parser.alloc(node))
    } else {
        None
    };

    Ok(text.build(NodeData::LiteralTerminal { regex, options, embed }))
}

fn literal_to_regex<'a>(literal: &'a str, allocator: &'a Bump) -> &'a str {
    const ESCAPE_CHARACTERS: &str = "^$\\'.*+?()[]{}|";

    if !literal.chars().any(|c| ESCAPE_CHARACTERS.find(c).is_some()) {
        return literal;
    }

    let mut result = String::new();
    for chr in literal.chars() {
        if ESCAPE_CHARACTERS.find(chr).is_some() {
            result.push('\\');
        }

        result.push(chr);
    }
    allocator.alloc_str(&result)
}

#[cfg(test)]
mod tests {
    use crate::TextLocation;
    use crate::sbnf::*;
    use bumpalo::Bump;

    #[test]
    fn parse_empty() {
        let a = Bump::new();
        assert!(parse("", &a).unwrap().nodes.is_empty());
    }

    fn parameters<'a>(loc: (u32, u32), nodes: &'a [Node<'a>]) -> Node<'a> {
        Node::new(
            "",
            TextLocation::from_tuple(loc),
            NodeData::Parameters(nodes),
        )
    }

    fn options<'a>(loc: (u32, u32), nodes: &'a [Node<'a>]) -> Node<'a> {
        Node::new("", TextLocation::from_tuple(loc), NodeData::Options(nodes))
    }

    fn variable<'a>(
        name: &'a str,
        loc: (u32, u32),
        parameters: Option<&'a Node<'a>>,
        value: &'a Node<'a>,
    ) -> Node<'a> {
        Node::new(
            name,
            TextLocation::from_tuple(loc),
            NodeData::Variable { parameters, value },
        )
    }

    fn rule<'a>(
        name: &'a str,
        loc: (u32, u32),
        parameters: Option<&'a Node<'a>>,
        options: Option<&'a Node<'a>>,
        node: &'a Node<'a>,
    ) -> Node<'a> {
        Node::new(
            name,
            TextLocation::from_tuple(loc),
            NodeData::Rule { parameters, options, node },
        )
    }

    fn refr<'a>(
        name: &'a str,
        loc: (u32, u32),
        parameters: Option<&'a Node<'a>>,
        options: Option<&'a Node<'a>>,
    ) -> Node<'a> {
        Node::new(
            name,
            TextLocation::from_tuple(loc),
            NodeData::Reference { parameters, options },
        )
    }

    fn regex<'a>(
        contents: &'a str,
        loc: (u32, u32),
        options: Option<&'a Node<'a>>,
    ) -> Node<'a> {
        Node::new(
            contents,
            TextLocation::from_tuple(loc),
            NodeData::RegexTerminal { options, embed: None },
        )
    }

    fn literal<'a>(
        literal: &'a str,
        loc: (u32, u32),
        regex: &'a str,
        options: Option<&'a Node<'a>>,
    ) -> Node<'a> {
        Node::new(
            literal,
            TextLocation::from_tuple(loc),
            NodeData::LiteralTerminal { regex, options, embed: None },
        )
    }

    fn passive<'a>(loc: (u32, u32), node: &'a Node<'a>) -> Node<'a> {
        Node::new("~", TextLocation::from_tuple(loc), NodeData::Passive(node))
    }

    fn repetition<'a>(loc: (u32, u32), node: &'a Node<'a>) -> Node<'a> {
        Node::new(
            "*",
            TextLocation::from_tuple(loc),
            NodeData::Repetition(node),
        )
    }

    fn optional<'a>(loc: (u32, u32), node: &'a Node<'a>) -> Node<'a> {
        Node::new("?", TextLocation::from_tuple(loc), NodeData::Optional(node))
    }

    fn alt<'a>(loc: (u32, u32), nodes: &'a [Node<'a>]) -> Node<'a> {
        Node::new(
            "",
            TextLocation::from_tuple(loc),
            NodeData::Alternation(nodes),
        )
    }

    fn concat<'a>(loc: (u32, u32), nodes: &'a [Node<'a>]) -> Node<'a> {
        Node::new(
            "",
            TextLocation::from_tuple(loc),
            NodeData::Concatenation(nodes),
        )
    }

    fn arg(value: &str, loc: (u32, u32)) -> Node<'_> {
        Node::new(
            value,
            TextLocation::from_tuple(loc),
            NodeData::PositionalOption,
        )
    }

    fn keyarg<'a>(
        key: &'a str,
        key_loc: (u32, u32),
        opt: &'a Node<'a>,
    ) -> Node<'a> {
        Node::new(
            key,
            TextLocation::from_tuple(key_loc),
            NodeData::KeywordOption(opt),
        )
    }

    fn keyopt(value: &str, value_loc: (u32, u32)) -> Node {
        Node::new(
            value,
            TextLocation::from_tuple(value_loc),
            NodeData::KeywordOptionValue,
        )
    }

    #[test]
    fn parse_variables() {
        let a = Bump::new();
        assert!(parse("a", &a).is_err());
        assert!(parse("a=", &a).is_err());
        assert!(parse("a =", &a).is_err());
        assert!(parse(" a =", &a).is_err());
        assert!(
            parse("a=b", &a).unwrap().nodes
                == [&variable(
                    "a",
                    (0, 0),
                    None,
                    &refr("b", (0, 2), None, None)
                ),]
        );
        assert!(
            parse("a='b' \nf\t\n\n= `foo`\n", &a).unwrap().nodes
                == [
                    &variable("a", (0, 0), None, &regex("b", (0, 2), None)),
                    &variable(
                        "f",
                        (1, 0),
                        None,
                        &literal("foo", (3, 2), "foo", None)
                    ),
                ]
        );
        assert!(
            parse("a[b, 'c'] = b", &a).unwrap().nodes
                == [&variable(
                    "a",
                    (0, 0),
                    Some(&parameters(
                        (0, 1),
                        &[
                            refr("b", (0, 2), None, None),
                            regex("c", (0, 5), None),
                        ]
                    )),
                    &refr("b", (0, 12), None, None)
                ),]
        );
    }

    #[test]
    fn parse_syntax_parameters() {
        let a = Bump::new();
        assert!(parse("[", &a).is_err());
        assert!(parse("]", &a).is_err());
        assert!(parse("[,]", &a).is_err());
        assert!(
            parse("  [ A ] ", &a).unwrap().nodes
                == [&parameters((0, 2), &[refr("A", (0, 4), None, None)])]
        );
        assert!(
            parse("[B, `bar`]", &a).unwrap().nodes
                == [&parameters(
                    (0, 0),
                    &[
                        refr("B", (0, 1), None, None),
                        literal("bar", (0, 4), "bar", None)
                    ]
                )]
        );
    }

    #[test]
    fn parse_rules() {
        let a = Bump::new();
        assert!(parse("a:", &a).is_err());
        assert!(parse("a:a", &a).is_err());
        assert!(parse("a:(", &a).is_err());
        assert!(parse("a:(a", &a).is_err());
        assert!(parse("a:(a;", &a).is_err());
        assert!(
            parse("a:b;", &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    None,
                    None,
                    &refr("b", (0, 2), None, None)
                ),]
        );
        assert!(
            parse("a :b c |d ;b:`a`;", &a).unwrap().nodes
                == [
                    &rule(
                        "a",
                        (0, 0),
                        None,
                        None,
                        &alt(
                            (0, 3),
                            &[
                                concat(
                                    (0, 3),
                                    &[
                                        refr("b", (0, 3), None, None),
                                        refr("c", (0, 5), None, None),
                                    ]
                                ),
                                refr("d", (0, 8), None, None),
                            ]
                        )
                    ),
                    &rule(
                        "b",
                        (0, 11),
                        None,
                        None,
                        &literal("a", (0, 13), "a", None)
                    ),
                ]
        );
        assert!(
            parse("a:~(b c)? (d|(e)|f) ~g*;", &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    None,
                    None,
                    &concat(
                        (0, 2),
                        &[
                            passive(
                                (0, 2),
                                &optional(
                                    (0, 8),
                                    &concat(
                                        (0, 4),
                                        &[
                                            refr("b", (0, 4), None, None),
                                            refr("c", (0, 6), None, None),
                                        ]
                                    )
                                )
                            ),
                            alt(
                                (0, 11),
                                &[
                                    refr("d", (0, 11), None, None),
                                    refr("e", (0, 14), None, None),
                                    refr("f", (0, 17), None, None),
                                ]
                            ),
                            passive(
                                (0, 20),
                                &repetition(
                                    (0, 22),
                                    &refr("g", (0, 21), None, None)
                                )
                            ),
                        ]
                    )
                ),]
        );
        assert!(
            parse("a{b c, 2:d, e}:a;", &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    None,
                    Some(&options(
                        (0, 1),
                        &[
                            arg("b c", (0, 2)),
                            keyarg(" 2", (0, 6), &keyopt("d", (0, 9))),
                            arg(" e", (0, 11)),
                        ]
                    )),
                    &refr("a", (0, 15), None, None)
                )]
        );
        assert!(
            parse("a[`b`]:a['c'] b[c, d];", &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    Some(&parameters(
                        (0, 1),
                        &[literal("b", (0, 2), "b", None),]
                    )),
                    None,
                    &concat(
                        (0, 7),
                        &[
                            refr(
                                "a",
                                (0, 7),
                                Some(&parameters(
                                    (0, 8),
                                    &[regex("c", (0, 9), None),]
                                )),
                                None
                            ),
                            refr(
                                "b",
                                (0, 14),
                                Some(&parameters(
                                    (0, 15),
                                    &[
                                        refr("c", (0, 16), None, None),
                                        refr("d", (0, 19), None, None),
                                    ]
                                )),
                                None,
                            ),
                        ]
                    )
                )]
        );
    }

    #[test]
    fn parse_terminals() {
        let a = Bump::new();
        assert!(parse("a:`;", &a).is_err());
        assert!(parse("a:\';", &a).is_err());
        assert!(
            parse("a:`\\`;", &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    None,
                    None,
                    &literal("\\", (0, 2), "\\\\", None)
                )]
        );
        assert!(parse(r"a:'\';", &a).is_err());
        assert!(
            parse(r"a:'\'';", &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    None,
                    None,
                    &regex("\\'", (0, 2), None)
                )]
        );
        assert!(
            parse(r#"a:'#';"#, &a).unwrap().nodes
                == [&rule("a", (0, 0), None, None, &regex("#", (0, 2), None))]
        );
        assert!(
            parse(r#"a:'b(c)'{d, 1 :d e}?;"#, &a).unwrap().nodes
                == [&rule(
                    "a",
                    (0, 0),
                    None,
                    None,
                    &optional(
                        (0, 19),
                        &regex(
                            "b(c)",
                            (0, 2),
                            Some(&options(
                                (0, 8),
                                &[
                                    arg("d", (0, 9)),
                                    keyarg(
                                        " 1 ",
                                        (0, 11),
                                        &keyopt("d e", (0, 15))
                                    ),
                                ]
                            ))
                        )
                    )
                )]
        );
    }
}
