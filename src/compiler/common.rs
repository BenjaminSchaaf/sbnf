use std::collections::HashMap;

use crate::sbnf::{is_identifier_char, Node};
use crate::sublime_syntax;

pub struct CompileResult<'a, T> {
    pub result: Result<T, Vec<Error<'a>>>,
    pub warnings: Vec<Error<'a>>,
}

impl<'a, T> CompileResult<'a, T> {
    pub fn new(
        result: Result<T, Vec<Error<'a>>>,
        warnings: Vec<Error<'a>>,
    ) -> CompileResult<'a, T> {
        CompileResult { result, warnings }
    }

    pub fn is_ok(&self) -> bool {
        self.result.is_ok()
    }
    pub fn is_err(&self) -> bool {
        self.result.is_err()
    }
}

pub struct CompileOptions<'a> {
    pub name_hint: Option<&'a str>,
    pub arguments: Vec<&'a str>,
    pub debug_contexts: bool,
    pub entry_points: Vec<&'a str>,
}

pub fn parse_top_level_scope(s: &str) -> sublime_syntax::Scope {
    sublime_syntax::Scope::new(
        s.split_ascii_whitespace()
            .map(|s| s.to_string())
            .collect::<Vec<String>>(),
    )
}

pub fn trim_ascii<'a>(s: &'a str) -> &'a str {
    s.trim_matches(|c: char| c.is_ascii_whitespace())
}

#[derive(Debug)]
pub struct Error<'a> {
    error: String,
    node: Option<&'a Node<'a>>,
    comments: Vec<(&'a Node<'a>, String)>,
    traceback: Vec<StackFrame<'a>>,
}

impl Error<'_> {
    pub fn new<'a>(
        error: String,
        node: &'a Node<'a>,
        comments: Vec<(&'a Node<'a>, String)>,
    ) -> Error<'a> {
        Error { error, node: Some(node), comments, traceback: vec![] }
    }

    pub fn from_str<'a>(
        err: &str,
        node: &'a Node<'a>,
        comments: Vec<(&'a Node<'a>, String)>,
    ) -> Error<'a> {
        Error::new(err.to_string(), node, comments)
    }

    pub fn without_node<'a>(
        error: String,
        comments: Vec<(&'a Node<'a>, String)>,
    ) -> Error<'a> {
        Error { error, node: None, comments, traceback: vec![] }
    }

    pub fn fmt(&self, typ: &str, origin: &str, source: &str) -> String {
        let mut result = if let Some(node) = self.node {
            format!("{}: {} ({}:{})\n", typ, self.error, origin, node.location)
        } else {
            format!("{}: {} ({})\n", typ, self.error, origin)
        };

        for (node, comment) in &self.comments {
            result = format!(
                "{}{} {}\n",
                result,
                node.location.fmt_source(source),
                comment
            );
        }

        if !self.traceback.is_empty() {
            result.push_str("TRACEBACK:\n");

            for frame in self.traceback.iter().rev() {
                result = format!(
                    "{}{}:{} - {}\n",
                    result, origin, frame.rule.location, frame
                );
                if let Some(variable) = frame.variable {
                    result.push_str(&variable.location.fmt_source(source));
                    result.push('\n');
                }
            }
        }

        result
    }
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    String { regex: &'a str, literal: &'a str, node: Option<&'a Node<'a>> },
    Rule { name: &'a str, node: &'a Node<'a> },
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String { regex, literal, .. } => {
                if regex == literal {
                    write!(f, "'{}'", regex)
                } else {
                    write!(f, "`{}`", literal)
                }
            }
            Value::Rule { name, .. } => write!(f, "{}", name),
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Value::String { literal: s, .. },
                Value::String { literal: o, .. },
            ) => s == o,
            (Value::Rule { name: s, .. }, Value::Rule { name: o, .. }) => {
                s == o
            }
            _ => false,
        }
    }
}
impl<'a> Eq for Value<'a> {}

impl<'a> std::hash::Hash for Value<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::String { regex, literal, .. } => {
                regex.hash(state);
                literal.hash(state);
            }
            Value::Rule { name, .. } => {
                name.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame<'a> {
    variable: Option<&'a Node<'a>>,
    rule: &'a Node<'a>,
    arguments: Vec<Value<'a>>,
}

impl<'a> std::fmt::Display for StackFrame<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rule.text)?;
        if !self.arguments.is_empty() {
            write!(f, "[{}", self.arguments[0])?;
            for arg in &self.arguments[1..] {
                write!(f, ", {}", arg)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct CallStack<'a> {
    stack: Vec<StackFrame<'a>>,
}

impl<'a> CallStack<'a> {
    pub fn new() -> CallStack<'a> {
        return CallStack { stack: vec![] };
    }

    pub fn error(
        &self,
        error: String,
        node: &'a Node<'a>,
        comments: Vec<(&'a Node<'a>, String)>,
    ) -> Error<'a> {
        Error {
            error,
            node: Some(node),
            comments,
            traceback: self.stack.clone(),
        }
    }

    pub fn error_from_str(
        &self,
        err: &str,
        node: &'a Node<'a>,
        comments: Vec<(&'a Node<'a>, String)>,
    ) -> Error<'a> {
        self.error(err.to_string(), node, comments)
    }

    pub fn push(
        &mut self,
        variable: Option<&'a Node<'a>>,
        rule: &'a Node<'a>,
        arguments: Vec<Value<'a>>,
    ) {
        self.stack.push(StackFrame { variable, rule, arguments });
    }

    pub fn pop(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }
}

#[derive(Debug, Clone)]
pub struct RuleOptions {
    pub scope: sublime_syntax::Scope,
    pub include_prototype: bool,
    pub capture: bool,
}

pub type VarMap<'a> = HashMap<&'a str, Value<'a>>;

pub fn var_maps_get<'a, 'b>(
    maps: &[&'b VarMap<'a>],
    key: &str,
) -> Option<&'b Value<'a>> {
    for var_map in maps {
        if let Some(value) = var_map.get(key) {
            return Some(value);
        }
    }

    None
}

// TODO: Tests
pub fn interpolate_string<'a, 'b>(
    stack: &CallStack<'a>,
    var_maps: &[&'b VarMap<'a>],
    node: &'a Node<'a>,
    string: &'a str,
) -> (String, Vec<Error<'a>>) {
    let mut result = String::new();
    let mut errors = vec![];

    let mut iter = string.chars();

    while let Some(chr) = iter.next() {
        if chr == '\\' {
            match iter.next() {
                Some('#') => {
                    result.push('#');
                }
                Some(next_chr) => {
                    result.push('\\');
                    result.push(next_chr);
                }
                _ => {
                    break;
                }
            }
        } else if chr == '#' {
            match iter.next() {
                Some('[') => {
                    let mut variable = String::new();

                    loop {
                        match iter.next() {
                            Some(']') => {
                                break;
                            }
                            Some(c) => {
                                if is_identifier_char(c) {
                                    variable.push(c);
                                } else {
                                    // TODO: Add node offset to show the error at the character
                                    errors.push(stack.error(
                                        format!("Unexpected character '{}' in string interpolation", c),
                                        node,
                                        vec!()));
                                    return (result, errors);
                                }
                            }
                            None => {
                                errors.push(stack.error_from_str(
                                    "Expected ']' before the end of the string to end string interpolation",
                                    node,
                                    vec!()));
                                return (result, errors);
                            }
                        }
                    }

                    if let Some(value) = var_maps_get(&var_maps, &variable) {
                        match value {
                            Value::Rule { node: rule_node, .. } => {
                                errors.push(stack.error_from_str(
                                    "Can't interpolate rule",
                                    node,
                                    vec![
                                        (
                                            node,
                                            format!(
                                                "{} must refer to a string",
                                                variable
                                            ),
                                        ),
                                        (
                                            rule_node,
                                            format!(
                                                "{} is {}",
                                                variable, value
                                            ),
                                        ),
                                    ],
                                ));
                            }
                            Value::String { regex, .. } => {
                                result.push_str(regex);
                            }
                        }
                    } else {
                        errors.push(stack.error_from_str(
                            "Variable in string interpolation not found",
                            node,
                            vec!(
                                (node, format!("{} must be defined as an argument to the rule", variable)),
                            )));
                    }
                }
                Some(next_chr) => {
                    result.push('#');
                    result.push(next_chr);
                }
                _ => {
                    break;
                }
            }
        } else {
            result.push(chr);
        }
    }

    (result, errors)
}
