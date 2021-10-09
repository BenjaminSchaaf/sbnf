use std::collections::HashMap;

use crate::sbnf::Node;
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
                    result, origin, frame.function.location, frame
                );
                if let Some(variable) = frame.reference {
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
    String { regex: String, literal: String, node: Option<&'a Node<'a>> },
    Rule { name: &'a str, arguments: Vec<Value<'a>>, node: &'a Node<'a> },
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

pub type VarMap<'a> = HashMap<&'a str, Value<'a>>;

#[derive(Debug, Clone)]
pub struct StackFrame<'a> {
    reference: Option<&'a Node<'a>>,
    function: &'a Node<'a>,
    arguments: Vec<Value<'a>>,
}

impl<'a> std::fmt::Display for StackFrame<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.function.text)?;
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

pub const STACK_SIZE_LIMIT: usize = 500;

#[derive(Debug)]
pub struct CallStack<'a> {
    stack: Vec<StackFrame<'a>>,
}

impl<'a> CallStack<'a> {
    pub fn new() -> CallStack<'a> {
        let mut stack = vec![];
        stack.reserve_exact(STACK_SIZE_LIMIT);
        return CallStack { stack };
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
        reference: Option<&'a Node<'a>>,
        function: &'a Node<'a>,
        arguments: Vec<Value<'a>>,
    ) -> bool {
        if self.stack.len() == STACK_SIZE_LIMIT {
            false
        } else {
            self.stack.push(StackFrame { reference, function, arguments });
            true
        }
    }

    pub fn pop(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn run<F>(
        &mut self,
        reference: Option<&'a Node<'a>>,
        function: &'a Node<'a>,
        arguments: Vec<Value<'a>>,
        f: F,
    ) -> bool
    where
        F: Fn(),
    {
        if self.push(reference, function, arguments) {
            f();
            self.pop();
            true
        } else {
            false
        }
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct RuleOptions {
    pub scope: sublime_syntax::Scope,
    pub include_prototype: bool,
    pub capture: bool,
}

pub struct Metadata {
    pub name: String,
    pub file_extensions: Vec<String>,
    pub first_line_match: Option<sublime_syntax::Pattern>,
    pub scope: sublime_syntax::Scope,
    pub scope_postfix: String,
    pub hidden: bool,
}

pub fn parse_scope(metadata: &Metadata, s: &str) -> sublime_syntax::Scope {
    let mut s = parse_top_level_scope(s);
    for scope in &mut s.scopes {
        let postfix = &metadata.scope_postfix;
        if !postfix.is_empty() {
            scope.push('.');
            scope.push_str(postfix);
        }
    }
    s
}
