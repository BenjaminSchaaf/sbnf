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

            for frame in &self.traceback {
                result = format!(
                    "{}{}:{} - {}\n",
                    result, origin, frame.rule.location, frame
                );
                if let Some(variable) = frame.variable {
                    result.push_str(&variable.location.fmt_source(source));
                }
            }
        }

        result
    }
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    String { regex: &'a str, literal: &'a str, node: Option<&'a Node<'a>> },
    Variable { name: &'a str, node: &'a Node<'a> },
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
            Value::Variable { name, .. } => write!(f, "{}", name),
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
            (
                Value::Variable { name: s, .. },
                Value::Variable { name: o, .. },
            ) => s == o,
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
            Value::Variable { name, .. } => {
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
