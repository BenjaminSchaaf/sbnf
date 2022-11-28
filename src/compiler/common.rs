use std::collections::HashMap;
use std::cell::RefCell;

use indexmap::IndexMap;
use bumpalo::Bump;
use symbol_table::SymbolTable;

use crate::sbnf::TextLocation;
use crate::sublime_syntax;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceReference(pub usize);

pub struct Compiler {
    interner: SymbolTable,
    pub allocator: Bump,
    source_map: RefCell<IndexMap<Option<String>, String>>,
}

pub type Symbol = symbol_table::Symbol;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            interner: SymbolTable::new(),
            allocator: Bump::new(),
            source_map: RefCell::new(IndexMap::new()),
        }
    }

    pub fn get_symbol(&self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    pub fn resolve_symbol(&self, s: Symbol) -> &str {
        self.interner.resolve(s)
    }

    pub fn add_source(&self, name: Option<String>, source: String) -> SourceReference {
        let mut source_map = self.source_map.borrow_mut();

        // Only the first source may be None
        assert!(name.is_some() || source_map.is_empty());

        let (index, old_value) = source_map.insert_full(name, source);
        assert!(old_value.is_none());

        SourceReference(index)
    }

    pub fn get_source<'a>(&'a self, source_reference: SourceReference) -> (Option<&'a str>, &'a str) {
        // The source map is append-only, so it's safe to return references to the interior strings
        let source_map = unsafe { self.source_map.try_borrow_unguarded() }.unwrap();
        let pair = source_map.get_index(source_reference.0).unwrap();
        (pair.0.as_deref(), pair.1)
    }
}

pub type CompileResult<T> = Result<(T, Vec<Error>), (Vec<Error>, Vec<Error>)>;

pub struct CompileOptions<'a> {
    pub name_hint: Option<&'a str>,
    pub arguments: Vec<&'a str>,
    pub debug_contexts: bool,
    pub entry_points: Vec<&'a str>,
    pub import_function: Option<Box<dyn Fn(&str) -> Result<String, String>>>,
}

pub fn trim_ascii<'a>(s: &'a str) -> &'a str {
    s.trim_matches(|c: char| c.is_ascii_whitespace())
}

#[derive(Debug)]
pub struct ErrorComment {
    message: String,
    source: SourceReference,
    location: TextLocation,
}

#[derive(Debug)]
pub struct Error {
    message: String,
    source: SourceReference,
    location: Option<TextLocation>,
    comments: Vec<ErrorComment>,
    traceback: CallStack,
}

impl Error {
    pub fn new_full(
        message: String,
        source: SourceReference,
        location: Option<TextLocation>,
        comments: Vec<ErrorComment>,
        traceback: CallStack
    ) -> Error {
        Error { message, source, location, comments, traceback }
    }

    pub fn new(message: String, source: SourceReference) -> Error {
        Error::new_full(message, source, None, vec![], CallStack::empty())
    }

    pub fn new_str(message: &str, source: SourceReference) -> Error {
        Error::new(message.to_string(), source)
    }

    pub fn location(mut self, location: TextLocation) -> Error {
        self.location = Some(location);
        self
    }

    pub fn optional_location(mut self, location: Option<TextLocation>) -> Error {
        self.location = location;
        self
    }

    pub fn comment(mut self, message: String, source: SourceReference, location: TextLocation) -> Error {
        self.comments.push(ErrorComment { message, source, location });
        self
    }

    pub fn comment_str(self, message: &str, source: SourceReference, location: TextLocation) -> Error {
        self.comment(message.to_string(), source, location)
    }

    pub fn traceback(mut self, stack: CallStack) -> Error {
        self.traceback = stack;
        self
    }

    pub fn with_compiler<'a>(
        &'a self,
        compiler: &'a Compiler,
        error_type: &'a str,
    ) -> ErrorWithCompilerAndSource {
        ErrorWithCompilerAndSource {
            error: self,
            compiler,
            error_type,
        }
    }
}

pub struct ErrorWithCompilerAndSource<'a> {
    error: &'a Error,
    compiler: &'a Compiler,
    error_type: &'a str,
}

impl std::fmt::Display for ErrorWithCompilerAndSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        {
            let (origin, _) = self.compiler.get_source(self.error.source);

            write!(f, "{}: {}", self.error_type, self.error.message)?;

            if origin.is_some() || self.error.location.is_some() {
                write!(f, " (")?;

                if let Some(origin) = origin {
                    write!(f, "{}", origin)?;
                }

                if let Some(loc) = self.error.location {
                    if origin.is_some() {
                        write!(f, ":")?;
                    }

                    write!(f, "{}", loc)?;
                }

                write!(f, ")\n")?;
            }
        }

        for comment in &self.error.comments {
            let (origin, src) = self.compiler.get_source(comment.source);

            if comment.source != self.error.source {
                if let Some(origin) = origin {
                    write!(f, "{}:\n", origin)?;
                }
            }

            write!(f, "{} {}\n", comment.location.with_source(src), comment.message)?;
        }

        if !self.error.traceback.is_empty() {
            write!(f, "TRACEBACK:\n")?;

            for frame in self.error.traceback.iter().rev() {
                let (origin, src) = self.compiler.get_source(frame.source);

                if let Some(origin) = origin {
                    write!(f, "{}:", origin)?;
                }

                write!(
                    f,
                    "{} - {}\n",
                    frame.location,
                    frame.with_compiler(self.compiler))?;

                if let Some(loc) = frame.reference_loc {
                    write!(f, "{}\n", loc.with_source(src))?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String { regex: Symbol, literal: Symbol, source: SourceReference, location: Option<TextLocation> },
    Rule { name: Symbol, arguments: Vec<Value>, source: SourceReference, location: TextLocation },
    Grammar { grammar_source: SourceReference, import_source: SourceReference, import_location: TextLocation },
    // TODO: Use a poison value instead of generating extra errors
    // Poison,
}

impl PartialEq for Value {
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
impl Eq for Value {}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::String { regex, literal, .. } => {
                regex.hash(state);
                literal.hash(state);
            }
            Value::Rule { name, .. } => {
                name.hash(state);
            }
            Value::Grammar { grammar_source, .. } => {
                grammar_source.0.hash(state);
            }
        }
    }
}

impl Value {
    pub fn with_compiler<'a>(
        &'a self,
        compiler: &'a Compiler,
    ) -> ValueWithCompiler {
        ValueWithCompiler { value: self, compiler }
    }
}

pub struct ValueWithCompiler<'a> {
    value: &'a Value,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Display for ValueWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::String { regex, literal, .. } => {
                if regex == literal {
                    write!(f, "'{}'", self.compiler.resolve_symbol(*regex))
                } else {
                    write!(f, "`{}`", self.compiler.resolve_symbol(*literal))
                }
            }
            Value::Rule { name, .. } => {
                write!(f, "{}", self.compiler.resolve_symbol(*name))
            }
            Value::Grammar { grammar_source, .. } => {
                let (origin, _) = self.compiler.get_source(*grammar_source);
                if let Some(origin) = origin {
                    write!(f, "%import[{}]", origin)
                } else {
                    write!(f, "%import[]")
                }
            }
        }
    }
}

pub type VarMap = HashMap<Symbol, Value>;

#[derive(Debug, Clone)]
pub struct StackFrame {
    reference_loc: Option<TextLocation>,
    function: Symbol,
    source: SourceReference,
    location: TextLocation,
    arguments: Vec<Value>,
}

impl StackFrame {
    pub fn with_compiler<'a>(
        &'a self,
        compiler: &'a Compiler,
    ) -> StackFrameWithCompiler {
        StackFrameWithCompiler { stack_frame: self, compiler }
    }
}

pub struct StackFrameWithCompiler<'a> {
    stack_frame: &'a StackFrame,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Display for StackFrameWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.compiler.resolve_symbol(self.stack_frame.function)
        )?;
        if !self.stack_frame.arguments.is_empty() {
            write!(
                f,
                "[{}",
                self.stack_frame.arguments[0].with_compiler(self.compiler)
            )?;
            for arg in &self.stack_frame.arguments[1..] {
                write!(f, ", {}", arg.with_compiler(self.compiler))?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

pub const STACK_SIZE_LIMIT: usize = 500;

#[derive(Debug, Clone)]
pub struct CallStack(Vec<StackFrame>);

impl CallStack {
    pub fn new() -> CallStack {
        let mut stack = vec![];
        stack.reserve_exact(STACK_SIZE_LIMIT);
        return CallStack(stack);
    }

    pub fn empty() -> CallStack {
        CallStack(vec![])
    }

    pub fn push(
        &mut self,
        reference_loc: Option<TextLocation>,
        function: Symbol,
        source: SourceReference,
        location: TextLocation,
        arguments: Vec<Value>,
    ) -> bool {
        if self.0.len() == STACK_SIZE_LIMIT {
            false
        } else {
            self.0.push(StackFrame {
                reference_loc,
                function,
                source,
                location,
                arguments,
            });
            true
        }
    }

    pub fn pop(&mut self) {
        self.0.pop().unwrap();
    }

    pub fn run<F>(
        &mut self,
        reference_loc: Option<TextLocation>,
        function: Symbol,
        source: SourceReference,
        location: TextLocation,
        arguments: Vec<Value>,
        f: F,
    ) -> bool
    where
        F: Fn(),
    {
        if self.push(reference_loc, function, source, location, arguments) {
            f();
            self.pop();
            true
        } else {
            false
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, StackFrame> {
        self.0.iter()
    }
}

#[derive(Debug, Clone)]
pub struct RuleOptions {
    pub scope: sublime_syntax::Scope,
    pub include_prototype: bool,
    pub capture: bool,
}

#[derive(Debug, Clone)]
pub struct Metadata {
    pub name: String,
    pub file_extensions: Vec<String>,
    pub first_line_match: Option<sublime_syntax::Pattern>,
    pub scope: sublime_syntax::Scope,
    pub scope_postfix: String,
    pub hidden: bool,
}

pub fn parse_scope(metadata: &Metadata, s: &str) -> sublime_syntax::Scope {
    let mut s = sublime_syntax::Scope::parse(s);
    for scope in &mut s.scopes {
        let postfix = &metadata.scope_postfix;
        if !postfix.is_empty() {
            scope.push('.');
            scope.push_str(postfix);
        }
    }
    s
}
