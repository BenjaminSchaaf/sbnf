use hashbrown::HashMap;

use bumpalo::Bump;

use crate::sbnf::TextLocation;
use crate::sublime_syntax;
pub use crate::symbol_table::Symbol;
use crate::symbol_table::SymbolTable;

pub struct Compiler {
    interner: SymbolTable,
    pub allocator: Bump,
}

impl Default for Compiler {
    fn default() -> Compiler {
        Compiler { interner: SymbolTable::new(), allocator: Bump::new() }
    }
}

impl Compiler {
    pub fn get_symbol(&self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    pub fn resolve_symbol(&self, s: Symbol) -> &str {
        self.interner.resolve(s)
    }
}

pub struct CompileResult<T> {
    pub result: Result<T, Vec<Error>>,
    pub warnings: Vec<Error>,
}

impl<T> CompileResult<T> {
    pub fn new(value: T, errors: Vec<Error>, warnings: Vec<Error>) -> Self {
        if errors.is_empty() {
            CompileResult { result: Ok(value), warnings }
        } else {
            CompileResult { result: Err(errors), warnings }
        }
    }

    pub fn err(errors: Vec<Error>, warnings: Vec<Error>) -> Self {
        CompileResult { result: Err(errors), warnings }
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

pub fn trim_ascii(s: &str) -> &str {
    s.trim_matches(|c: char| c.is_ascii_whitespace())
}

#[derive(Debug)]
pub struct Error {
    message: String,
    location: Option<TextLocation>,
    comments: Vec<(TextLocation, String)>,
    traceback: CallStack,
}

impl Error {
    pub fn new(
        message: String,
        location: Option<TextLocation>,
        comments: Vec<(TextLocation, String)>,
    ) -> Error {
        Error { message, location, comments, traceback: CallStack::empty() }
    }

    pub fn from_str(
        message: &str,
        location: Option<TextLocation>,
        comments: Vec<(TextLocation, String)>,
    ) -> Error {
        Error::new(message.to_string(), location, comments)
    }

    pub fn with_traceback(mut self, stack: CallStack) -> Error {
        self.traceback = stack;
        self
    }

    pub fn with_compiler_and_source<'a>(
        &'a self,
        compiler: &'a Compiler,
        error_type: &'a str,
        origin: &'a str,
        source: &'a str,
    ) -> ErrorWithCompilerAndSource {
        ErrorWithCompilerAndSource {
            error: self,
            compiler,
            error_type,
            origin,
            source,
        }
    }
}

pub struct ErrorWithCompilerAndSource<'a> {
    error: &'a Error,
    compiler: &'a Compiler,
    error_type: &'a str,
    origin: &'a str,
    source: &'a str,
}

impl std::fmt::Display for ErrorWithCompilerAndSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {} ({}",
            self.error_type, self.error.message, self.origin
        )?;
        if let Some(loc) = self.error.location {
            write!(f, ":{}", loc)?;
        }
        writeln!(f, ")")?;

        for (loc, comment) in &self.error.comments {
            writeln!(f, "{} {}", loc.with_source(self.source), comment)?;
        }

        if !self.error.traceback.is_empty() {
            writeln!(f, "TRACEBACK:")?;

            for frame in self.error.traceback.iter().rev() {
                writeln!(
                    f,
                    "{}:{} - {}",
                    self.origin,
                    frame.function_loc,
                    frame.with_compiler(self.compiler)
                )?;

                if let Some(loc) = frame.reference_loc {
                    writeln!(f, "{}", loc.with_source(self.source))?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String { regex: Symbol, literal: Symbol, location: Option<TextLocation> },
    Rule { name: Symbol, arguments: Vec<Value>, location: TextLocation },
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
        }
    }
}

pub type VarMap = HashMap<Symbol, Value>;

#[derive(Debug, Clone)]
pub struct StackFrame {
    reference_loc: Option<TextLocation>,
    function: Symbol,
    function_loc: TextLocation,
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

impl Default for CallStack {
    fn default() -> CallStack {
        let mut stack = vec![];
        stack.reserve_exact(STACK_SIZE_LIMIT);
        CallStack(stack)
    }
}

impl CallStack {
    pub fn empty() -> CallStack {
        CallStack(vec![])
    }

    pub fn push(
        &mut self,
        reference_loc: Option<TextLocation>,
        function: Symbol,
        function_loc: TextLocation,
        arguments: Vec<Value>,
    ) -> bool {
        if self.0.len() == STACK_SIZE_LIMIT {
            false
        } else {
            self.0.push(StackFrame {
                reference_loc,
                function,
                function_loc,
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
        function_loc: TextLocation,
        arguments: Vec<Value>,
        f: F,
    ) -> bool
    where
        F: Fn(),
    {
        if self.push(reference_loc, function, function_loc, arguments) {
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
    sublime_syntax::Scope::parse_with_postfix(s, &metadata.scope_postfix)
}
