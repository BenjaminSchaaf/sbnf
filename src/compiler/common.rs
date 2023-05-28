use std::collections::HashMap;

use bumpalo::Bump;
use symbol_table::SymbolTable;

use crate::sbnf::TextLocation;
use crate::sublime_syntax;

pub struct Compiler {
    interner: SymbolTable,
    pub allocator: Bump,
}

pub type Symbol = symbol_table::Symbol;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { interner: SymbolTable::new(), allocator: Bump::new() }
    }

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

pub fn trim_ascii<'a>(s: &'a str) -> &'a str {
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
        write!(f, ")\n")?;

        for (loc, comment) in &self.error.comments {
            write!(f, "{} {}\n", loc.with_source(self.source), comment)?;
        }

        if !self.error.traceback.is_empty() {
            write!(f, "TRACEBACK:\n")?;

            for frame in self.error.traceback.iter().rev() {
                write!(
                    f,
                    "{}:{} - {}\n",
                    self.origin,
                    frame.function_loc,
                    frame.with_compiler(self.compiler)
                )?;

                if let Some(loc) = frame.reference_loc {
                    write!(f, "{}\n", loc.with_source(self.source))?;
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

#[inline]
pub const fn is_valid_variable_name_char(&c:&char) -> bool {
    if c.is_ascii_uppercase() { return true }
    if c.is_ascii_digit()     { return true }
    if c == '_'               { return true }

    if c.is_ascii_lowercase() { return false} // at least ASCII should NOT be lowercase (otherwise `is_valid_ident` will match them)

    return is_valid_name_char(&c)
}
#[inline]
pub const fn is_valid_rule_name_char(&c:&char) -> bool {
    if c.is_ascii_lowercase() { return true }
    if c.is_ascii_digit()     { return true }
    if c == '-'               { return true }

    if c.is_ascii_uppercase() { return false} // at least ASCII should NOT be UPPERCASE (otherwise `is_valid_name_char` will match them)

    return is_valid_name_char(&c)
}
#[inline]
pub const fn is_valid_name_char(&c:&char) -> bool {
    // if c.is_control()          { return false} // not const
    if c.is_ascii_control()    { return false}
    if c.is_ascii_whitespace() { return false}

    if c.is_ascii_digit()      { return true}

    match c {
        // EXclude
        // unicode-space
        '\u{9}'|'\u{20}'|'\u{a0}'|'\u{1680}'|'\u{2000}'..='\u{200a}'|'\u{200b}'..='\u{200f}'|'\u{202a}'..='\u{202f}'|'\u{205f}'|'\u{3000}'
        |'\u{feff}' //bom

        // others
        |'\u{0}'..='\u{1f}' // ASCII C0 control chars
        |'!'..='/' // ASCII punctuation and symbols
        |':'..='@' // ASCII punctuation and symbols
        |'['..='`' // ASCII punctuation and symbols
        |'{'..='~' // ASCII punctuation and symbols
        |'\u{7f}' // ASCII control character
        |'\u{80}'..='\u{9f}' // C1 control chars

        // yaml: c-printable
        // |'\u{A}' // Line feed (LF \n)
        // |'\u{D}' // Carriage Return (CR \r)
        // yaml: Indicator Characters
        // |'-' // (x2D, hyphen) denotes a block sequence entry
        // |'?' // (x3F, question mark) denotes a mapping key
        // |':' // (x3A, colon) denotes a mapping value
        // |',' // (x2C, comma) ends a flow collection entry
        // |'[' // (x5B, left bracket) starts a flow sequence
        // |']' // (x5D, right bracket) ends a flow sequence
        // |'{' // (x7B, left brace) starts a flow mapping
        // |'}' // (x7D, right brace) ends a flow mapping
        // |'#' // (x23, octothorpe, hash, sharp, pound, number sign) denotes a comment
        // |'&' // (x26, ampersand) denotes a node’s anchor property
        // |'*' // (x2A, asterisk) denotes an alias node
        // |'!' // (x21, exclamation) is used for specifying node tags. It is used to denote tag handles used in tag directives and tag properties; to denote local tags; and as the non-specific tag for non-plain scalars
          => false,

        // INclude                      //
        // yaml: c-printable            //
        // 8 bit                        //
        // '\u{9}'                      // Tab (\t)
        // | '\u{A}'                    // Line feed (LF \n)
        // | '\u{D}'                    // Carriage Return (CR \r)
         '\u{20}'     ..='\u{7E}'       // Printable ASCII
        // 16 bit                       //
        // |'\u{85}'                    // Next Line (NEL)
        |'\u{A0}'     ..='\u{D7FF}'     // Basic Multilingual Plane (BMP)
        |'\u{E000}'   ..='\u{FFFD}'     // Additional Unicode Areas
        // |'\u{10000}'  ..='\u{10FFFF}'// 32 bit
        |'\u{10000}'  ..='\u{DFFFF}'    // 32 bit without special purpose and private use planes
          => true,
        _ => false,
    }
}
