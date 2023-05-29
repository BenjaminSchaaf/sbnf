use std::collections::{HashMap, HashSet};

use super::collector::{Collection, Definition, DefinitionKind, DefinitionMap};
use super::common::{
    is_valid_rule_name_char, is_valid_variable_name_char, parse_scope,
    trim_ascii, CallStack, CompileOptions, CompileResult, Compiler, Error,
    Metadata, RuleOptions, Symbol, Value, VarMap,
};
use crate::sbnf::{is_identifier_char, Node, NodeData, TextLocation};
use crate::sublime_syntax;

#[derive(Debug, Clone)]
pub struct Interpreted<'a> {
    pub rules: HashMap<Key, Rule<'a>>,
    pub entry_points: Vec<Key>,
    pub metadata: Metadata,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Key {
    pub name: Symbol,
    pub arguments: Vec<Value>,
}

impl Key {
    pub fn with_compiler<'a>(
        &'a self,
        compiler: &'a Compiler,
    ) -> KeyWithCompiler {
        KeyWithCompiler { key: self, compiler }
    }
}

pub struct KeyWithCompiler<'a> {
    key: &'a Key,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Debug for KeyWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.compiler.resolve_symbol(self.key.name))?;
        if !self.key.arguments.is_empty() {
            write!(
                f,
                "[{}",
                self.key.arguments[0].with_compiler(self.compiler)
            )?;
            for arg in &self.key.arguments[1..] {
                write!(f, ", {}", arg.with_compiler(self.compiler))?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl<'a> std::fmt::Display for KeyWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Rule<'a> {
    pub options: RuleOptions,
    pub expression: &'a Expression<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TerminalOptions {
    pub scope: sublime_syntax::Scope,
    pub captures: HashMap<u16, sublime_syntax::Scope>,
    pub embed: TerminalEmbed,
}

impl TerminalOptions {
    pub fn new() -> TerminalOptions {
        TerminalOptions {
            scope: sublime_syntax::Scope::empty(),
            captures: HashMap::new(),
            embed: TerminalEmbed::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TerminalEmbed {
    Embed {
        embed: String,
        embed_scope: sublime_syntax::Scope,
        escape: String,
        escape_captures: HashMap<u16, sublime_syntax::Scope>,
    },
    Include {
        context: String,
        prototype: Key,
    },
    None,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Variable { key: Key, location: TextLocation },
    Terminal { regex: Symbol, options: TerminalOptions, location: TextLocation },
    Passive { expression: &'a Expression<'a>, location: TextLocation },
    Repetition { expression: &'a Expression<'a>, location: TextLocation },
    Optional { expression: &'a Expression<'a>, location: TextLocation },
    Alternation { expressions: &'a [Expression<'a>], location: TextLocation },
    Concatenation { expressions: &'a [Expression<'a>], location: TextLocation },
}

impl PartialEq for Expression<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Expression::Variable { key, .. },
                Expression::Variable { key: k, .. },
            ) => key == k,
            (
                Expression::Terminal { regex, options, .. },
                Expression::Terminal { regex: r, options: o, .. },
            ) => regex == r && options == o,
            (
                Expression::Passive { expression, .. },
                Expression::Passive { expression: e, .. },
            )
            | (
                Expression::Repetition { expression, .. },
                Expression::Repetition { expression: e, .. },
            )
            | (
                Expression::Optional { expression, .. },
                Expression::Optional { expression: e, .. },
            ) => expression == e,
            (
                Expression::Alternation { expressions, .. },
                Expression::Alternation { expressions: e, .. },
            )
            | (
                Expression::Concatenation { expressions, .. },
                Expression::Concatenation { expressions: e, .. },
            ) => expressions == e,
            _ => false,
        }
    }
}
impl Eq for Expression<'_> {}

impl std::hash::Hash for Expression<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Expression::Variable { key, .. } => key.hash(state),
            Expression::Terminal { regex, .. } => regex.hash(state),
            Expression::Passive { expression, .. } => {
                1.hash(state);
                expression.hash(state);
            }
            Expression::Repetition { expression, .. } => {
                2.hash(state);
                expression.hash(state);
            }
            Expression::Optional { expression, .. } => {
                3.hash(state);
                expression.hash(state);
            }
            Expression::Alternation { expressions, .. } => {
                4.hash(state);
                expressions.hash(state);
            }
            Expression::Concatenation { expressions, .. } => {
                5.hash(state);
                expressions.hash(state);
            }
        }
    }
}

impl<'a> Expression<'a> {
    pub fn with_compiler(
        &'a self,
        compiler: &'a Compiler,
    ) -> ExpressionWithCompiler {
        ExpressionWithCompiler { expression: self, compiler }
    }
}

pub struct ExpressionWithCompiler<'a> {
    expression: &'a Expression<'a>,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Debug for ExpressionWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.expression {
            Expression::Variable { key, .. } => {
                write!(f, "{}", key.with_compiler(self.compiler))
            }
            Expression::Terminal { regex, .. } => {
                write!(f, "'{}'", self.compiler.resolve_symbol(*regex))
            }
            Expression::Passive { expression, .. } => {
                write!(f, "~{:?}", expression.with_compiler(self.compiler))
            }
            Expression::Repetition { expression, .. } => {
                write!(f, "{:?}*", expression.with_compiler(self.compiler))
            }
            Expression::Optional { expression, .. } => {
                write!(f, "{:?}?", expression.with_compiler(self.compiler))
            }
            Expression::Alternation { expressions, .. } => {
                write!(
                    f,
                    "({:?}",
                    expressions[0].with_compiler(self.compiler)
                )?;
                for expression in &expressions[1..] {
                    write!(
                        f,
                        " | {:?}",
                        expression.with_compiler(self.compiler)
                    )?;
                }
                write!(f, ")")
            }
            Expression::Concatenation { expressions, .. } => {
                write!(
                    f,
                    "({:?}",
                    expressions[0].with_compiler(self.compiler)
                )?;
                for expression in &expressions[1..] {
                    write!(
                        f,
                        " {:?}",
                        expression.with_compiler(self.compiler)
                    )?;
                }
                write!(f, ")")
            }
        }
    }
}

struct State<'a> {
    compiler: &'a Compiler,
    options: &'a CompileOptions<'a>,
    seen_definitions: HashSet<Key>,
    variables: HashMap<Key, Option<Value>>,
    rules: HashMap<Key, Rule<'a>>,
    stack: CallStack,
    errors: Vec<Error>,
    warnings: Vec<Error>,
}

struct MetaState<'a, 'b> {
    collection: &'b DefinitionMap<'a>,
    metadata: &'b Metadata,
}

pub fn interpret<'a>(
    compiler: &'a Compiler,
    options: &'a CompileOptions<'a>,
    collection: Collection<'a>,
) -> CompileResult<Interpreted<'a>> {
    let mut state = State {
        compiler,
        options,
        variables: HashMap::new(),
        seen_definitions: HashSet::new(),
        rules: HashMap::new(),
        stack: CallStack::new(),
        errors: vec![],
        warnings: vec![],
    };

    for (name, value) in collection.variables {
        state.variables.insert(Key { name, arguments: vec![] }, Some(value));
    }

    let definitions = &collection.definitions;

    let metadata = collect_metadata(&mut state, &definitions);

    let mut entry_points = vec![];

    {
        let meta_state =
            MetaState { collection: definitions, metadata: &metadata };

        for entry_point in &options.entry_points {
            let name = state.compiler.get_symbol(entry_point);
            if meta_state.collection.get_key_value(&(name, 0)).is_some() {
                let key = Key { name, arguments: vec![] };

                assert!(state.stack.is_empty());
                interpret_rule(&mut state, &meta_state, None, &key);

                entry_points.push(key);
            }
        }
    }

    CompileResult::new(
        Interpreted { rules: state.rules, entry_points, metadata },
        state.errors,
        state.warnings,
    )
}

fn collect_metadata<'a, 'b>(
    state: &'b mut State<'a>,
    collection: &DefinitionMap<'a>,
) -> Metadata {
    let name = interpret_metadata_variable(state, collection, "NAME", true)
        .map(|s| s.1.to_string())
        .or_else(|| state.options.name_hint.map(|s| trim_ascii(s).to_string()));

    // A name is required, either from a variable or the name hint
    if name.is_none() {
        state.errors.push(Error::from_str(
            "No syntax name provided. Use a 'NAME' variable to specify the name of the syntax",
            None,
            vec!()));
    }

    let file_extensions =
        interpret_metadata_variable(state, collection, "EXTENSIONS", true);

    let first_line_match =
        interpret_metadata_variable(state, collection, "FIRST_LINE", false);

    let name_ref = name.as_ref();

    let scope = interpret_metadata_variable(state, collection, "SCOPE", true)
        .map_or_else(
            || {
                sublime_syntax::Scope::new(vec![format!(
                    "source.{}",
                    name_ref.map_or_else(
                        || "".to_string(),
                        |s| s.to_ascii_lowercase()
                    ),
                )])
            },
            |s| sublime_syntax::Scope::parse(&s.1),
        );

    let scope_postfix =
        interpret_metadata_variable(state, collection, "SCOPE_POSTFIX", true)
            .map_or_else(
                || {
                    name_ref.map_or_else(
                        || "".to_string(),
                        |n| n.to_ascii_lowercase(),
                    )
                },
                |s| s.1,
            );

    let hidden = if let Some((node, value)) =
        interpret_metadata_variable(state, collection, "HIDDEN", true)
    {
        if value != "false" && value != "true" {
            state.errors.push(Error::from_str(
                "HIDDEN must be either `true` or `false`",
                Some(node.location),
                vec![(node.location, format!("was `{}` instead", value))],
            ));
        }

        value == "true"
    } else {
        false
    };

    Metadata {
        name: name.unwrap_or_else(|| "".to_string()),
        // File extensions are separated by whitespace
        file_extensions: file_extensions.map_or(vec![], |s| {
            s.1.split_ascii_whitespace()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
        }),
        first_line_match: first_line_match
            .map(|s| sublime_syntax::Pattern::new(s.1)),
        scope,
        scope_postfix,
        hidden,
    }
}

fn interpret_metadata_variable<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    name: &'a str,
    is_literal: bool,
) -> Option<(&'a Node<'a>, String)> {
    let symbol = state.compiler.get_symbol(name);

    if let Some(definition) = collection.get(&(symbol, 0)) {
        assert!(definition.kind == DefinitionKind::Variable);

        let overloads = &definition.overloads;

        let key = Key { name: symbol, arguments: vec![] };

        let value = interpret_variable(state, collection, None, key);

        match value {
            Some(Value::String { literal, regex, .. }) => Some((
                overloads[0],
                if is_literal {
                    state.compiler.resolve_symbol(literal).to_string()
                } else {
                    state.compiler.resolve_symbol(regex).to_string()
                },
            )),
            Some(Value::Rule { location, .. }) => {
                state.errors.push(Error::new(
                    format!("'{}' must be a string, not a rule", name),
                    Some(overloads[0].location),
                    vec![
                        (overloads[0].location, "must be a string".to_string()),
                        (location, "was this instead".to_string()),
                    ],
                ));

                None
            }
            None => None,
        }
    } else {
        None
    }
}

fn match_rule<'a, 'b>(
    state: &mut State<'a>,
    collection: &'b DefinitionMap<'a>,
    def_node: &'a Node<'a>,
    arguments: &Vec<Value>,
) -> Option<(&'a Node<'a>, VarMap)> {
    let parameters = match &def_node.data {
        NodeData::Rule { parameters, .. }
        | NodeData::Variable { parameters, .. } => {
            parameters.as_ref().map(|p| p.as_ref())
        }
        // Syntax parameters don't have a rule or variable node, but they also
        // never have parameters.
        _ => None,
    };

    if parameters.is_none() && arguments.is_empty() {
        return Some((def_node, HashMap::new()));
    }

    let parameters =
        if let NodeData::Parameters(params) = &parameters.unwrap().data {
            params
        } else {
            panic!()
        };

    if parameters.len() != arguments.len() {
        return None;
    }

    let mut var_map = HashMap::new();

    for (param, arg) in parameters.iter().zip(arguments.iter()) {
        let value = interpret_value(state, collection, &var_map, param, true);

        if let Some(value) = value {
            if value != *arg {
                return None;
            }
        } else {
            var_map.insert(state.compiler.get_symbol(param.text), arg.clone());
        }
    }

    Some((def_node, var_map))
}

fn resolve_definition<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    reference_loc: Option<TextLocation>,
    key: &Key,
) -> Option<(DefinitionKind, &'a Node<'a>, VarMap)> {
    if let Some(Definition { kind, overloads }) =
        collection.get(&(key.name, key.arguments.len() as u8))
    {
        let mut matching = overloads
            .iter()
            .filter_map(|r| match_rule(state, collection, r, &key.arguments));

        if let Some((node, var_map)) = matching.next() {
            let remaining = matching.collect::<Vec<_>>();

            if !remaining.is_empty() {
                let mut comments = vec![(
                    node.location,
                    "This matches the arguments".to_string(),
                )];
                for (other_node, _) in remaining {
                    comments.push((
                        other_node.location,
                        "This also matches the arguments".to_string(),
                    ));
                }
                if let Some(loc) = reference_loc {
                    comments.push((loc, "Instantiated from here".to_string()));
                }
                state.errors.push(
                    Error::new(
                        format!(
                            "Ambiguous instantiation for {}",
                            key.with_compiler(state.compiler)
                        ),
                        Some(node.location),
                        comments,
                    )
                    .with_traceback(state.stack.clone()),
                );
                return None;
            }

            Some((*kind, node, var_map))
        } else {
            let mut comments: Vec<(TextLocation, String)> = vec![];
            for node in overloads {
                comments.push((node.location, "Does not match".to_string()));
            }
            if let Some(loc) = reference_loc {
                comments.push((loc, "Instantiated from here".to_string()));
            }
            state.errors.push(
                Error::new(
                    format!(
                        "No matching instantiation for {}",
                        key.with_compiler(state.compiler)
                    ),
                    reference_loc.or(Some(overloads[0].location)),
                    comments,
                )
                .with_traceback(state.stack.clone()),
            );

            None
        }
    } else {
        let mut msg =
            if is_variable_name(state.compiler.resolve_symbol(key.name)) {
                format!(
                    "Could not find variable {}",
                    state.compiler.resolve_symbol(key.name)
                )
            } else if is_rule_name(state.compiler.resolve_symbol(key.name)) {
                format!(
                    "Could not find rule {}",
                    state.compiler.resolve_symbol(key.name)
                )
            } else {
                format!(
                    "Could not find {}",
                    state.compiler.resolve_symbol(key.name)
                )
            };

        if key.arguments.len() > 0 {
            msg.push_str(&format!(" with {} arguments", key.arguments.len()));
        }

        state.errors.push(
            Error::new(
                msg,
                reference_loc,
                vec![(reference_loc.unwrap(), "Does not exist".to_string())],
            )
            .with_traceback(state.stack.clone()),
        );

        None
    }
}

fn is_variable_name(name: &str) -> bool {
    return name.chars().all(|c| is_valid_variable_name_char(&c));
}

fn is_rule_name(name: &str) -> bool {
    return name.chars().all(|c| is_valid_rule_name_char(&c));
}

fn interpret_variable<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    reference_loc: Option<TextLocation>,
    key: Key,
) -> Option<Value> {
    if state.variables.contains_key(&key) {
        return state.variables[&key].clone();
    }

    if let Some((kind, node, var_map)) =
        resolve_definition(state, collection, reference_loc, &key)
    {
        assert!(kind == DefinitionKind::Variable);

        // Detect recursive definitions
        if state.seen_definitions.contains(&key) {
            state.errors.push(
                Error::new(
                    format!(
                        "Variable {} is defined in terms of itself",
                        state.compiler.resolve_symbol(key.name)
                    ),
                    Some(node.location),
                    vec![],
                )
                .with_traceback(state.stack.clone()),
            );
            return None;
        }

        let is_new_key = state.seen_definitions.insert(key.clone());
        assert!(is_new_key);

        let symbol = state.compiler.get_symbol(node.text);

        if !state.stack.push(
            reference_loc,
            symbol,
            node.location,
            key.arguments.clone(),
        ) {
            state.errors.push(
                Error::from_str(
                    "Recursion depth limit reached",
                    reference_loc, // Only none when the stack size is 1
                    vec![],
                )
                .with_traceback(state.stack.clone()),
            );
            return None;
        }

        let value_node = match &node.data {
            NodeData::Variable { value, .. } => value,
            _ => panic!(),
        };

        let value =
            interpret_value(state, collection, &var_map, value_node, false);
        state.variables.insert(key, value.clone());

        state.stack.pop();

        value
    } else {
        None
    }
}

fn interpret_rule<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    reference_loc: Option<TextLocation>,
    key: &Key,
) {
    // Nothing to do if the key has been seen before
    if state.seen_definitions.contains(key) {
        return;
    }

    if let Some((kind, node, var_map)) =
        resolve_definition(state, &meta_state.collection, reference_loc, key)
    {
        assert!(kind == DefinitionKind::Rule);

        let is_new_key = state.seen_definitions.insert(key.clone());
        assert!(is_new_key);

        let (expression_node, options_node) =
            if let NodeData::Rule { node, options, .. } = &node.data {
                (node, options)
            } else {
                panic!()
            };

        let symbol = state.compiler.get_symbol(node.text);

        if !state.stack.push(
            reference_loc,
            symbol,
            node.location,
            key.arguments.clone(),
        ) {
            state.errors.push(
                Error::from_str(
                    "Recursion depth limit reached",
                    reference_loc, // Only none when the stack size is 1
                    vec![],
                )
                .with_traceback(state.stack.clone()),
            );
            return;
        }

        let options = parse_rule_options(
            state,
            meta_state,
            &var_map,
            key.name,
            options_node.as_ref().map(|o| o.as_ref()),
        );

        let expression =
            interpret_expression(state, meta_state, &var_map, expression_node);

        if let Some(expression) = expression {
            let expression = state.compiler.allocator.alloc(expression);

            let is_new_rule =
                state.rules.insert(key.clone(), Rule { options, expression });
            assert!(is_new_rule.is_none());
        }

        state.stack.pop();
    }
}

fn interpret_value<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    var_map: &VarMap,
    node: &'a Node<'a>,
    is_parameter: bool,
) -> Option<Value> {
    match &node.data {
        NodeData::RegexTerminal { options, embed } => {
            assert!(options.is_none());
            assert!(embed.is_none());

            let regex = interpolate_string(
                state,
                collection,
                var_map,
                node.location,
                node.text,
            );

            let regex = state.compiler.get_symbol(&regex);
            Some(Value::String {
                regex,
                literal: regex,
                location: Some(node.location),
            })
        }
        NodeData::LiteralTerminal { regex, options, embed } => {
            assert!(options.is_none());
            assert!(embed.is_none());
            Some(Value::String {
                regex: state.compiler.get_symbol(regex),
                literal: state.compiler.get_symbol(node.text),
                location: Some(node.location),
            })
        }
        NodeData::Reference { parameters, .. } => {
            let arguments = if let Some(param_node) = parameters {
                let parameters = match &param_node.data {
                    NodeData::Parameters(params) => params,
                    _ => panic!(),
                };

                let mut arguments = vec![];
                for p in parameters {
                    if let Some(v) =
                        interpret_value(state, collection, var_map, p, false)
                    {
                        arguments.push(v);
                    }
                }

                if arguments.len() != parameters.len() {
                    return None;
                }

                arguments
            } else {
                vec![]
            };

            let name = state.compiler.get_symbol(node.text);

            if arguments.is_empty() {
                if let Some(value) = var_map.get(&name) {
                    return Some(value.clone());
                }
            }

            // Don't try to resolve the definition if we're a parameter and know
            // it doesn't exist
            if is_parameter
                && arguments.is_empty()
                && !collection.contains_key(&(name, 0))
            {
                return None;
            }

            let key = Key { name, arguments };

            if let Some((kind, ref_node, _)) =
                resolve_definition(state, collection, Some(node.location), &key)
            {
                match kind {
                    DefinitionKind::Variable => interpret_variable(
                        state,
                        collection,
                        Some(node.location),
                        key,
                    ),
                    DefinitionKind::Rule => Some(Value::Rule {
                        name: key.name,
                        arguments: key.arguments,
                        location: ref_node.location,
                    }),
                }
            } else {
                None
            }
        }
        _ => {
            panic!();
        }
    }
}

fn interpret_expression<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    var_map: &VarMap,
    node: &'a Node<'a>,
) -> Option<Expression<'a>> {
    match &node.data {
        NodeData::Reference { options: node_options, .. } => {
            let value = interpret_value(
                state,
                &meta_state.collection,
                var_map,
                node,
                false,
            );

            let options = parse_terminal_options(
                state,
                meta_state,
                var_map,
                node_options,
                &None,
            );

            match value {
                Some(Value::String { regex, location, .. }) => {
                    Some(Expression::Terminal {
                        regex,
                        options,
                        location: location.unwrap(),
                    })
                }
                Some(Value::Rule { name, arguments, .. }) => {
                    assert!(node_options.is_none());

                    let key = Key { name, arguments };
                    interpret_rule(
                        state,
                        meta_state,
                        Some(node.location),
                        &key,
                    );
                    Some(Expression::Variable { key, location: node.location })
                }
                None => None,
            }
        }
        NodeData::RegexTerminal { options: node_options, embed } => {
            let regex = interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                node.location,
                node.text,
            );
            let options = parse_terminal_options(
                state,
                meta_state,
                var_map,
                node_options,
                embed,
            );

            let regex = state.compiler.get_symbol(&regex);
            Some(Expression::Terminal {
                regex,
                options,
                location: node.location,
            })
        }
        NodeData::LiteralTerminal { regex, options: node_options, embed } => {
            let options = parse_terminal_options(
                state,
                meta_state,
                var_map,
                node_options,
                embed,
            );
            let regex = state.compiler.get_symbol(&regex);
            Some(Expression::Terminal {
                regex,
                options,
                location: node.location,
            })
        }
        NodeData::Passive(child) => {
            if let Some(expression) =
                interpret_expression(state, meta_state, var_map, child)
            {
                Some(Expression::Passive {
                    expression: state.compiler.allocator.alloc(expression),
                    location: node.location,
                })
            } else {
                None
            }
        }
        NodeData::Repetition(child) => {
            if let Some(expression) =
                interpret_expression(state, meta_state, var_map, child)
            {
                Some(Expression::Repetition {
                    expression: state.compiler.allocator.alloc(expression),
                    location: node.location,
                })
            } else {
                None
            }
        }
        NodeData::Optional(child) => {
            if let Some(expression) =
                interpret_expression(state, meta_state, var_map, child)
            {
                Some(Expression::Optional {
                    expression: state.compiler.allocator.alloc(expression),
                    location: node.location,
                })
            } else {
                None
            }
        }
        NodeData::Alternation(children) => {
            let mut expressions = vec![];
            for child in children {
                if let Some(e) =
                    interpret_expression(state, meta_state, var_map, child)
                {
                    expressions.push(e);
                }
            }
            let expressions = state
                .compiler
                .allocator
                .alloc_slice_fill_iter(expressions.into_iter());

            if expressions.len() != children.len() {
                None
            } else {
                Some(Expression::Alternation {
                    expressions,
                    location: node.location,
                })
            }
        }
        NodeData::Concatenation(children) => {
            let mut expressions = vec![];
            for child in children {
                if let Some(e) =
                    interpret_expression(state, meta_state, var_map, child)
                {
                    expressions.push(e);
                }
            }
            let expressions = state
                .compiler
                .allocator
                .alloc_slice_fill_iter(expressions.into_iter());

            if expressions.len() != children.len() {
                None
            } else {
                Some(Expression::Concatenation {
                    expressions,
                    location: node.location,
                })
            }
        }
        _ => panic!(),
    }
}

fn parse_terminal_options<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    var_map: &VarMap,
    node_options: &'a Option<Box<Node<'a>>>,
    node_embed: &'a Option<Box<Node<'a>>>,
) -> TerminalOptions {
    let mut options = TerminalOptions {
        scope: sublime_syntax::Scope::empty(),
        captures: HashMap::new(),
        embed: parse_terminal_embed(state, meta_state, var_map, node_embed),
    };

    let node_options = if let Some(opts) = node_options {
        match &opts.data {
            NodeData::Options(opts) => opts,
            _ => panic!(),
        }
    } else {
        return options;
    };

    for (i, option) in node_options.iter().enumerate() {
        match &option.data {
            NodeData::PositionalOption => {
                // A positional option may only appear at the start to
                // determine the scope
                if i == 0 {
                    let interpolated = interpolate_string(
                        state,
                        &meta_state.collection,
                        var_map,
                        option.location,
                        option.text,
                    );

                    options.scope =
                        parse_scope(meta_state.metadata, &interpolated);
                } else {
                    state.errors.push(Error::from_str(
                        "Positional argument for terminal scope may only be the first argument",
                        Some(option.location),
                        vec!()).with_traceback(state.stack.clone()));
                }
            }
            NodeData::KeywordOption(value_node) => {
                let key = trim_ascii(option.text);

                assert!(value_node.data == NodeData::KeywordOptionValue);
                let value_text = trim_ascii(value_node.text);
                let value = interpolate_string(
                    state,
                    &meta_state.collection,
                    var_map,
                    value_node.location,
                    value_text,
                );

                // The first set of keyword arguments determine captures
                if let Some(group) = key.parse::<u16>().ok() {
                    if options.captures.contains_key(&group) {
                        // TODO: Improve error message
                        state.errors.push(
                            Error::from_str(
                                "Duplicate capture group argument",
                                Some(option.location),
                                vec![],
                            )
                            .with_traceback(state.stack.clone()),
                        );
                    } else {
                        options.captures.insert(
                            group,
                            parse_scope(meta_state.metadata, &value),
                        );
                    }
                } else {
                    state.errors.push(Error::from_str(
                        "Unexpected keyword argument",
                        Some(option.location),
                        vec!(
                            (option.location, "There should be no arguments after capture groups".to_string()),
                        )).with_traceback(state.stack.clone()));
                }
            }
            _ => panic!(),
        }
    }

    options
}

fn parse_rule_options<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    var_map: &VarMap,
    name: Symbol,
    options: Option<&'a Node<'a>>,
) -> RuleOptions {
    let mut scope = sublime_syntax::Scope::empty();
    let mut include_prototype: Option<(&'a Node<'a>, bool)> = None;

    {
        let name = state.compiler.resolve_symbol(name);

        if state.options.debug_contexts {
            scope.scopes.push(format!("{}.sbnf-dbg", name));
        }

        if options.is_none() {
            return RuleOptions {
                scope,
                include_prototype: true,
                capture: name == "main" || name == "prototype",
            };
        }
    }

    let options = match &options.unwrap().data {
        NodeData::Options(opts) => opts,
        _ => panic!(),
    };

    for (i, argument) in options.iter().enumerate() {
        if i == 0 && argument.data == NodeData::PositionalOption {
            let interpolated = interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                argument.location,
                argument.text,
            );
            scope = parse_scope(meta_state.metadata, &interpolated);
        } else if argument.data == NodeData::PositionalOption {
            state.errors.push(Error::from_str(
                "Rules may only have one positional argument specifying the meta scope",
                Some(argument.location),
                vec!(
                    (argument.location, "this argument".to_string()),
                    (options[0].location, "should be used here".to_string()),
                )));
        } else if let NodeData::KeywordOption(value_node) = &argument.data {
            assert!(value_node.data == NodeData::KeywordOptionValue);
            let value_text = trim_ascii(value_node.text);
            let value = interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                value_node.location,
                value_text,
            );

            if trim_ascii(argument.text) == "include-prototype" {
                if let Some((node, _)) = include_prototype {
                    state.errors.push(Error::from_str(
                        "Duplicate 'include-prototype' argument",
                        Some(argument.location),
                        vec![
                            (argument.location, "duplicate here".to_string()),
                            (node.location, "first used here".to_string()),
                        ],
                    ));
                } else {
                    if let Ok(v) = value.parse::<bool>() {
                        include_prototype = Some((argument, v));
                    } else {
                        state.errors.push(Error::new(
                            format!("Unexpected option value '{}' for 'include-prototype'", argument.text),
                            Some(value_node.location),
                            vec!(
                                (value_node.location, "expected either 'true' or 'false'".to_string()),
                                (argument.location, "for this argument".to_string()),
                            )));
                    }
                }
            } else {
                state.errors.push(Error::new(
                    format!("Unknown argument '{}'", argument.text),
                    Some(argument.location),
                    vec![(
                        argument.location,
                        "expected 'include-prototype' here".to_string(),
                    )],
                ));
            }
        }
    }

    let name = state.compiler.resolve_symbol(name);

    RuleOptions {
        scope,
        include_prototype: include_prototype.map_or(true, |s| s.1),
        capture: name == "main" || name == "prototype",
    }
}

fn parse_terminal_embed<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    var_map: &VarMap,
    node_embed: &'a Option<Box<Node<'a>>>,
) -> TerminalEmbed {
    let node_embed = if let Some(n) = node_embed.as_ref() {
        n
    } else {
        return TerminalEmbed::None;
    };

    let (parameters, options) = match &node_embed.data {
        NodeData::Embed { parameters, options } => (parameters, options),
        _ => panic!(),
    };

    let parameters = match &parameters.data {
        NodeData::Parameters(p) => p,
        _ => panic!(),
    };

    let options = match &options.data {
        NodeData::Options(o) => o,
        _ => panic!(),
    };

    if node_embed.text == "embed" {
        // Embed takes a single string parameter for the escape regex
        if parameters.len() != 1 {
            state.errors.push(
                Error::from_str(
                    "Expected a single parameter to %embed",
                    Some(node_embed.location),
                    vec![(
                        node_embed.location,
                        format!("Got {} parameters instead", parameters.len()),
                    )],
                )
                .with_traceback(state.stack.clone()),
            );
            return TerminalEmbed::None;
        }

        let escape = if let NodeData::RegexTerminal { .. } = parameters[0].data
        {
            interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                parameters[0].location,
                parameters[0].text,
            )
        } else if let NodeData::LiteralTerminal { regex, .. } =
            &parameters[0].data
        {
            regex.to_string()
        } else {
            state.errors.push(
                Error::from_str(
                    "%embed can only take a string as an argument",
                    Some(node_embed.location),
                    vec![(
                        node_embed.location,
                        "Given a variable instead".to_string(),
                    )],
                )
                .with_traceback(state.stack.clone()),
            );
            return TerminalEmbed::None;
        };

        // Embed takes at least one option for the syntax to include and the
        // escape scope and capture scopes
        if options.len() < 1 {
            state.errors.push(
                Error::from_str(
                    "Expected at least one option for %embed",
                    Some(node_embed.location),
                    vec![(
                        node_embed.location,
                        "Requires at least one option".to_string(),
                    )],
                )
                .with_traceback(state.stack.clone()),
            );
            return TerminalEmbed::None;
        }

        let embed = match &options[0].data {
            NodeData::PositionalOption => interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                options[0].location,
                options[0].text,
            ),
            NodeData::KeywordOption(value_node) => {
                let key = options[0].text;

                assert!(value_node.data == NodeData::KeywordOptionValue);
                let value_text = interpolate_string(
                    state,
                    &meta_state.collection,
                    var_map,
                    value_node.location,
                    value_node.text,
                );

                let mut result = String::new();
                result.push_str(key);
                result.push(':');
                result.push_str(&value_text);
                result
            }
            _ => panic!(),
        };

        let mut embed_scope = sublime_syntax::Scope::empty();
        let mut escape_captures = HashMap::new();

        for (i, option) in options[1..].iter().enumerate() {
            match &option.data {
                NodeData::PositionalOption => {
                    // A positional option may only appear at the start to
                    // determine the scope
                    if i == 0 {
                        let interpolated = interpolate_string(
                            state,
                            &meta_state.collection,
                            var_map,
                            option.location,
                            option.text,
                        );

                        embed_scope =
                            parse_scope(meta_state.metadata, &interpolated);
                    } else {
                        state.errors.push(Error::from_str(
                            "Positional argument for escape scope may only be the second argument",
                            Some(option.location),
                            vec!()).with_traceback(state.stack.clone()));
                    }
                }
                NodeData::KeywordOption(value_node) => {
                    let key = trim_ascii(option.text);

                    assert!(value_node.data == NodeData::KeywordOptionValue);
                    let value_text = trim_ascii(value_node.text);
                    let value = interpolate_string(
                        state,
                        &meta_state.collection,
                        var_map,
                        value_node.location,
                        value_text,
                    );

                    // The first set of keyword arguments determine captures
                    if let Some(group) = key.parse::<u16>().ok() {
                        if escape_captures.contains_key(&group) {
                            // TODO: Improve error message
                            state.errors.push(
                                Error::from_str(
                                    "Duplicate capture group argument",
                                    Some(option.location),
                                    vec![],
                                )
                                .with_traceback(state.stack.clone()),
                            );
                        } else {
                            escape_captures.insert(
                                group,
                                parse_scope(meta_state.metadata, &value),
                            );
                        }
                    } else {
                        state.errors.push(Error::from_str(
                            "Unexpected keyword argument",
                            Some(option.location),
                            vec!(
                                (option.location, "There should be no arguments after capture groups".to_string()),
                            )).with_traceback(state.stack.clone()));
                    }
                }
                _ => panic!(),
            }
        }

        TerminalEmbed::Embed { embed, embed_scope, escape, escape_captures }
    } else if node_embed.text == "include" {
        // Include take a single parameter as the rule for the with_prototype
        if parameters.len() != 1 {
            state.errors.push(
                Error::from_str(
                    "Expected a single parameter to %include",
                    Some(node_embed.location),
                    vec![(
                        node_embed.location,
                        format!("Got {} parameters instead", parameters.len()),
                    )],
                )
                .with_traceback(state.stack.clone()),
            );
            return TerminalEmbed::None;
        }

        let value = interpret_value(
            state,
            meta_state.collection,
            var_map,
            &parameters[0],
            false,
        );

        let prototype = match value {
            Some(Value::String { location, .. }) => {
                let mut comments = vec![
                    (
                        node_embed.location,
                        "Must be provided a rule".to_string(),
                    ),
                    (
                        parameters[0].location,
                        "Should be a rule, but was a string".to_string(),
                    ),
                ];
                if let Some(location) = location {
                    comments
                        .push((location, "String came from here".to_string()));
                }
                state.errors.push(
                    Error::from_str(
                        "Argument must be a rule, but was a string instead",
                        Some(node_embed.location),
                        comments,
                    )
                    .with_traceback(state.stack.clone()),
                );
                return TerminalEmbed::None;
            }
            Some(Value::Rule { name, arguments, .. }) => {
                let key = Key { name, arguments };
                interpret_rule(
                    state,
                    meta_state,
                    Some(parameters[0].location),
                    &key,
                );
                key
            }
            None => return TerminalEmbed::None,
        };

        // Include takes a single option for the context to include
        if options.len() != 1 {
            state.errors.push(
                Error::from_str(
                    "Expected a single option for %include",
                    Some(node_embed.location),
                    vec![(
                        node_embed.location,
                        "Requires one option".to_string(),
                    )],
                )
                .with_traceback(state.stack.clone()),
            );
            return TerminalEmbed::None;
        }

        let context = match &options[0].data {
            NodeData::PositionalOption => interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                options[0].location,
                options[0].text,
            ),
            NodeData::KeywordOption(value_node) => {
                let key = options[0].text;

                assert!(value_node.data == NodeData::KeywordOptionValue);
                let value_text = interpolate_string(
                    state,
                    &meta_state.collection,
                    var_map,
                    value_node.location,
                    value_node.text,
                );

                let mut result = String::new();
                result.push_str(key);
                result.push(':');
                result.push_str(&value_text);
                result
            }
            _ => panic!(),
        };

        TerminalEmbed::Include { context, prototype }
    } else {
        state.errors.push(
            Error::from_str(
                "Unknown keyword",
                Some(node_embed.location),
                vec![(
                    node_embed.location,
                    format!("Unknown keyword '{}'", node_embed.text),
                )],
            )
            .with_traceback(state.stack.clone()),
        );

        TerminalEmbed::None
    }
}

// A fast way to convert an interval of iterators to a substring. Rust should
// at least have an easy way to get byte indices from Chars :(
fn str_from_iterators<'a>(
    string: &'a str,
    start: std::str::Chars<'a>,
    end: std::str::Chars<'a>,
) -> &'a str {
    // Convert start and end into byte offsets
    let bytes_start = string.as_bytes().len() - start.as_str().as_bytes().len();
    let bytes_end = string.as_bytes().len() - end.as_str().as_bytes().len();

    // SAFETY: As long as the iterators are from the string the byte offsets
    // will always be valid.
    unsafe {
        std::str::from_utf8_unchecked(
            &string.as_bytes()[bytes_start..bytes_end],
        )
    }
}

// TODO: Tests
// TODO: Move to parser
fn interpolate_string<'a, 'b>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    var_map: &VarMap,
    location: TextLocation,
    string: &'a str,
) -> String {
    let mut result = String::new();

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
                    let start = iter.clone();

                    loop {
                        let mut peek = iter.clone();
                        match peek.next() {
                            Some(']') => {
                                break;
                            }
                            Some(c) => {
                                if is_identifier_char(c) {
                                    iter.next();
                                } else {
                                    // TODO: Add node offset to show the error at the character
                                    state.errors.push(Error::new(
                                        format!("Unexpected character '{}' in string interpolation", c),
                                        Some(location),
                                        vec!()).with_traceback(state.stack.clone()));
                                    return result;
                                }
                            }
                            None => {
                                state.errors.push(Error::from_str(
                                    "Expected ']' before the end of the string to end string interpolation",
                                    Some(location),
                                    vec!()).with_traceback(state.stack.clone()));
                                return result;
                            }
                        }
                    }

                    let variable =
                        str_from_iterators(string, start, iter.clone());
                    let name = state.compiler.get_symbol(&variable);
                    let end = iter.next();
                    assert!(end.unwrap() == ']');

                    let value = if let Some(value) = var_map.get(&name) {
                        Some(value.clone())
                    } else {
                        let key = Key { name, arguments: vec![] };

                        if let Some((kind, ref_node, _)) = resolve_definition(
                            state,
                            collection,
                            Some(location),
                            &key,
                        ) {
                            match kind {
                                DefinitionKind::Variable => interpret_variable(
                                    state,
                                    collection,
                                    Some(location),
                                    key,
                                ),
                                DefinitionKind::Rule => Some(Value::Rule {
                                    name: key.name,
                                    arguments: key.arguments,
                                    location: ref_node.location,
                                }),
                            }
                        } else {
                            None
                        }
                    };

                    if let Some(value) = value {
                        match value {
                            Value::Rule { location, .. } => {
                                state.errors.push(
                                    Error::from_str(
                                        "Can't interpolate rule",
                                        Some(location),
                                        vec![
                                            (
                                                location,
                                                format!(
                                                    "{} must refer to a string",
                                                    variable
                                                ),
                                            ),
                                            (
                                                location,
                                                format!(
                                                    "{} is {}",
                                                    variable,
                                                    value.with_compiler(
                                                        state.compiler
                                                    )
                                                ),
                                            ),
                                        ],
                                    )
                                    .with_traceback(state.stack.clone()),
                                );
                            }
                            Value::String { regex, .. } => {
                                result.push_str(
                                    state.compiler.resolve_symbol(regex),
                                );
                            }
                        }
                    } else {
                        state.errors.push(Error::from_str(
                            "Variable in string interpolation not found",
                            Some(location),
                            vec!(
                                (location, format!("{} must be defined as an argument to the rule", variable)),
                            )).with_traceback(state.stack.clone()));
                    }
                }
                Some(next_chr) => {
                    result.push('#');
                    result.push(next_chr);
                }
                None => {
                    result.push('#');
                    break;
                }
            }
        } else {
            result.push(chr);
        }
    }

    result
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::sbnf::TextLocation;

    pub fn expr_var<'a>(key: Key) -> Expression<'a> {
        Expression::Variable { key, location: TextLocation::INITIAL }
    }

    pub fn expr_trm<'a>(
        regex: Symbol,
        options: TerminalOptions,
    ) -> Expression<'a> {
        Expression::Terminal { regex, options, location: TextLocation::INITIAL }
    }

    pub fn expr_trm_noopt<'a>(regex: Symbol) -> Expression<'a> {
        expr_trm(regex, TerminalOptions::new())
    }

    pub fn expr_pas<'a>(
        c: &'a Compiler,
        expr: Expression<'a>,
    ) -> Expression<'a> {
        Expression::Passive {
            expression: c.allocator.alloc(expr),
            location: TextLocation::INITIAL,
        }
    }

    pub fn expr_rep<'a>(
        c: &'a Compiler,
        expr: Expression<'a>,
    ) -> Expression<'a> {
        Expression::Repetition {
            expression: c.allocator.alloc(expr),
            location: TextLocation::INITIAL,
        }
    }

    pub fn expr_opt<'a>(
        c: &'a Compiler,
        expr: Expression<'a>,
    ) -> Expression<'a> {
        Expression::Optional {
            expression: c.allocator.alloc(expr),
            location: TextLocation::INITIAL,
        }
    }

    pub fn expr_alt<'a>(
        c: &'a Compiler,
        exprs: &[Expression<'a>],
    ) -> Expression<'a> {
        Expression::Alternation {
            expressions: c.allocator.alloc_slice_clone(exprs),
            location: TextLocation::INITIAL,
        }
    }

    pub fn expr_cat<'a>(
        c: &'a Compiler,
        exprs: &[Expression<'a>],
    ) -> Expression<'a> {
        Expression::Concatenation {
            expressions: c.allocator.alloc_slice_clone(exprs),
            location: TextLocation::INITIAL,
        }
    }

    fn interp<'a, 'b>(
        compiler: &mut Compiler,
        collection: &DefinitionMap<'a>,
        var_map: &VarMap,
        location: TextLocation,
        string: &'a str,
    ) -> Option<Symbol> {
        let options = CompileOptions {
            name_hint: None,
            arguments: vec![],
            debug_contexts: false,
            entry_points: vec![],
        };

        let mut state = State {
            compiler,
            options: &options,
            seen_definitions: HashSet::new(),
            variables: HashMap::new(),
            rules: HashMap::new(),
            stack: CallStack::new(),
            errors: vec![],
            warnings: vec![],
        };

        let result = interpolate_string(
            &mut state, collection, var_map, location, string,
        );

        if state.errors.is_empty() {
            Some(compiler.get_symbol(&result))
        } else {
            None
        }
    }

    #[test]
    fn interpolate_string_test() {
        let mut compiler = Compiler::new();
        let collection = DefinitionMap::new();
        let var_map = VarMap::from([(
            compiler.get_symbol("a"),
            Value::String {
                regex: compiler.get_symbol("b"),
                literal: compiler.get_symbol("b"),
                location: None,
            },
        )]);
        let location = TextLocation::new(0, 0);

        assert_eq!(
            Some(compiler.get_symbol("")),
            interp(&mut compiler, &collection, &var_map, location, "")
        );
        assert_eq!(
            Some(compiler.get_symbol("#")),
            interp(&mut compiler, &collection, &var_map, location, "#")
        );
        assert_eq!(
            Some(compiler.get_symbol("#!")),
            interp(&mut compiler, &collection, &var_map, location, "#!")
        );
        assert_eq!(
            Some(compiler.get_symbol("#a")),
            interp(&mut compiler, &collection, &var_map, location, "#a")
        );
        assert_eq!(
            None,
            interp(&mut compiler, &collection, &var_map, location, "#[")
        );
        assert_eq!(
            None,
            interp(&mut compiler, &collection, &var_map, location, "#[]")
        );
        assert_eq!(
            Some(compiler.get_symbol("#]")),
            interp(&mut compiler, &collection, &var_map, location, "#]")
        );
        assert_eq!(
            Some(compiler.get_symbol("b")),
            interp(&mut compiler, &collection, &var_map, location, "#[a]")
        );
        assert_eq!(
            Some(compiler.get_symbol("aba")),
            interp(&mut compiler, &collection, &var_map, location, "a#[a]a")
        );
        assert_eq!(
            Some(compiler.get_symbol("#[a]")),
            interp(&mut compiler, &collection, &var_map, location, "\\#[a]")
        );
    }
}
