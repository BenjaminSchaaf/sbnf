use std::collections::{HashMap, HashSet};

use super::collector::{
    is_rule_name, is_variable_name, Collection, Definition, DefinitionKind,
    DefinitionMap,
};
use super::common::{
    parse_scope, parse_top_level_scope, trim_ascii, CallStack, CompileOptions,
    CompileResult, Error, Metadata, RuleOptions, Value, VarMap,
};
use crate::sbnf::parser::{Node, NodeData};
use crate::sbnf::common::is_identifier_char;
use crate::sublime_syntax;

pub struct Interpreted<'a> {
    pub rules: HashMap<Key<'a>, Rule<'a>>,
    pub entry_points: Vec<Key<'a>>,
    pub metadata: Metadata,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Key<'a> {
    pub name: &'a str,
    pub arguments: Vec<Value<'a>>,
}

impl<'a> std::fmt::Display for Key<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
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
pub struct Rule<'a> {
    pub options: RuleOptions,
    pub expression: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TerminalOptions<'a> {
    pub scope: sublime_syntax::Scope,
    pub captures: HashMap<u16, sublime_syntax::Scope>,
    pub embed: TerminalEmbed<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TerminalEmbed<'a> {
    Embed {
        embed: String,
        embed_scope: sublime_syntax::Scope,
        escape: String,
        escape_captures: HashMap<u16, sublime_syntax::Scope>,
    },
    Include {
        context: String,
        prototype: Key<'a>,
    },
    None,
}

pub enum Expression<'a> {
    Variable { key: Key<'a>, node: &'a Node<'a> },
    Terminal { regex: String, options: TerminalOptions<'a>, node: &'a Node<'a> },
    Passive { expression: Box<Expression<'a>>, node: &'a Node<'a> },
    Repetition { expression: Box<Expression<'a>>, node: &'a Node<'a> },
    Optional { expression: Box<Expression<'a>>, node: &'a Node<'a> },
    Alternation { expressions: Vec<Expression<'a>>, node: &'a Node<'a> },
    Concatenation { expressions: Vec<Expression<'a>>, node: &'a Node<'a> },
}

impl<'a> PartialEq for Expression<'a> {
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
impl<'a> Eq for Expression<'a> {}

impl<'a> std::hash::Hash for Expression<'a> {
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

impl<'a> std::fmt::Debug for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Variable { key, .. } => write!(f, "{}", key),
            Expression::Terminal { regex, .. } => write!(f, "'{}'", regex),
            Expression::Passive { expression, .. } => {
                write!(f, "~{:?}", expression)
            }
            Expression::Repetition { expression, .. } => {
                write!(f, "{:?}*", expression)
            }
            Expression::Optional { expression, .. } => {
                write!(f, "{:?}?", expression)
            }
            Expression::Alternation { expressions, .. } => {
                write!(f, "({:?}", expressions[0])?;
                for expression in &expressions[1..] {
                    write!(f, " | {:?}", expression)?;
                }
                write!(f, ")")
            }
            Expression::Concatenation { expressions, .. } => {
                write!(f, "({:?}", expressions[0])?;
                for expression in &expressions[1..] {
                    write!(f, " {:?}", expression)?;
                }
                write!(f, ")")
            }
        }
    }
}

struct State<'a> {
    seen_definitions: HashSet<Key<'a>>,
    variables: HashMap<Key<'a>, Option<Value<'a>>>,
    rules: HashMap<Key<'a>, Rule<'a>>,
    stack: CallStack<'a>,
    errors: Vec<Error<'a>>,
    warnings: Vec<Error<'a>>,
}

struct MetaState<'a, 'b> {
    options: &'a CompileOptions<'a>,
    collection: &'b DefinitionMap<'a>,
    metadata: &'b Metadata,
}

pub fn interpret<'a, 'b>(
    options: &'a CompileOptions<'a>,
    collection: Collection<'a>,
) -> CompileResult<'a, Interpreted<'a>> {
    let mut state = State {
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

    let metadata = collect_metadata(&mut state, &definitions, options);

    let mut entry_points = vec![];

    {
        let meta_state =
            MetaState { options, collection: definitions, metadata: &metadata };

        for entry_point in &options.entry_points {
            if let Some(((name, _), _)) =
                meta_state.collection.get_key_value(&(*entry_point, 0))
            {
                let key = Key { name, arguments: vec![] };

                assert!(state.stack.is_empty());
                interpret_rule(&mut state, &meta_state, None, &key);

                entry_points.push(key);
            }
        }
    }

    CompileResult::new(
        if state.errors.is_empty() {
            Ok(Interpreted { rules: state.rules, entry_points, metadata })
        } else {
            Err(state.errors)
        },
        state.warnings,
    )
}

fn collect_metadata<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    options: &CompileOptions<'a>,
) -> Metadata {
    let name = interpret_metadata_variable(state, collection, "NAME", true)
        .map(|s| s.1.to_string())
        .or_else(|| options.name_hint.map(|s| trim_ascii(s).to_string()));

    // A name is required, either from a variable or the name hint
    if name.is_none() {
        state.errors.push(Error::without_node(
            "No syntax name provided. Use a 'NAME' variable to specify the name of the syntax".to_string(),
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
            |s| parse_top_level_scope(&s.1),
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
                node,
                vec![(node, format!("was `{}` instead", value))],
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
    if let Some(definition) = collection.get(&(name, 0)) {
        assert!(definition.kind == DefinitionKind::Variable);

        let overloads = &definition.overloads;

        let key = Key { name, arguments: vec![] };

        let value = interpret_variable(state, collection, None, key);

        match value {
            Some(Value::String { literal, regex, .. }) => Some((
                overloads[0],
                if is_literal {
                    literal.to_string()
                } else {
                    regex.to_string()
                },
            )),
            Some(Value::Rule { node, .. }) => {
                state.errors.push(Error::new(
                    format!("'{}' must be a string, not a rule", name),
                    overloads[0],
                    vec![
                        (overloads[0], "must be a string".to_string()),
                        (node, "was this instead".to_string()),
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
    arguments: &Vec<Value<'a>>,
) -> Option<(&'a Node<'a>, VarMap<'a>)> {
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
            var_map.insert(param.text, arg.clone());
        }
    }

    Some((def_node, var_map))
}

fn resolve_definition<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    reference: Option<&'a Node<'a>>,
    key: &Key<'a>,
) -> Option<(DefinitionKind, &'a Node<'a>, VarMap<'a>)> {
    if let Some(Definition { kind, overloads }) =
        collection.get(&(key.name, key.arguments.len() as u8))
    {
        let mut matching = overloads
            .iter()
            .filter_map(|r| match_rule(state, collection, r, &key.arguments));

        if let Some((node, var_map)) = matching.next() {
            let remaining = matching.collect::<Vec<_>>();

            if !remaining.is_empty() {
                let mut comments =
                    vec![(node, "This matches the arguments".to_string())];
                for (other_node, _) in remaining {
                    comments.push((
                        other_node,
                        "This also matches the arguments".to_string(),
                    ));
                }
                if let Some(reference) = reference {
                    comments.push((
                        reference,
                        "Instantiated from here".to_string(),
                    ));
                }
                state.errors.push(state.stack.error(
                    format!("Ambiguous instantiation for {}", key),
                    node,
                    comments,
                ));
                return None;
            }

            Some((*kind, node, var_map))
        } else {
            let mut comments: Vec<(&'a Node<'a>, String)> = vec![];
            for node in overloads {
                comments.push((node, "Does not match".to_string()));
            }
            if let Some(reference) = &reference {
                comments
                    .push((reference, "Instantiated from here".to_string()));
            }
            state.errors.push(state.stack.error(
                format!("No matching instantiation for {}", key),
                reference.unwrap_or(overloads[0]),
                comments,
            ));

            None
        }
    } else {
        let mut msg = if is_variable_name(key.name) {
            format!("Could not find variable {}", key.name)
        } else if is_rule_name(key.name) {
            format!("Could not find rule {}", key.name)
        } else {
            format!("Could not find {}", key.name)
        };

        if key.arguments.len() > 0 {
            msg.push_str(&format!(" with {} arguments", key.arguments.len()));
        }

        state.errors.push(state.stack.error(
            msg,
            reference.unwrap(),
            vec![(reference.unwrap(), "Does not exist".to_string())],
        ));

        None
    }
}

fn interpret_variable<'a>(
    state: &mut State<'a>,
    collection: &DefinitionMap<'a>,
    reference: Option<&'a Node<'a>>,
    key: Key<'a>,
) -> Option<Value<'a>> {
    if state.variables.contains_key(&key) {
        return state.variables[&key].clone();
    }

    if let Some((kind, node, var_map)) =
        resolve_definition(state, collection, reference, &key)
    {
        assert!(kind == DefinitionKind::Variable);

        // Detect recursive definitions
        if state.seen_definitions.contains(&key) {
            state.errors.push(state.stack.error(
                format!("Variable {} is defined in terms of itself", key.name),
                node,
                vec![],
            ));
            return None;
        }

        let is_new_key = state.seen_definitions.insert(key.clone());
        assert!(is_new_key);

        if !state.stack.push(reference, node, key.arguments.clone()) {
            state.errors.push(state.stack.error_from_str(
                "Recursion depth limit reached",
                reference.unwrap(), // Only none when the stack size is 1
                vec![],
            ));
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
    reference: Option<&'a Node<'a>>,
    key: &Key<'a>,
) {
    // Nothing to do if the key has been seen before
    if state.seen_definitions.contains(key) {
        return;
    }

    if let Some((kind, node, var_map)) =
        resolve_definition(state, &meta_state.collection, reference, key)
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

        if !state.stack.push(reference, node, key.arguments.clone()) {
            state.errors.push(state.stack.error_from_str(
                "Recursion depth limit reached",
                reference.unwrap(), // Only none when the stack size is 1
                vec![],
            ));
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
    var_map: &VarMap<'a>,
    node: &'a Node<'a>,
    is_parameter: bool,
) -> Option<Value<'a>> {
    match &node.data {
        NodeData::RegexTerminal { options, embed } => {
            assert!(options.is_none());
            assert!(embed.is_none());

            let regex =
                interpolate_string(state, collection, var_map, node, node.text);

            Some(Value::String {
                regex: regex.clone(),
                literal: regex,
                node: Some(node),
            })
        }
        NodeData::LiteralTerminal { regex, options, embed } => {
            assert!(options.is_none());
            assert!(embed.is_none());
            Some(Value::String {
                regex: regex.to_string(),
                literal: node.text.to_string(),
                node: Some(node),
            })
        }
        NodeData::Reference { parameters, .. } => {
            let arguments = if let Some(param_node) = parameters {
                let parameters = match &param_node.data {
                    NodeData::Parameters(params) => params,
                    _ => panic!(),
                };

                let arguments = parameters
                    .iter()
                    .filter_map(|p| {
                        interpret_value(state, collection, var_map, p, false)
                    })
                    .collect::<Vec<_>>();

                if arguments.len() != parameters.len() {
                    return None;
                }

                arguments
            } else {
                vec![]
            };

            if arguments.is_empty() {
                if let Some(value) = var_map.get(node.text) {
                    return Some(value.clone());
                }
            }

            // Don't try to resolve the definition if we're a parameter and know
            // it doesn't exist
            if is_parameter
                && arguments.is_empty()
                && !collection.contains_key(&(node.text, 0))
            {
                return None;
            }

            let key = Key { name: node.text, arguments };

            if let Some((kind, ref_node, _)) =
                resolve_definition(state, collection, Some(node), &key)
            {
                match kind {
                    DefinitionKind::Variable => {
                        interpret_variable(state, collection, Some(node), key)
                    }
                    DefinitionKind::Rule => Some(Value::Rule {
                        name: key.name,
                        arguments: key.arguments,
                        node: ref_node,
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
    var_map: &VarMap<'a>,
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
                Some(Value::String { regex, node, .. }) => {
                    Some(Expression::Terminal {
                        regex: regex.to_string(),
                        options,
                        node: node.unwrap(),
                    })
                }
                Some(Value::Rule { name, arguments, .. }) => {
                    assert!(node_options.is_none());

                    let key = Key { name, arguments };
                    interpret_rule(state, meta_state, Some(node), &key);
                    Some(Expression::Variable { key, node })
                }
                None => None,
            }
        }
        NodeData::RegexTerminal { options: node_options, embed } => {
            let regex = interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                node,
                node.text,
            );
            let options = parse_terminal_options(
                state,
                meta_state,
                var_map,
                node_options,
                embed,
            );

            Some(Expression::Terminal { regex, options, node })
        }
        NodeData::LiteralTerminal { regex, options: node_options, embed } => {
            let options = parse_terminal_options(
                state,
                meta_state,
                var_map,
                node_options,
                embed,
            );
            Some(Expression::Terminal { regex: regex.clone(), options, node })
        }
        NodeData::Passive(child) => {
            if let Some(expression) =
                interpret_expression(state, meta_state, var_map, child)
            {
                Some(Expression::Passive {
                    expression: Box::new(expression),
                    node,
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
                    expression: Box::new(expression),
                    node,
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
                    expression: Box::new(expression),
                    node,
                })
            } else {
                None
            }
        }
        NodeData::Alternation(children) => {
            let expressions = children
                .iter()
                .filter_map(|child| {
                    interpret_expression(state, meta_state, var_map, child)
                })
                .collect::<Vec<_>>();

            if expressions.len() != children.len() {
                None
            } else {
                Some(Expression::Alternation { expressions, node })
            }
        }
        NodeData::Concatenation(children) => {
            let expressions = children
                .iter()
                .filter_map(|child| {
                    interpret_expression(state, meta_state, var_map, child)
                })
                .collect::<Vec<_>>();

            if expressions.len() != children.len() {
                None
            } else {
                Some(Expression::Concatenation { expressions, node })
            }
        }
        _ => panic!(),
    }
}

fn parse_terminal_options<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    var_map: &VarMap<'a>,
    node_options: &'a Option<Box<Node<'a>>>,
    node_embed: &'a Option<Box<Node<'a>>>,
) -> TerminalOptions<'a> {
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
                        option,
                        option.text,
                    );

                    options.scope =
                        parse_scope(meta_state.metadata, &interpolated);
                } else {
                    state.errors.push(state.stack.error_from_str(
                        "Positional argument for terminal scope may only be the first argument",
                        option,
                        vec!()));
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
                    value_node,
                    value_text,
                );

                // The first set of keyword arguments determine captures
                if let Some(group) = key.parse::<u16>().ok() {
                    if options.captures.contains_key(&group) {
                        // TODO: Improve error message
                        state.errors.push(state.stack.error_from_str(
                            "Duplicate capture group argument",
                            option,
                            vec![],
                        ));
                    } else {
                        options.captures.insert(
                            group,
                            parse_scope(meta_state.metadata, &value),
                        );
                    }
                } else {
                    state.errors.push(state.stack.error_from_str(
                        "Unexpected keyword argument",
                        option,
                        vec!(
                            (option, "There should be no arguments after capture groups".to_string()),
                        )));
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
    var_map: &VarMap<'a>,
    name: &'a str,
    options: Option<&'a Node<'a>>,
) -> RuleOptions {
    let mut scope = sublime_syntax::Scope::empty();
    let mut include_prototype: Option<(&'a Node<'a>, bool)> = None;

    if meta_state.options.debug_contexts {
        scope.scopes.push(format!("{}.sbnf-dbg", name));
    }

    if options.is_none() {
        return RuleOptions {
            scope,
            include_prototype: true,
            capture: name == "main" || name == "prototype",
        };
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
                argument,
                argument.text,
            );
            scope = parse_scope(meta_state.metadata, &interpolated);
        } else if argument.data == NodeData::PositionalOption {
            state.errors.push(Error::from_str(
                "Rules may only have one positional argument specifying the meta scope",
                argument,
                vec!(
                    (argument, "this argument".to_string()),
                    (&options[0], "should be used here".to_string()),
                )));
        } else if let NodeData::KeywordOption(value_node) = &argument.data {
            assert!(value_node.data == NodeData::KeywordOptionValue);
            let value_text = trim_ascii(value_node.text);
            let value = interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                value_node,
                value_text,
            );

            if trim_ascii(argument.text) == "include-prototype" {
                if include_prototype.is_none() {
                    if let Ok(v) = value.parse::<bool>() {
                        include_prototype = Some((argument, v));
                    } else {
                        state.errors.push(Error::new(
                            format!("Unexpected option value '{}' for 'include-prototype'", argument.text),
                            value_node,
                            vec!(
                                (value_node, "expected either 'true' or 'false'".to_string()),
                                (argument, "for this argument".to_string()),
                            )));
                    }
                } else {
                    state.errors.push(Error::from_str(
                        "Duplicate 'include-prototype' argument",
                        argument,
                        vec![
                            (argument, "duplicate here".to_string()),
                            (
                                include_prototype.unwrap().0,
                                "first used here".to_string(),
                            ),
                        ],
                    ));
                }
            } else {
                state.errors.push(Error::new(
                    format!("Unknown argument '{}'", argument.text),
                    argument,
                    vec![(
                        argument,
                        "expected 'include-prototype' here".to_string(),
                    )],
                ));
            }
        }
    }

    RuleOptions {
        scope,
        include_prototype: include_prototype.map_or(true, |s| s.1),
        capture: name == "main" || name == "prototype",
    }
}

fn parse_terminal_embed<'a, 'b>(
    state: &mut State<'a>,
    meta_state: &MetaState<'a, 'b>,
    var_map: &VarMap<'a>,
    node_embed: &'a Option<Box<Node<'a>>>,
) -> TerminalEmbed<'a> {
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
            state.errors.push(state.stack.error_from_str(
                "Expected a single parameter to %embed",
                node_embed,
                vec![(
                    node_embed,
                    format!("Got {} parameters instead", parameters.len()),
                )],
            ));
            return TerminalEmbed::None;
        }

        let escape = if let NodeData::RegexTerminal { .. } = parameters[0].data
        {
            interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                &parameters[0],
                parameters[0].text,
            )
        } else if let NodeData::LiteralTerminal { regex, .. } =
            &parameters[0].data
        {
            regex.to_string()
        } else {
            state.errors.push(state.stack.error_from_str(
                "%embed can only take a string as an argument",
                node_embed,
                vec![(node_embed, "Given a variable instead".to_string())],
            ));
            return TerminalEmbed::None;
        };

        // Embed takes at least one option for the syntax to include and the
        // escape scope and capture scopes
        if options.len() < 1 {
            state.errors.push(state.stack.error_from_str(
                "Expected at least one option for %embed",
                node_embed,
                vec![(node_embed, "Requires at least one option".to_string())],
            ));
            return TerminalEmbed::None;
        }

        let embed = match &options[0].data {
            NodeData::PositionalOption => interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                &options[0],
                options[0].text,
            ),
            NodeData::KeywordOption(value_node) => {
                let key = options[0].text;

                assert!(value_node.data == NodeData::KeywordOptionValue);
                let value_text = interpolate_string(
                    state,
                    &meta_state.collection,
                    var_map,
                    value_node,
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
                            option,
                            option.text,
                        );

                        embed_scope =
                            parse_scope(meta_state.metadata, &interpolated);
                    } else {
                        state.errors.push(state.stack.error_from_str(
                            "Positional argument for escape scope may only be the second argument",
                            option,
                            vec!()));
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
                        value_node,
                        value_text,
                    );

                    // The first set of keyword arguments determine captures
                    if let Some(group) = key.parse::<u16>().ok() {
                        if escape_captures.contains_key(&group) {
                            // TODO: Improve error message
                            state.errors.push(state.stack.error_from_str(
                                "Duplicate capture group argument",
                                option,
                                vec![],
                            ));
                        } else {
                            escape_captures.insert(
                                group,
                                parse_scope(meta_state.metadata, &value),
                            );
                        }
                    } else {
                        state.errors.push(state.stack.error_from_str(
                            "Unexpected keyword argument",
                            option,
                            vec!(
                                (option, "There should be no arguments after capture groups".to_string()),
                            )));
                    }
                }
                _ => panic!(),
            }
        }

        TerminalEmbed::Embed { embed, embed_scope, escape, escape_captures }
    } else if node_embed.text == "include" {
        // Include take a single parameter as the rule for the with_prototype
        if parameters.len() != 1 {
            state.errors.push(state.stack.error_from_str(
                "Expected a single parameter to %include",
                node_embed,
                vec![(
                    node_embed,
                    format!("Got {} parameters instead", parameters.len()),
                )],
            ));
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
            Some(Value::String { node, .. }) => {
                let mut comments = vec![
                    (
                        node_embed.as_ref(),
                        "Must be provided a rule".to_string(),
                    ),
                    (
                        &parameters[0],
                        "Should be a rule, but was a string".to_string(),
                    ),
                ];
                if let Some(node) = node {
                    comments.push((node, "String came from here".to_string()));
                }
                state.errors.push(state.stack.error_from_str(
                    "Argument must be a rule, but was a string instead",
                    node_embed,
                    comments,
                ));
                return TerminalEmbed::None;
            }
            Some(Value::Rule { name, arguments, .. }) => {
                let key = Key { name, arguments };
                interpret_rule(state, meta_state, Some(&parameters[0]), &key);
                key
            }
            None => return TerminalEmbed::None,
        };

        // Include takes a single option for the context to include
        if options.len() != 1 {
            state.errors.push(state.stack.error_from_str(
                "Expected a single option for %include",
                node_embed,
                vec![(node_embed, "Requires one option".to_string())],
            ));
            return TerminalEmbed::None;
        }

        let context = match &options[0].data {
            NodeData::PositionalOption => interpolate_string(
                state,
                &meta_state.collection,
                var_map,
                &options[0],
                options[0].text,
            ),
            NodeData::KeywordOption(value_node) => {
                let key = options[0].text;

                assert!(value_node.data == NodeData::KeywordOptionValue);
                let value_text = interpolate_string(
                    state,
                    &meta_state.collection,
                    var_map,
                    value_node,
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
        state.errors.push(state.stack.error_from_str(
            "Unknown keyword",
            node_embed,
            vec![(
                node_embed,
                format!("Unknown keyword '{}'", node_embed.text),
            )],
        ));

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
    var_map: &VarMap<'a>,
    node: &'a Node<'a>,
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
                                    state.errors.push(state.stack.error(
                                        format!("Unexpected character '{}' in string interpolation", c),
                                        node,
                                        vec!()));
                                    return result;
                                }
                            }
                            None => {
                                state.errors.push(state.stack.error_from_str(
                                    "Expected ']' before the end of the string to end string interpolation",
                                    node,
                                    vec!()));
                                return result;
                            }
                        }
                    }

                    let variable =
                        str_from_iterators(string, start, iter.clone());
                    let end = iter.next();
                    assert!(end.unwrap() == ']');

                    let value = if let Some(value) = var_map.get(&variable) {
                        Some(value.clone())
                    } else {
                        let key = Key { name: variable, arguments: vec![] };

                        if let Some((kind, ref_node, _)) = resolve_definition(
                            state,
                            collection,
                            Some(node),
                            &key,
                        ) {
                            match kind {
                                DefinitionKind::Variable => interpret_variable(
                                    state,
                                    collection,
                                    Some(node),
                                    key,
                                ),
                                DefinitionKind::Rule => Some(Value::Rule {
                                    name: key.name,
                                    arguments: key.arguments,
                                    node: ref_node,
                                }),
                            }
                        } else {
                            None
                        }
                    };

                    if let Some(value) = value {
                        match value {
                            Value::Rule { node: rule_node, .. } => {
                                state.errors.push(state.stack.error_from_str(
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
                                result.push_str(&regex);
                            }
                        }
                    } else {
                        state.errors.push(state.stack.error_from_str(
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
mod tests {
    use super::*;
    use crate::sbnf::parser::TextLocation;

    fn interp<'a, 'b>(
        collection: &DefinitionMap<'a>,
        var_map: &VarMap<'a>,
        node: &'a Node<'a>,
        string: &'a str,
    ) -> Option<String> {
        let mut state = State {
            seen_definitions: HashSet::new(),
            variables: HashMap::new(),
            rules: HashMap::new(),
            stack: CallStack::new(),
            errors: vec![],
            warnings: vec![],
        };

        let result =
            interpolate_string(&mut state, collection, var_map, node, string);

        if state.errors.is_empty() {
            Some(result)
        } else {
            None
        }
    }

    #[test]
    fn interpolate_string_test() {
        let collection = DefinitionMap::new();
        let var_map = VarMap::from([(
            "a",
            Value::String {
                regex: "b".to_string(),
                literal: "b".to_string(),
                node: None,
            },
        )]);
        let node = Node::new(
            "",
            TextLocation::new(0, 0),
            NodeData::Parameters(vec![]),
        );

        assert_eq!(
            Some("".to_string()),
            interp(&collection, &var_map, &node, "")
        );
        assert_eq!(
            Some("#".to_string()),
            interp(&collection, &var_map, &node, "#")
        );
        assert_eq!(
            Some("#!".to_string()),
            interp(&collection, &var_map, &node, "#!")
        );
        assert_eq!(
            Some("#a".to_string()),
            interp(&collection, &var_map, &node, "#a")
        );
        assert_eq!(None, interp(&collection, &var_map, &node, "#["));
        assert_eq!(None, interp(&collection, &var_map, &node, "#[]"));
        assert_eq!(
            Some("#]".to_string()),
            interp(&collection, &var_map, &node, "#]")
        );
        assert_eq!(
            Some("b".to_string()),
            interp(&collection, &var_map, &node, "#[a]")
        );
        assert_eq!(
            Some("aba".to_string()),
            interp(&collection, &var_map, &node, "a#[a]a")
        );
        assert_eq!(
            Some("#[a]".to_string()),
            interp(&collection, &var_map, &node, "\\#[a]")
        );
    }
}
