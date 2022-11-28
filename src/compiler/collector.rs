use std::collections::HashMap;

use super::common::{
    CompileOptions, CompileResult, Compiler, Error, Symbol, Value, VarMap, SourceReference,
};
use crate::sbnf::{Grammar, Node, NodeData};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DefinitionKind {
    Variable,
    Rule,
}

#[derive(Debug)]
pub struct Definition<'a> {
    pub kind: DefinitionKind,
    pub overloads: Vec<&'a Node<'a>>,
}

pub type DefinitionMap<'a> = HashMap<(Symbol, u8), Definition<'a>>;

pub struct Collection<'a> {
    pub variables: VarMap,
    pub definitions: DefinitionMap<'a>,
}

struct State<'a> {
    compiler: &'a Compiler,
    options: &'a CompileOptions<'a>,
    variables: VarMap,
    definitions: DefinitionMap<'a>,
    errors: Vec<Error>,
    warnings: Vec<Error>,
}

pub fn collect<'a>(
    compiler: &'a Compiler,
    options: &'a CompileOptions<'a>,
    source: SourceReference,
    grammar: &Grammar<'a>,
) -> CompileResult<Collection<'a>> {
    let mut state = State {
        compiler,
        options,
        variables: VarMap::new(),
        definitions: HashMap::new(),
        errors: vec![],
        warnings: vec![],
    };

    collect_parameters(&grammar, source, &mut state);
    collect_definitions(&grammar, source, &mut state);

    if state.errors.is_empty() {
        Ok((
            Collection {
                variables: state.variables,
                definitions: state.definitions,
            },
            state.warnings
        ))
    } else {
        Err((state.errors, state.warnings))
    }
}

fn collect_parameters<'a>(
    grammar: &Grammar<'a>,
    source: SourceReference,
    state: &mut State<'a>,
) {
    let mut duplicate: Option<&Node<'a>> = None;

    for node in grammar.nodes {
        match &node.data {
            NodeData::Parameters(params) => {
                if let Some(duplicate) = duplicate {
                    state.errors.push(
                        Error::new_str("Syntax may only contain a single set of syntax parameters", source)
                            .location(node.location)
                            .comment_str("first declared here", source, duplicate.location)
                            .comment_str("duplicate found here", source, node.location));
                    continue;
                }

                duplicate = Some(node);

                if params.is_empty() {
                    state.errors.push(Error::new_str("Empty syntax parameters", source)
                        .location(node.location)
                        .comment_str("leave out parameters if none are required", source, node.location)
                    );
                    continue;
                }

                if params.len() != state.options.arguments.len() {
                    state.errors.push(Error::new_str("Wrong number of arguments", source)
                        .location(node.location)
                        .comment(format!(
                                "expected {} arguments, but was {} instead",
                                state.options.arguments.len(),
                                params.len()
                            ),
                            source,
                            node.location)
                    );
                    continue;
                }

                for (param, arg) in
                    params.iter().zip(state.options.arguments.iter())
                {
                    match &param.data {
                        NodeData::RegexTerminal { .. }
                        | NodeData::LiteralTerminal { .. } => {
                            state.errors.push(Error::new_str("Syntax parameters may only be variables", source)
                                .location(node.location)
                                .comment_str("got terminal instead", source, param.location)
                            );
                        }
                        NodeData::Reference { parameters, options } => {
                            assert!(options.is_none());

                            if !parameters.is_none() {
                                state.errors.push(Error::new_str("Syntax parameters must be plain variables", source)
                                    .location(node.location)
                                    .comment_str("has parameters", source, node.location)
                                );
                                continue;
                            }

                            if !is_variable_name(param.text) {
                                state.errors.push(Error::new_str("Variables must be all upper-case", source)
                                    .location(node.location)
                                    .comment(format!(
                                            "use '{}' instead",
                                            to_variable_name(param.text)
                                        ),
                                        source,
                                        node.location)
                                );
                            }

                            let symbol = state.compiler.get_symbol(param.text);

                            if let Some(duplicate) =
                                state.definitions.get(&(symbol, 0))
                            {
                                assert!(
                                    duplicate.kind == DefinitionKind::Variable
                                );
                                assert!(duplicate.overloads.len() == 1);

                                state.errors.push(Error::new_str("Duplicate parameters are not allowed in syntax parameters", source)
                                    .location(node.location)
                                    .comment_str("first parameter here", source, duplicate.overloads[0].location)
                                    .comment_str("conflicts with this one", source, node.location));
                                continue;
                            }

                            // Keep track of the definition, but also store the value
                            state.definitions.insert(
                                (symbol, 0),
                                Definition {
                                    kind: DefinitionKind::Variable,
                                    overloads: vec![param],
                                },
                            );

                            let arg_symbol = state.compiler.get_symbol(arg);
                            let value = Value::String {
                                regex: arg_symbol,
                                literal: arg_symbol,
                                source,
                                location: None,
                            };
                            state.variables.insert(symbol, value);
                        }
                        _ => panic!(),
                    }
                }
            }
            NodeData::Variable { .. } => {}
            NodeData::Rule { .. } => {}
            _ => panic!(),
        }
    }
}

fn collect_definitions<'a>(
    grammar: &Grammar<'a>,
    source: SourceReference,
    state: &mut State<'a>,
) {
    for node in grammar.nodes {
        match &node.data {
            NodeData::Parameters(_) => {}
            NodeData::Variable { parameters, .. }
            | NodeData::Rule { parameters, .. } => {
                let kind = if let NodeData::Variable { .. } = node.data {
                    DefinitionKind::Variable
                } else {
                    DefinitionKind::Rule
                };

                let name = node.text;

                if kind == DefinitionKind::Variable && !is_variable_name(name) {
                    state.errors.push(Error::new_str("Variables must be all upper-case", source)
                        .location(node.location)
                        .comment(format!("use '{}' instead", to_variable_name(name)), source, node.location));
                    continue;
                } else if kind == DefinitionKind::Rule && !is_rule_name(name) {
                    state.errors.push(Error::new_str("Rules must be all lower-case", source)
                        .location(node.location)
                        .comment(format!("use '{}' instead", to_rule_name(name)), source, node.location),
                    );
                    continue;
                }

                let num_params = parameters.as_ref().map_or(0, |node| {
                    if let Node { data: NodeData::Parameters(v), .. } =
                        node
                    {
                        v.len()
                    } else {
                        panic!();
                    }
                });

                let symbol = state.compiler.get_symbol(name);

                let key = (symbol, num_params as u8);

                let definition: &mut Definition<'a> =
                    if let Some(def) = state.definitions.get_mut(&key) {
                        def
                    } else {
                        state.definitions.insert(
                            key.clone(),
                            Definition { kind, overloads: vec![] },
                        );

                        state.definitions.get_mut(&key).unwrap()
                    };

                // Should always be the case since capitalization is enforced
                assert!(definition.kind == kind);

                if num_params == 0 && !definition.overloads.is_empty() {
                    state.errors.push(Error::new(format!("'{}' has already been defined", name), source)
                        .location(node.location)
                        .comment_str("already defined here", source, definition.overloads[0].location)
                        .comment_str("conflicts with first definition", source, node.location)
                    );
                    continue;
                }

                definition.overloads.push(node);
            }
            _ => panic!(),
        }
    }
}

pub fn is_variable_name(name: &str) -> bool {
    return name
        .chars()
        .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_');
}

fn to_variable_name(name: &str) -> String {
    return name
        .chars()
        .map(|c| if c == '-' { '_' } else { c.to_ascii_uppercase() })
        .collect::<String>();
}

pub fn is_rule_name(name: &str) -> bool {
    return name
        .chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
}

fn to_rule_name(name: &str) -> String {
    return name
        .chars()
        .map(|c| if c == '_' { '-' } else { c.to_ascii_lowercase() })
        .collect::<String>();
}
