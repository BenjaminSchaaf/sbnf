use std::collections::HashMap;

use super::common::{CompileOptions, CompileResult, Error, Value, VarMap};
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

pub type DefinitionMap<'a> = HashMap<(&'a str, u8), Definition<'a>>;

pub struct Collection<'a> {
    pub variables: VarMap<'a>,
    pub definitions: DefinitionMap<'a>,
}

struct State<'a> {
    variables: VarMap<'a>,
    definitions: DefinitionMap<'a>,
    errors: Vec<Error<'a>>,
    warnings: Vec<Error<'a>>,
}

pub fn collect<'a>(
    options: &CompileOptions<'a>,
    grammar: &'a Grammar<'a>,
) -> CompileResult<'a, Collection<'a>> {
    let mut state = State {
        variables: VarMap::new(),
        definitions: HashMap::new(),
        errors: vec![],
        warnings: vec![],
    };

    collect_parameters(options, grammar, &mut state);
    collect_definitions(grammar, &mut state);

    CompileResult::new(
        if state.errors.is_empty() {
            Ok(Collection {
                variables: state.variables,
                definitions: state.definitions,
            })
        } else {
            Err(state.errors)
        },
        state.warnings,
    )
}

fn collect_parameters<'a>(
    compile_options: &CompileOptions<'a>,
    grammar: &'a Grammar<'a>,
    state: &mut State<'a>,
) {
    let mut duplicate: Option<&Node<'a>> = None;

    for node in &grammar.nodes {
        match &node.data {
            NodeData::Parameters(params) => {
                if let Some(duplicate) = duplicate {
                    state.errors.push(Error::from_str(
                        "Syntax may only contain a single set of syntax parameters",
                        node,
                        vec![
                            (duplicate, "first declared here".to_string()),
                            (node, "duplicate found here".to_string()),
                        ]));
                    continue;
                }

                duplicate = Some(node);

                if params.is_empty() {
                    state.errors.push(Error::from_str(
                        "Empty syntax parameters",
                        node,
                        vec![(
                            node,
                            "leave out parameters if none are required"
                                .to_string(),
                        )],
                    ));
                    continue;
                }

                if params.len() != compile_options.arguments.len() {
                    state.errors.push(Error::from_str(
                        "Wrong number of arguments",
                        node,
                        vec![(
                            node,
                            format!(
                                "expected {} arguments, but was {} instead",
                                compile_options.arguments.len(),
                                params.len()
                            ),
                        )],
                    ));
                    continue;
                }

                for (param, arg) in
                    params.iter().zip(compile_options.arguments.iter())
                {
                    match &param.data {
                        NodeData::RegexTerminal { .. }
                        | NodeData::LiteralTerminal { .. } => {
                            state.errors.push(Error::from_str(
                                "Syntax parameters may only be variables",
                                node,
                                vec![(
                                    param,
                                    "got terminal instead".to_string(),
                                )],
                            ));
                        }
                        NodeData::Reference { parameters, options } => {
                            assert!(options.is_none());

                            if !parameters.is_none() {
                                state.errors.push(Error::from_str(
                                    "Syntax parameters must be plain variables",
                                    node,
                                    vec![(node, "has parameters".to_string())],
                                ));
                                continue;
                            }

                            if !is_variable_name(param.text) {
                                state.errors.push(Error::from_str(
                                    "Variables must be all upper-case",
                                    node,
                                    vec![(
                                        node,
                                        format!(
                                            "use '{}' instead",
                                            to_variable_name(param.text)
                                        ),
                                    )],
                                ));
                            }

                            let name = param.text;

                            if let Some(duplicate) =
                                state.definitions.get(&(&name, 0))
                            {
                                assert!(
                                    duplicate.kind == DefinitionKind::Variable
                                );
                                assert!(duplicate.overloads.len() == 1);

                                state.errors.push(Error::from_str(
                                    "Duplicate parameters are not allowed in syntax parameters",
                                    node,
                                    vec![
                                        (duplicate.overloads[0], "first parameter here".to_string()),
                                        (node, "conflicts with this one".to_string()),
                                    ]));
                                continue;
                            }

                            // Keep track of the definition, but also store the value
                            state.definitions.insert(
                                (name, 0),
                                Definition {
                                    kind: DefinitionKind::Variable,
                                    overloads: vec![param],
                                },
                            );

                            let value = Value::String {
                                regex: arg.to_string(),
                                literal: arg.to_string(),
                                node: None,
                            };
                            state.variables.insert(name, value);
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

fn collect_definitions<'a>(grammar: &'a Grammar<'a>, state: &mut State<'a>) {
    for node in &grammar.nodes {
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
                    state.errors.push(Error::from_str(
                        "Variables must be all upper-case",
                        node,
                        vec![(
                            node,
                            format!("use '{}' instead", to_variable_name(name)),
                        )],
                    ));
                    continue;
                } else if kind == DefinitionKind::Rule && !is_rule_name(name) {
                    state.errors.push(Error::from_str(
                        "Rules must be all lower-case",
                        node,
                        vec![(
                            node,
                            format!("use '{}' instead", to_rule_name(name)),
                        )],
                    ));
                    continue;
                }

                let num_params = parameters.as_ref().map_or(0, |node| {
                    if let Node { data: NodeData::Parameters(v), .. } =
                        node.as_ref()
                    {
                        v.len()
                    } else {
                        panic!();
                    }
                });

                let key = (name, num_params as u8);

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
                    state.errors.push(Error::new(
                        format!("'{}' has already been defined", name),
                        node,
                        vec![
                            (
                                definition.overloads[0],
                                "already defined here".to_string(),
                            ),
                            (
                                node,
                                "conflicts with first definition".to_string(),
                            ),
                        ],
                    ));
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
