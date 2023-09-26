use hashbrown::HashMap;

use super::common::{
    CompileOptions, CompileResult, Compiler, Error, Symbol, Value, VarMap,
};
use crate::sbnf::{Grammar, Node, NodeData};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DefinitionKind {
    Variable,
    Rule,
}

impl std::fmt::Display for DefinitionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DefinitionKind::Variable => write!(f, "variable"),
            DefinitionKind::Rule => write!(f, "rule"),
        }
    }
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

struct State<'a, 'b> {
    compiler: &'b Compiler,
    options: &'a CompileOptions<'a>,
    variables: VarMap,
    definitions: DefinitionMap<'a>,
    errors: Vec<Error>,
    warnings: Vec<Error>,
}

pub fn collect<'a>(
    compiler: &Compiler,
    options: &'a CompileOptions<'a>,
    grammar: &'a Grammar<'a>,
) -> CompileResult<Collection<'a>> {
    let mut state = State {
        compiler,
        options,
        variables: VarMap::new(),
        definitions: HashMap::new(),
        errors: vec![],
        warnings: vec![],
    };

    collect_parameters(grammar, &mut state);
    collect_definitions(grammar, &mut state);

    CompileResult::new(
        Collection {
            variables: state.variables,
            definitions: state.definitions,
        },
        state.errors,
        state.warnings,
    )
}

fn collect_parameters<'a, 'b>(
    grammar: &'a Grammar<'a>,
    state: &mut State<'a, 'b>,
) {
    let mut duplicate: Option<&Node<'a>> = None;

    for node in &grammar.nodes {
        match &node.data {
            NodeData::Parameters(params) => {
                if let Some(duplicate) = duplicate {
                    state.errors.push(Error::from_str(
                        "Syntax may only contain a single set of syntax parameters",
                        Some(node.location),
                        vec![
                            (duplicate.location, "first declared here".to_string()),
                            (node.location, "duplicate found here".to_string()),
                        ]));
                    continue;
                }

                duplicate = Some(node);

                if params.is_empty() {
                    state.errors.push(Error::from_str(
                        "Empty syntax parameters",
                        Some(node.location),
                        vec![(
                            node.location,
                            "leave out parameters if none are required"
                                .to_string(),
                        )],
                    ));
                    continue;
                }

                if params.len() != state.options.arguments.len() {
                    state.errors.push(Error::from_str(
                        "Wrong number of arguments",
                        Some(node.location),
                        vec![(
                            node.location,
                            format!(
                                "expected {} arguments, but was {} instead",
                                state.options.arguments.len(),
                                params.len()
                            ),
                        )],
                    ));
                    continue;
                }

                for (param, arg) in
                    params.iter().zip(state.options.arguments.iter())
                {
                    match &param.data {
                        NodeData::RegexTerminal { .. }
                        | NodeData::LiteralTerminal { .. } => {
                            state.errors.push(Error::from_str(
                                "Syntax parameters may only be variables",
                                Some(node.location),
                                vec![(
                                    param.location,
                                    "got terminal instead".to_string(),
                                )],
                            ));
                        }
                        NodeData::Reference { parameters, options } => {
                            assert!(options.is_none());

                            if !parameters.is_none() {
                                state.errors.push(Error::from_str(
                                    "Syntax parameters must be plain variables",
                                    Some(node.location),
                                    vec![(
                                        node.location,
                                        "has parameters".to_string(),
                                    )],
                                ));
                                continue;
                            }

                            if !is_valid_variable_name(param.text) {
                                state.errors.push(Error::from_str(
                                    "Variables must be all upper-case",
                                    Some(node.location),
                                    vec![(
                                        node.location,
                                        format!(
                                            "use '{}' instead",
                                            to_variable_name(param.text)
                                        ),
                                    )],
                                ));
                            }

                            let symbol = state.compiler.get_symbol(param.text);

                            if let Some(duplicate) =
                                state.definitions.get(&(symbol, 0))
                            {
                                assert!(
                                    duplicate.kind == DefinitionKind::Variable
                                );
                                assert!(duplicate.overloads.len() == 1);

                                state.errors.push(Error::from_str(
                                    "Duplicate parameters are not allowed in syntax parameters",
                                    Some(node.location),
                                    vec![
                                        (duplicate.overloads[0].location, "first parameter here".to_string()),
                                        (node.location, "conflicts with this one".to_string()),
                                    ]));
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

fn collect_definitions<'a, 'b>(
    grammar: &'a Grammar<'a>,
    state: &mut State<'a, 'b>,
) {
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

                if kind == DefinitionKind::Variable
                    && !is_valid_variable_name(name)
                {
                    state.errors.push(Error::from_str(
                        "Variables must be all upper-case",
                        Some(node.location),
                        vec![(
                            node.location,
                            format!("use '{}' instead", to_variable_name(name)),
                        )],
                    ));
                    continue;
                } else if kind == DefinitionKind::Rule
                    && !is_valid_rule_name(name)
                {
                    state.errors.push(Error::from_str(
                        "Rules must be all lower-case",
                        Some(node.location),
                        vec![(
                            node.location,
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

                let symbol = state.compiler.get_symbol(name);

                let key = (symbol, num_params as u8);

                let definition: &mut Definition<'a> = if let Some(def) =
                    state.definitions.get_mut(&key)
                {
                    def
                } else {
                    state
                        .definitions
                        .insert(key, Definition { kind, overloads: vec![] });

                    state.definitions.get_mut(&key).unwrap()
                };

                if definition.kind != kind {
                    state.errors.push(Error::new(
                        format!(
                            "'{}' must be either a rule or a variable",
                            name
                        ),
                        None,
                        vec![
                            (
                                definition.overloads[0].location,
                                format!(
                                    "defined as a {} here",
                                    definition.kind
                                ),
                            ),
                            (
                                node.location,
                                format!("defined as a {} here", kind),
                            ),
                        ],
                    ));
                    continue;
                }

                if num_params == 0 && !definition.overloads.is_empty() {
                    state.errors.push(Error::new(
                        format!("'{}' has already been defined", name),
                        Some(node.location),
                        vec![
                            (
                                definition.overloads[0].location,
                                "already defined here".to_string(),
                            ),
                            (
                                node.location,
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

fn is_valid_variable_name(name: &str) -> bool {
    return name.chars().all(|c| {
        c.is_ascii_uppercase()
            || c.is_ascii_digit()
            || c == '_'
            || !c.is_ascii()
    });
}

fn to_variable_name(name: &str) -> String {
    return name
        .chars()
        .map(|c| if c == '-' { '_' } else { c.to_ascii_uppercase() })
        .collect::<String>();
}

fn is_valid_rule_name(name: &str) -> bool {
    return name.chars().all(|c| {
        c.is_ascii_lowercase()
            || c.is_ascii_digit()
            || c == '-'
            || !c.is_ascii()
    });
}

fn to_rule_name(name: &str) -> String {
    return name
        .chars()
        .map(|c| if c == '_' { '-' } else { c.to_ascii_lowercase() })
        .collect::<String>();
}
