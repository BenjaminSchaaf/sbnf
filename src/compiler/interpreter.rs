use std::collections::{HashMap, HashSet};

use super::analysis::{parse_scope, Analysis, Metadata};
use super::analysis;
use super::common::{trim_ascii, Value, CallStack, Error, RuleOptions, CompileOptions, CompileResult};
use crate::sbnf::{is_identifier_char, Node, NodeData};
use crate::sublime_syntax;

pub struct Interpreted<'a> {
    pub rules: HashMap<RuleKey<'a>, Rule<'a>>,
    pub entry_points: Vec<RuleKey<'a>>,
    pub metadata: Metadata,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RuleKey<'a> {
    pub name: &'a str,
    pub arguments: Vec<Value<'a>>,
}

impl<'a> std::fmt::Display for RuleKey<'a> {
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
    pub embed: Option<String>,
    pub prototype: Option<RuleKey<'a>>,
}

pub enum Expression<'a> {
    Variable {
        key: RuleKey<'a>,
        node: &'a Node<'a>,
    },
    Terminal {
        regex: String,
        options: TerminalOptions<'a>,
        node: &'a Node<'a>,
    },
    Passive {
        expression: Box<Expression<'a>>,
        node: &'a Node<'a>,
    },
    Repetition {
        expression: Box<Expression<'a>>,
        node: &'a Node<'a>,
    },
    Optional {
        expression: Box<Expression<'a>>,
        node: &'a Node<'a>,
    },
    Alternation {
        expressions: Vec<Expression<'a>>,
        node: &'a Node<'a>,
    },
    Concatenation {
        expressions: Vec<Expression<'a>>,
        node: &'a Node<'a>,
    },
}

impl<'a> PartialEq for Expression<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Variable { key, .. }, Expression::Variable { key: k, .. })
                => key == k,
            (Expression::Terminal { regex, options, .. }, Expression::Terminal { regex: r, options: o, .. })
                => regex == r && options == o,
            (Expression::Passive { expression, .. }, Expression::Passive { expression: e, .. })
            | (Expression::Repetition { expression, .. }, Expression::Repetition { expression: e, .. })
            | (Expression::Optional { expression, .. }, Expression::Optional { expression: e, .. })
                => expression == e,
            (Expression::Alternation { expressions, .. }, Expression::Alternation { expressions: e, .. })
            | (Expression::Concatenation { expressions, .. }, Expression::Concatenation { expressions: e, .. })
                => expressions == e,
            _ => false,
        }
    }
}
impl<'a> Eq for Expression<'a> {}

impl<'a> std::hash::Hash for Expression<'a> {
    fn hash<H : std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Expression::Variable { key, .. } => key.hash(state),
            Expression::Terminal { regex, .. } => regex.hash(state),
            Expression::Passive { expression, .. } => {
                1.hash(state);
                expression.hash(state);
            },
            Expression::Repetition { expression, .. } => {
                2.hash(state);
                expression.hash(state);
            },
            Expression::Optional { expression, .. } => {
                3.hash(state);
                expression.hash(state);
            },
            Expression::Alternation { expressions, .. } => {
                4.hash(state);
                expressions.hash(state);
            },
            Expression::Concatenation { expressions, .. } => {
                5.hash(state);
                expressions.hash(state);
            },
        }
    }
}

impl<'a> std::fmt::Debug for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Variable { key, .. } => write!(f, "{}", key),
            Expression::Terminal { regex, .. } => write!(f, "'{}'", regex),
            Expression::Passive { expression, .. } => write!(f, "~{:?}", expression),
            Expression::Repetition { expression, .. } => write!(f, "{:?}*", expression),
            Expression::Optional { expression, .. } => write!(f, "{:?}?", expression),
            Expression::Alternation { expressions, .. } => {
                write!(f, "({:?}", expressions[0])?;
                for expression in expressions {
                    write!(f, " | {:?}", expression)?;
                }
                write!(f, ")")
            },
            Expression::Concatenation { expressions, .. } => {
                write!(f, "({:?}", expressions[0])?;
                for expression in expressions {
                    write!(f, " {:?}", expression)?;
                }
                write!(f, ")")
            },
        }
    }
}

struct State<'a> {
    rule_keys: HashSet<RuleKey<'a>>,
    rules: HashMap<RuleKey<'a>, Rule<'a>>,
    stack: CallStack<'a>,
    errors: Vec<Error<'a>>,
    warnings: Vec<Error<'a>>,
}

type VarMap<'a> = HashMap<&'a str, Value<'a>>;

pub fn interpret<'a>(options: &CompileOptions<'a>, analysis: Analysis<'a>) -> CompileResult<'a, Interpreted<'a>> {
    let arguments = options.arguments.iter()
                           .map(|arg| Value::String { regex: arg, literal: arg, node: None })
                           .collect::<Vec<_>>();

    let mut state = State {
        rule_keys: HashSet::new(),
        rules: HashMap::new(),
        stack: CallStack::new(),
        errors: vec!(),
        warnings: vec!(),
    };

    let mut entry_points = vec!();

    for entry_point in &options.entry_points {
        if analysis.rules.contains_key(entry_point) {
            let key = RuleKey { name: entry_point, arguments: arguments.clone() };

            interpret_rule(
                &mut state,
                &analysis,
                None,
                &key);

            entry_points.push(key);
        }
    }

    CompileResult::new(
        if state.errors.is_empty() {
            Ok(Interpreted {
                rules: state.rules,
                entry_points,
                metadata: analysis.metadata,
            })
        } else {
            Err(state.errors)
        },
        state.warnings,
    )
}

const STACK_SIZE_LIMIT: usize = 500;

fn interpret_rule<'a>(state: &mut State<'a>, analysis: &Analysis<'a>, variable: Option<&'a Node<'a>>, key: &RuleKey<'a>) {
    // Nothing to do if the key has been seen before
    if state.rule_keys.contains(key) {
        return;
    }

    // Find rule matching key
    let rules =
        if let Some(rules) = analysis.rules.get(key.name) {
            rules
        } else {
            state.errors.push(state.stack.error(
                format!("Could not file rule {}", key.name),
                variable.unwrap(),
                vec!(
                    (variable.unwrap(), "Unknown rule".to_string()),
                )));
            return;
        };

    let mut matching_rules = rules.iter().filter_map(|r| match_rule(analysis, r, &key.arguments));

    if let Some((rule, var_map)) = matching_rules.next() {
        // Check for conflicting match
        if let Some((dup_rule, _)) = matching_rules.next() {
            let mut comments = vec!(
                (rule.node, "This rule matches the arguments".to_string()),
                (dup_rule.node, "This rule also matches the arguments".to_string()),
            );
            if let Some(variable) = variable {
                comments.push((variable, "Instantiated from here".to_string()));
            }
            state.errors.push(state.stack.error(
                format!("Ambiguous rule instantiation for {}", key),
                rule.node,
                comments));
            return;
        }


        let is_new_key = state.rule_keys.insert(key.clone());
        assert!(is_new_key);

        let node = if let NodeData::Rule { node, .. } = &rule.node.data {
                node
            } else {
                panic!()
            };

        state.stack.push(variable, node, key.arguments.clone());

        // Limit the stack depth. Infinite loops can be constructed through
        // recursion.
        if state.stack.len() > STACK_SIZE_LIMIT {
            state.errors.push(state.stack.error_from_str(
                "Recursion depth limit reached",
                variable.unwrap(), // Only none when the stack size is 1
                vec!()));
            state.stack.pop();
            return;
        }

        let expression = interpret_expression(state, &var_map, analysis, node);

        if let Some(expression) = expression {
            let is_new_rule = state.rules.insert(key.clone(), Rule {
                options: rule.options.clone(),
                expression,
            });
            assert!(is_new_rule.is_none());
        }

        state.stack.pop();
    } else {
        let mut comments = vec!();
        for rule in rules {
            comments.push((rule.node, "Does not match".to_string()));
        }
        if let Some(variable) = variable {
            comments.push((variable, "Instantiated from here".to_string()));
        }
        state.errors.push(state.stack.error(
            format!("No matching rule instantiation for {}", key),
            variable.unwrap_or(rules[0].node),
            comments));
    }
}

fn match_rule<'a, 'b>(analysis: &'b Analysis<'a>, rule: &'b analysis::Rule<'a>, arguments: &Vec<Value<'a>>) -> Option<(&'b analysis::Rule<'a>, VarMap<'a>)> {
    let parameters =
        if let NodeData::Rule { parameters, .. } = &rule.node.data {
            parameters
        } else {
            panic!()
        };

    if parameters.len() != arguments.len() {
        return None;
    }

    let mut var_map = HashMap::new();

    for (param, arg) in parameters.iter().zip(arguments.iter()) {
        // Check if the parameter is a unique name, else it refers to a rule and
        // needs to match the argument
        match param.data {
            NodeData::Variable { .. } => {
                if !analysis.rules.contains_key(param.text) {
                    // Check if there's another parameter with the same name. If
                    // so it needs to match that parameter's value. Otherwise
                    // add it to the var map.
                    if let Some(value) = var_map.get(param.text) {
                        if value != arg {
                            return None;
                        }
                    } else {
                        var_map.insert(param.text, arg.clone());
                        continue;
                    }
                }
            },
            _ => {}
        }

        let param_value = interpret_value(param);

        if param_value != *arg {
            return None;
        }
    }

    Some((rule, var_map))
}

fn interpret_value<'a>(node: &'a Node<'a>) -> Value<'a> {
    match &node.data {
        NodeData::RegexTerminal { options } => {
            assert!(options.is_empty());
            Value::String { regex: node.text, literal: node.text, node: Some(node) }
        },
        NodeData::LiteralTerminal { regex, options } => {
            assert!(options.is_empty());
            Value::String { regex: &regex, literal: node.text, node: Some(node) }
        },
        NodeData::Variable { parameters } => {
            assert!(parameters.is_empty());
            Value::Variable { name: node.text, node: node }
        },
        _ => {
            panic!();
        }
    }
}

// TODO: Tests
fn interpolate_string<'a>(state: &mut State<'a>, var_map: &VarMap<'a>, node: &'a Node<'a>, string: &'a str) -> String {
    let mut result = String::new();

    let mut iter = string.chars();

    while let Some(chr) = iter.next() {
        if chr == '\\' {
            match iter.next() {
                Some('#') => {
                    result.push('#');
                },
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
                    let mut variable = String::new();

                    loop {
                        match iter.next() {
                            Some(']') => {
                                break;
                            },
                            Some(c) => {
                                if is_identifier_char(c) {
                                    variable.push(c);
                                } else {
                                    // TODO: Add node offset to show the error at the character
                                    state.errors.push(state.stack.error(
                                        format!("Unexpected character '{}' in string interpolation", c),
                                        node,
                                        vec!()));
                                    return result;
                                }
                            },
                            None => {
                                state.errors.push(state.stack.error_from_str(
                                    "Expected ']' before the end of the string to end string interpolation",
                                    node,
                                    vec!()));
                                return result;
                            },
                        }
                    }

                    if let Some(value) = var_map.get::<str>(&variable) {
                        match value {
                            Value::Variable { node: var_node, .. } => {
                                state.errors.push(state.stack.error_from_str(
                                    "Can't interpolate rule",
                                    node,
                                    vec!(
                                        (node, format!("{} must refer to a string", variable)),
                                        (var_node, format!("{} is {}", variable, value)),
                                    )));
                            },
                            Value::String { regex, .. } => {
                                result.push_str(regex);
                            },
                        }
                    } else {
                        state.errors.push(state.stack.error_from_str(
                            "Variable in string interpolation not found",
                            node,
                            vec!(
                                (node, format!("{} must be defined as an argument to the rule", variable)),
                            )));
                    }
                },
                Some(next_chr) => {
                    result.push('#');
                    result.push(next_chr);
                },
                _ => {
                    break;
                }
            }
        } else {
            result.push(chr);
        }
    }

    result
}

fn interpret_expression<'a>(state: &mut State<'a>, var_map: &VarMap<'a>, analysis: &Analysis<'a>, node: &'a Node<'a>) -> Option<Expression<'a>> {
    match &node.data {
        NodeData::Variable { parameters } => {
            let name =
                if let Some(value) = var_map.get(node.text) {
                    match value {
                        Value::String { .. } => {
                            state.errors.push(state.stack.error_from_str(
                                "Can't mixin string as variable",
                                node,
                                vec!(
                                    (node, format!("Use '#[{}]' here instead", node.text)),
                                )));
                            return None;
                        },
                        Value::Variable { name, .. } => name,
                    }
                } else {
                    node.text
                };

            // TODO: string interpolation for arguments
            let arguments = parameters.iter().map(interpret_value).collect::<Vec<_>>();

            let key = RuleKey { name, arguments };
            interpret_rule(state, analysis, Some(node), &key);
            Some(Expression::Variable { key, node })
        },
        NodeData::RegexTerminal { options: node_options } => {
            let regex = interpolate_string(state, var_map, node, node.text);
            let options = parse_terminal_options(state, var_map, analysis, node_options);

            Some(Expression::Terminal { regex, options, node })
        },
        NodeData::LiteralTerminal { regex, options: node_options } => {
            let options = parse_terminal_options(state, var_map, analysis, node_options);
            Some(Expression::Terminal { regex: regex.clone(), options, node })
        },
        NodeData::Passive(child) => {
            if let Some(expression) = interpret_expression(state, var_map, analysis, child) {
                Some(Expression::Passive {
                    expression: Box::new(expression),
                    node,
                })
            } else {
                None
            }
        },
        NodeData::Repetition(child) => {
            if let Some(expression) = interpret_expression(state, var_map, analysis, child) {
                Some(Expression::Repetition {
                    expression: Box::new(expression),
                    node,
                })
            } else {
                None
            }
        },
        NodeData::Optional(child) => {
            if let Some(expression) = interpret_expression(state, var_map, analysis, child) {
                Some(Expression::Optional {
                    expression: Box::new(expression),
                    node,
                })
            } else {
                None
            }
        },
        NodeData::Alternation(children) => {
            let expressions = children.iter()
                .filter_map(|child| interpret_expression(state, var_map, analysis, child))
                .collect::<Vec<_>>();

            if expressions.len() != children.len() {
                None
            } else {
                Some(Expression::Alternation { expressions, node })
            }
        },
        NodeData::Concatenation(children) => {
            let expressions = children.iter()
                .filter_map(|child| interpret_expression(state, var_map, analysis, child))
                .collect::<Vec<_>>();

            if expressions.len() != children.len() {
                None
            } else {
                Some(Expression::Concatenation { expressions, node })
            }
        },
        _ => panic!()
    }
}

fn parse_terminal_options<'a>(state: &mut State<'a>, var_map: &VarMap<'a>, analysis: &Analysis<'a>, node_options: &'a Vec<Node<'a>>) -> TerminalOptions<'a> {
    let mut options = TerminalOptions {
        scope: sublime_syntax::Scope::empty(),
        captures: HashMap::new(),
        embed: None,
        prototype: None,
    };

    for (i, option) in node_options.iter().enumerate() {
        match &option.data {
            NodeData::PositionalArgument => {
                // A positional option may only appear at the start to
                // determine the scope
                if i == 0 {
                    let interpolated = interpolate_string(state, var_map, option, option.text);

                    options.scope = parse_scope(&analysis.metadata, &interpolated);
                } else {
                    state.errors.push(state.stack.error_from_str(
                        "Positional argument for terminal scope may only be the first argument",
                        option,
                        vec!()));
                }
            },
            NodeData::KeywordArgument(value_node) => {
                let key = trim_ascii(option.text);

                assert!(value_node.data == NodeData::KeywordArgumentValue);
                let value_text = trim_ascii(value_node.text);
                let value = interpolate_string(state, var_map, value_node, value_text);

                if options.embed.is_none() {
                    // The first set of keyword arguments determine captures
                    if let Some(group) = key.parse::<u16>().ok() {
                        if options.captures.contains_key(&group) {
                            // TODO: Improve error message
                            state.errors.push(state.stack.error_from_str(
                                "Duplicate capture group argument",
                                option,
                                vec!()));
                        } else {
                            options.captures.insert(group, parse_scope(&analysis.metadata, &value));
                        }
                    } else if key == "embed" {
                        options.embed = Some(value);
                    } else {
                        state.errors.push(state.stack.error_from_str(
                            "Expected either capture group number or 'embed' for keyword argument",
                            option,
                            vec!(
                                (option, format!("Got '{}' instead", option.text)),
                            )));
                    }
                } else if options.prototype.is_none() {
                    if key == "prototype" {
                        // TODO: Arguments?
                        let key = RuleKey { name: value_text, arguments: vec!() };

                        interpret_rule(state, analysis, Some(value_node), &key);
                        options.prototype = Some(key);
                    } else {
                        // TODO: Improve error message
                        state.errors.push(state.stack.error_from_str(
                            "Expected either 'prototype' keyword argument after 'embed' or nothing",
                            option,
                            vec!(
                                (option, format!("Got '{}' instead", option.text)),
                            )));
                    }
                } else {
                    state.errors.push(state.stack.error_from_str(
                        "Unexpected keyword argument",
                        option,
                        vec!(
                            (option, "There should be no arguments after 'prototype'".to_string()),
                        )));
                }
            },
            _ => panic!()
        }
    }

    options
}
