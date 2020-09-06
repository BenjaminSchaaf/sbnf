use std::collections::{HashMap, HashSet};

use super::analysis::{parse_scope, Analysis, Metadata};
// use super::analysis;
use super::common::{
    trim_ascii, var_maps_get, CallStack, CompileOptions, CompileResult, Error,
    RuleOptions, Value, VarMap,
};
use crate::sbnf::{Node, NodeData};
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
        prototype: RuleKey<'a>,
    },
    None,
}

pub enum Expression<'a> {
    Variable { key: RuleKey<'a>, node: &'a Node<'a> },
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
    rule_keys: HashSet<RuleKey<'a>>,
    rules: HashMap<RuleKey<'a>, Rule<'a>>,
    stack: CallStack<'a>,
    errors: Vec<Error<'a>>,
    warnings: Vec<Error<'a>>,
}

pub fn interpret<'a>(
    options: &CompileOptions<'a>,
    analysis: Analysis<'a>,
) -> CompileResult<'a, Interpreted<'a>> {
    let mut state = State {
        rule_keys: HashSet::new(),
        rules: HashMap::new(),
        stack: CallStack::new(),
        errors: vec![],
        warnings: vec![],
    };

    let mut entry_points = vec![];

    for entry_point in &options.entry_points {
        if analysis.rules.contains_key(entry_point) {
            let key = RuleKey { name: entry_point, arguments: vec![] };

            interpret_rule(&mut state, &analysis, None, &key);

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

fn interpret_rule<'a>(
    state: &mut State<'a>,
    analysis: &Analysis<'a>,
    variable: Option<&'a Node<'a>>,
    key: &RuleKey<'a>,
) {
    // Nothing to do if the key has been seen before
    if state.rule_keys.contains(key) {
        return;
    }

    // Find rule matching key
    let rules = if let Some(rules) = analysis.rules.get(key.name) {
        rules
    } else {
        state.errors.push(state.stack.error(
            format!("Could not find rule {}", key.name),
            variable.unwrap(),
            vec![(variable.unwrap(), "Unknown rule".to_string())],
        ));
        return;
    };

    let mut matching_rules =
        rules.iter().filter_map(|r| match_rule(analysis, r, &key.arguments));

    if let Some((rule, var_map)) = matching_rules.next() {
        // Check for conflicting match
        if let Some((dup_rule, _)) = matching_rules.next() {
            let mut comments = vec![
                (rule, "This rule matches the arguments".to_string()),
                (dup_rule, "This rule also matches the arguments".to_string()),
            ];
            if let Some(variable) = variable {
                comments.push((variable, "Instantiated from here".to_string()));
            }
            state.errors.push(state.stack.error(
                format!("Ambiguous rule instantiation for {}", key),
                rule,
                comments,
            ));
            return;
        }

        let is_new_key = state.rule_keys.insert(key.clone());
        assert!(is_new_key);

        let (expression_node, options_node) =
            if let NodeData::Rule { node, options, .. } = &rule.data {
                (node, options)
            } else {
                panic!()
            };

        state.stack.push(variable, rule, key.arguments.clone());

        // Limit the stack depth. Infinite loops can be constructed through
        // recursion.
        if state.stack.len() > STACK_SIZE_LIMIT {
            state.errors.push(state.stack.error_from_str(
                "Recursion depth limit reached",
                variable.unwrap(), // Only none when the stack size is 1
                vec![],
            ));
            state.stack.pop();
            return;
        }

        let options = parse_rule_options(
            state,
            &var_map,
            analysis,
            key.name,
            options_node,
        );

        let expression =
            interpret_expression(state, &var_map, analysis, expression_node);

        if let Some(expression) = expression {
            let is_new_rule =
                state.rules.insert(key.clone(), Rule { options, expression });
            assert!(is_new_rule.is_none());
        }

        state.stack.pop();
    } else {
        let mut comments: Vec<(&'a Node<'a>, String)> = vec![];
        for rule in rules {
            comments.push((rule, "Does not match".to_string()));
        }
        if let Some(variable) = &variable {
            comments.push((variable, "Instantiated from here".to_string()));
        }
        state.errors.push(state.stack.error(
            format!("No matching rule instantiation for {}", key),
            variable.unwrap_or(rules[0]),
            comments,
        ));
    }
}

fn match_rule<'a, 'b>(
    analysis: &'b Analysis<'a>,
    rule: &'a Node<'a>,
    arguments: &Vec<Value<'a>>,
) -> Option<(&'a Node<'a>, VarMap<'a>)> {
    let parameters = if let NodeData::Rule { parameters, .. } = &rule.data {
        parameters
    } else {
        panic!()
    };

    if parameters.len() != arguments.len() {
        return None;
    }

    let mut var_map = HashMap::new();

    for (param, arg) in parameters.iter().zip(arguments.iter()) {
        let value = interpret_value(&var_map, analysis, param);

        if let Some(value) = value {
            if value != *arg {
                return None;
            }
        } else {
            var_map.insert(param.text, arg.clone());
        }
    }

    Some((rule, var_map))
}

fn interpret_value<'a>(
    var_map: &VarMap<'a>,
    analysis: &Analysis<'a>,
    node: &'a Node<'a>,
) -> Option<Value<'a>> {
    match &node.data {
        NodeData::RegexTerminal { options, embed } => {
            assert!(options.is_empty());
            assert!(embed.is_none());
            Some(Value::String {
                regex: node.text,
                literal: node.text,
                node: Some(node),
            })
        }
        NodeData::LiteralTerminal { regex, options, embed } => {
            assert!(options.is_empty());
            assert!(embed.is_none());
            Some(Value::String {
                regex: &regex,
                literal: node.text,
                node: Some(node),
            })
        }
        NodeData::Variable { parameters } => {
            assert!(parameters.is_empty());

            if analysis.rules.contains_key(node.text) {
                Some(Value::Rule { name: node.text, node: node })
            } else if let Some(value) =
                var_maps_get(&[var_map, &analysis.variables], node.text)
            {
                Some(value.clone())
            } else {
                None
            }
        }
        _ => {
            panic!();
        }
    }
}

fn interpret_expression<'a>(
    state: &mut State<'a>,
    var_map: &VarMap<'a>,
    analysis: &Analysis<'a>,
    node: &'a Node<'a>,
) -> Option<Expression<'a>> {
    match &node.data {
        NodeData::Variable { parameters } => {
            let name = if let Some(value) =
                var_maps_get(&[var_map, &analysis.variables], node.text)
            {
                match value {
                    Value::String { .. } => {
                        state.errors.push(state.stack.error_from_str(
                            "Can't mixin string as variable",
                            node,
                            vec![(
                                node,
                                format!("Use '#[{}]' here instead", node.text),
                            )],
                        ));
                        return None;
                    }
                    Value::Rule { name, .. } => name,
                }
            } else {
                node.text
            };

            // TODO: string interpolation for arguments
            let mut arguments = vec![];
            for param in parameters {
                if let Some(value) = interpret_value(var_map, analysis, param) {
                    arguments.push(value);
                } else {
                    state.errors.push(state.stack.error_from_str(
                        "Undefined variable",
                        param,
                        vec![(
                                node,
                                "wasn't declared and doesn't refer to a rule"
                                    .to_string(),
                            )],
                    ));
                }
            }

            let key = RuleKey { name, arguments };
            interpret_rule(state, analysis, Some(node), &key);
            Some(Expression::Variable { key, node })
        }
        NodeData::RegexTerminal { options: node_options, embed } => {
            let regex =
                interpolate_string(state, analysis, var_map, node, node.text);
            let options = parse_terminal_options(
                state,
                var_map,
                analysis,
                node_options,
                embed,
            );

            Some(Expression::Terminal { regex, options, node })
        }
        NodeData::LiteralTerminal { regex, options: node_options, embed } => {
            let options = parse_terminal_options(
                state,
                var_map,
                analysis,
                node_options,
                embed,
            );
            Some(Expression::Terminal { regex: regex.clone(), options, node })
        }
        NodeData::Passive(child) => {
            if let Some(expression) =
                interpret_expression(state, var_map, analysis, child)
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
                interpret_expression(state, var_map, analysis, child)
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
                interpret_expression(state, var_map, analysis, child)
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
                    interpret_expression(state, var_map, analysis, child)
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
                    interpret_expression(state, var_map, analysis, child)
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

fn parse_terminal_options<'a>(
    state: &mut State<'a>,
    var_map: &VarMap<'a>,
    analysis: &Analysis<'a>,
    node_options: &'a Vec<Node<'a>>,
    node_embed: &'a Option<Box<Node<'a>>>,
) -> TerminalOptions<'a> {
    let mut options = TerminalOptions {
        scope: sublime_syntax::Scope::empty(),
        captures: HashMap::new(),
        embed: parse_terminal_embed(state, var_map, analysis, node_embed),
    };

    for (i, option) in node_options.iter().enumerate() {
        match &option.data {
            NodeData::PositionalArgument => {
                // A positional option may only appear at the start to
                // determine the scope
                if i == 0 {
                    let interpolated = interpolate_string(
                        state,
                        analysis,
                        var_map,
                        option,
                        option.text,
                    );

                    options.scope =
                        parse_scope(&analysis.metadata, &interpolated);
                } else {
                    state.errors.push(state.stack.error_from_str(
                        "Positional argument for terminal scope may only be the first argument",
                        option,
                        vec!()));
                }
            }
            NodeData::KeywordArgument(value_node) => {
                let key = trim_ascii(option.text);

                assert!(value_node.data == NodeData::KeywordArgumentValue);
                let value_text = trim_ascii(value_node.text);
                let value = interpolate_string(
                    state, analysis, var_map, value_node, value_text,
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
                            parse_scope(&analysis.metadata, &value),
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

fn parse_rule_options<'a>(
    state: &mut State<'a>,
    var_map: &VarMap<'a>,
    analysis: &Analysis<'a>,
    name: &'a str,
    options: &'a Vec<Node<'a>>,
) -> RuleOptions {
    let mut scope = sublime_syntax::Scope::empty();
    let mut include_prototype: Option<(&'a Node<'a>, bool)> = None;

    if analysis.debug_contexts {
        scope.scopes.push(format!("{}.sbnf-dbg", name));
    }

    for (i, argument) in options.iter().enumerate() {
        if i == 0 && argument.data == NodeData::PositionalArgument {
            let interpolated = interpolate_string(
                state,
                analysis,
                var_map,
                argument,
                argument.text,
            );
            scope = parse_scope(&analysis.metadata, &interpolated);
        } else if argument.data == NodeData::PositionalArgument {
            state.errors.push(Error::from_str(
                "Rules may only have one positional argument specifying the meta scope",
                argument,
                vec!(
                    (argument, "this argument".to_string()),
                    (&options[0], "should be used here".to_string()),
                )));
        } else if let NodeData::KeywordArgument(value_node) = &argument.data {
            assert!(value_node.data == NodeData::KeywordArgumentValue);
            let value_text = trim_ascii(value_node.text);
            let value = interpolate_string(
                state, analysis, var_map, value_node, value_text,
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

fn parse_terminal_embed<'a>(
    state: &mut State<'a>,
    var_map: &VarMap<'a>,
    analysis: &Analysis<'a>,
    node_embed: &'a Option<Box<Node<'a>>>,
) -> TerminalEmbed<'a> {
    let node_embed = if let Some(n) = node_embed.as_ref() {
        n
    } else {
        return TerminalEmbed::None;
    };

    let (parameters, options) =
        if let NodeData::Embed { parameters, options } = &node_embed.data {
            (parameters, options)
        } else {
            panic!();
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
                analysis,
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
            NodeData::PositionalArgument => interpolate_string(
                state,
                analysis,
                var_map,
                &options[0],
                options[0].text,
            ),
            NodeData::KeywordArgument(value_node) => {
                let key = options[0].text;

                assert!(value_node.data == NodeData::KeywordArgumentValue);
                let value_text = interpolate_string(
                    state,
                    analysis,
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
                NodeData::PositionalArgument => {
                    // A positional option may only appear at the start to
                    // determine the scope
                    if i == 0 {
                        let interpolated = interpolate_string(
                            state,
                            analysis,
                            var_map,
                            option,
                            option.text,
                        );

                        embed_scope =
                            parse_scope(&analysis.metadata, &interpolated);
                    } else {
                        state.errors.push(state.stack.error_from_str(
                            "Positional argument for escape scope may only be the second argument",
                            option,
                            vec!()));
                    }
                }
                NodeData::KeywordArgument(value_node) => {
                    let key = trim_ascii(option.text);

                    assert!(value_node.data == NodeData::KeywordArgumentValue);
                    let value_text = trim_ascii(value_node.text);
                    let value = interpolate_string(
                        state, analysis, var_map, value_node, value_text,
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
                                parse_scope(&analysis.metadata, &value),
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

        let prototype = if let NodeData::Variable { .. } = parameters[0].data {
            let name = parameters[0].text;
            let key = RuleKey { name, arguments: vec![] };
            interpret_rule(state, analysis, Some(&parameters[0]), &key);
            key
        } else {
            state.errors.push(state.stack.error_from_str(
                "%include can only take a variable as an argument",
                node_embed,
                vec![(node_embed, "Given a variable instead".to_string())],
            ));
            return TerminalEmbed::None;
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
            NodeData::PositionalArgument => interpolate_string(
                state,
                analysis,
                var_map,
                &options[0],
                options[0].text,
            ),
            NodeData::KeywordArgument(value_node) => {
                let key = options[0].text;

                assert!(value_node.data == NodeData::KeywordArgumentValue);
                let value_text = interpolate_string(
                    state,
                    analysis,
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

fn interpolate_string<'a>(
    state: &mut State<'a>,
    analysis: &Analysis<'a>,
    var_map: &VarMap<'a>,
    node: &'a Node<'a>,
    string: &'a str,
) -> String {
    let (result, mut errors) = super::common::interpolate_string(
        &state.stack,
        &[var_map, &analysis.variables],
        node,
        string,
    );

    state.errors.append(&mut errors);

    result
}
