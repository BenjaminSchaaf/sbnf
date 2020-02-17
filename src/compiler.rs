/// This file implements a compiler from a SBNF grammar to sublime-syntax
use std::collections::HashMap;
use indexmap::IndexMap;

use crate::sbnf;
use crate::sbnf::{Node, NodeData};
use crate::sublime_syntax;

#[derive(Debug)]
struct Compiler<'a> {
    name: Option<String>,
    file_extensions: Option<String>,
    first_line_match: Option<String>,
    scope: Option<String>,
    hidden: Option<bool>,
    rules: HashMap<&'a str, Rule<'a>>,
    context_cache: HashMap<Context<'a>, String>,
    contexts: HashMap<String, sublime_syntax::Context>,
}

impl<'a> Compiler<'a> {
    fn new(grammar: &'a sbnf::Grammar<'a>) -> Result<Compiler<'a>, CompileError<'a>> {
        let mut compiler = Compiler {
            name: None,
            file_extensions: None,
            first_line_match: None,
            scope: None,
            hidden: None,
            rules: HashMap::new(),
            context_cache: HashMap::new(),
            contexts: HashMap::new(),
        };

        for node in &grammar.nodes {
            match node.data {
                NodeData::Header(_) => {
                    compiler.compile_header(&node)?;
                },
                NodeData::Rule { .. } => {
                    compiler.collect_rule(&node)?;
                },
                _ => {
                    panic!("Top level nodes should only be headers and rules");
                }
            }
        }

        Ok(compiler)
    }
}

#[derive(Debug)]
struct Rule<'a> {
    pattern: &'a Node<'a>,
    collected: bool,

    inner_context_count: usize,
    branch_point_count: usize,
    meta_scope: sublime_syntax::Scope,
    meta_include_prototype: bool,
}

#[derive(Debug)]
pub struct CompileError<'a> {
    error: String,
    node: Option<&'a Node<'a>>,
}

impl CompileError<'_> {
    fn new<'a>(err: String, node: Option<&'a Node<'a>>) -> CompileError<'a> {
        CompileError { error: err, node: node }
    }

    fn from_str<'a>(err: &str, node: Option<&'a Node<'a>>) -> CompileError<'a> {
        CompileError { error: err.to_string(), node: node }
    }

    pub fn fmt(&self, origin: &str, source: &str) -> String {
        match self.node {
            Some(node) => {
                format!("Compiler Error: {} ({}:{})\n\n{}", self.error, origin, node.location, node.location.fmt_source(source))
            },
            None => {
                format!("Compiler Error: {}", self.error)
            }
        }
    }
}

pub fn compile<'a>(name_hint: Option<&str>, grammar: &'a sbnf::Grammar<'a>) -> Result<sublime_syntax::Syntax, CompileError<'a>> {
    let mut compiler = Compiler::new(grammar)?;

    // Compile rules
    for entry_point in &["main", "prototype"] {
        compiler.compile_rule(entry_point)?;
    }

    // Propagate defaults. Use the name_hint as a default name
    let name = compiler.name.or_else(
            || name_hint.map(|s| trim_ascii(s).to_string())
        ).ok_or(CompileError::from_str("No syntax name provided", None))?;

    let extensions = compiler.file_extensions.map_or(vec!(),
        |s| s.split_ascii_whitespace()
             .map(|s| s.to_string())
             .collect::<Vec<String>>());

    let first_line_match = compiler.first_line_match.map(
        |r| sublime_syntax::Pattern::new(r));

    // Default scope to source.{name}
    let scope = compiler.scope.map_or_else(
        || sublime_syntax::Scope::new(
            vec!(format!("source.{}", name.to_lowercase()))),
        |s| parse_scope(&s));

    let hidden = compiler.hidden.unwrap_or(false);

    Ok(sublime_syntax::Syntax {
        name: name,
        file_extensions: extensions,
        first_line_match: first_line_match,
        scope: scope,
        hidden: hidden,
        variables: HashMap::new(),
        contexts: compiler.contexts,
    })
}

fn parse_scope(s: &str) -> sublime_syntax::Scope {
    let scopes = s.split_ascii_whitespace().map(|s| s.to_string()).collect::<Vec<String>>();

    sublime_syntax::Scope::new(scopes)
}

fn trim_ascii<'a>(s: &'a str) -> &'a str {
    s.trim_matches(|c: char| c.is_ascii_whitespace())
}

impl<'a> Compiler<'a> {
    fn compile_header(&mut self, node: &'a Node<'a>) -> Result<(), CompileError<'a>> {
        let value =
            if let NodeData::Header(value_node) = &node.data {
                value_node.text
            } else {
                panic!();
            };

        match node.text {
            "name" => {
                if self.name.is_some() {
                    return Err(CompileError::from_str("Duplicate 'name' header", Some(node)));
                }

                self.name = Some(trim_ascii(value).to_string());
            },
            "extensions" => {
                if self.file_extensions.is_some() {
                    return Err(CompileError::from_str("Duplicate 'extensions' header", Some(node)));
                }

                self.file_extensions = Some(value.to_string());
            },
            "first-line" => {
                if self.first_line_match.is_some() {
                    return Err(CompileError::from_str("Duplicate 'first-line' header", Some(node)));
                }

                self.first_line_match = Some(trim_ascii(value).to_string());
            },
            "scope" => {
                if self.scope.is_some() {
                    return Err(CompileError::from_str("Duplicate 'scope' header", Some(node)));
                }

                self.scope = Some(value.to_string());
            },
            "hidden" => {
                if self.hidden.is_some() {
                    return Err(CompileError::from_str("Duplicate 'hidden' header", Some(node)));
                }

                if let Some(v) = trim_ascii(value).parse::<bool>().ok() {
                    self.hidden = Some(v);
                } else {
                    return Err(CompileError::from_str("Expected either 'true' or 'false' for header 'hidden'", Some(node)));
                }
            },
            _ => {
                return Err(CompileError::new(format!("Unknown header '{}'", node.text), Some(node)));
            }
        }

        Ok(())
    }

    fn collect_rule(&mut self, node: &'a Node<'a>) -> Result<(), CompileError<'a>> {
        let name = node.text;

        if self.rules.contains_key(name) {
            return Err(CompileError::new(format!("Rule '{}' has already been defined", name), Some(node)));
        }

        let arguments: &'a Vec<Node<'a>>;
        let pattern: &'a Node<'a>;
        if let NodeData::Rule { arguments: a, node: n } = &node.data {
            arguments = &a;
            pattern = &n;
        } else {
            panic!();
        }

        // Parse arguments
        let mut meta_scope = sublime_syntax::Scope::empty();
        let mut meta_include_prototype: Option<bool> = None;

        for (i, argument) in arguments.iter().enumerate() {
            if i == 0 && argument.data == NodeData::PositionalArgument {
                meta_scope = parse_scope(argument.text);
            } else if argument.data == NodeData::PositionalArgument {
                return Err(CompileError::from_str(
                    "Rules may only have one positional argument specifying the meta scope", Some(argument)));
            } else if let NodeData::KeyworkArgument(value_node) = &argument.data {
                if argument.text == "include-prototype" {
                    if meta_include_prototype.is_none() {
                        if let Ok(v) = trim_ascii(value_node.text).parse::<bool>() {
                            meta_include_prototype = Some(v);
                        } else {
                            return Err(CompileError::from_str(
                                "Expected 'true' or 'false'", Some(&value_node)));
                        }
                    } else {
                        return Err(CompileError::from_str(
                            "Duplicate 'include-prototype' argument", Some(argument)));
                    }
                } else {
                    return Err(CompileError::from_str(
                        "Unknown argument", Some(argument)));
                }
            }
        }

        self.rules.insert(name, Rule {
            pattern: pattern,
            collected: false,

            inner_context_count: 0,
            branch_point_count: 0,
            meta_scope: meta_scope,
            meta_include_prototype: meta_include_prototype.unwrap_or(true),
        });

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ContextEnd<'a> {
    Match,
    None,
    Push(Box<Context<'a>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ContextMatch<'a> {
    node: &'a Node<'a>,
    child: Option<Box<ContextMatch<'a>>>,
    remaining: Vec<&'a Node<'a>>,
}

impl<'a> ContextMatch<'a> {
    fn terminal(&self) -> &'a Node<'a> {
        if let Some(child) = &self.child {
            child.terminal()
        } else {
            self.node
        }
    }

    fn rule(&self, top_level: &'a str) -> &'a str {
        if let Some(child) = &self.child {
            child.rule(self.node.text)
        } else {
            top_level
        }
    }

    fn clone_with_new_child(&self, child: ContextMatch<'a>) -> ContextMatch<'a> {
        ContextMatch {
            node: self.node,
            child: Some(Box::new(child)),
            remaining: self.remaining.clone(),
        }
    }
}

// TODO: Use identity for PartialEQ instead of checking actual equality.
// Requires use of interned strings.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context<'a> {
    matches: Vec<ContextMatch<'a>>,
    match_end: ContextEnd<'a>,
    allow_empty: bool,
    // Used for compiling branch points. Irrelevant for collection.
    branch_point: Option<String>,
}

impl<'a> Context<'a> {
    fn append(&mut self, mut other: Context<'a>) {
        self.matches.append(&mut other.matches);

        self.append_update_end(other.match_end, other.allow_empty);
    }

    fn append_update_end(&mut self, mut end: ContextEnd<'a>, allow_empty: bool) {
        self.allow_empty = self.allow_empty || allow_empty;

        match self.match_end {
            ContextEnd::Match => {
                self.match_end = end;
            },
            ContextEnd::None => {
                self.match_end =
                    match end {
                        ContextEnd::Match
                        | ContextEnd::None => ContextEnd::None,
                        ContextEnd::Push(ref mut _push) => {
                            // push.append_update_end(ContextEnd::None);
                            end
                        },
                    };
            },
            ContextEnd::Push(ref mut self_push) => {
                match end {
                    ContextEnd::Match | ContextEnd::None => {
                        // self_push.append_update_end(ContextEnd::None);
                    },
                    ContextEnd::Push(other_push) => {
                        self_push.append(*other_push);
                    },
                }
            }
        }
    }
}

impl<'a> Compiler<'a> {
    fn gen_branch_point(&mut self, rule_name: &str) -> String {
        let rule = self.rules.get_mut(rule_name).unwrap();
        let name = format!("{}@{}", rule_name, rule.inner_context_count);
        rule.inner_context_count += 1;
        name
    }

    fn gen_context_name(&mut self, rule_name: &str, branch_point: &Option<String>) -> String {
        let rule = self.rules.get_mut(rule_name).unwrap();
        let name = format!("{}|{}", rule_name, rule.inner_context_count);
        rule.inner_context_count += 1;

        if let Some(branch_point) = &branch_point {
            format!("{}|{}", name, branch_point)
        } else {
            name
        }
    }

    fn compile_rule(&mut self, name: &'a str) -> Result<(), CompileError<'a>> {
        let node = self.rules.get(name).unwrap().pattern;

        let mut ctx = self.collect_context_nodes(node)?;

        // Top level rules are always repetitions
        ctx.match_end = ContextEnd::None;
        ctx.allow_empty = false;

        assert!(self.context_cache.get(&ctx).is_none());
        self.context_cache.insert(ctx.clone(), name.to_string());

        self.compile_contexts(name, vec!((name.to_string(), ctx)))?;

        Ok(())
    }

    fn compile_contexts(&mut self, rule: &'a str, contexts: Vec<(String, Context<'a>)>) -> Result<(), CompileError<'a>> {
        assert!(!contexts.is_empty());
        let branch_point = contexts[0].1.branch_point.clone();

        // If we've got more than one context we must have a branch point
        assert!(contexts.len() == 1 || branch_point.is_some());

        let mut regexes: HashMap<&'a str, usize> = HashMap::new();
        let mut context_maps: Vec<IndexMap<&'a str, Vec<&ContextMatch<'a>>>> = vec!();

        for (_, context) in &contexts {
            let mut map: IndexMap<&'a str, Vec<&ContextMatch<'a>>> = IndexMap::new();

            for context_match in &context.matches {
                // TODO: Literal
                let regex = context_match.terminal().text;

                if let Some(c) = regexes.get_mut(regex) {
                    *c += 1;
                } else {
                    regexes.insert(regex, 1);
                }

                if let Some(m) = map.get_mut(regex) {
                    m.push(context_match);
                } else {
                    map.insert(regex, vec!(context_match));
                }
            }

            context_maps.push(map);
        };

        assert!(context_maps.len() == contexts.len());

        let mut next_contexts: Vec<(String, Context<'a>)> = vec!();

        for ((name, context), matches_map) in contexts.iter().zip(context_maps.iter()) {
            let mut patterns = vec!();

            for (regex, matches) in matches_map {
                let continue_branch = *regexes.get(regex).unwrap() > 1;

                if matches.len() == 1 {
                    let m = matches[0];

                    // Continue branch
                    if continue_branch {
                        let exit =
                            if let Some(mut ctx) = self.collect_branch_context(m)? {
                                ctx.branch_point = branch_point.clone();

                                if let Some(name) = self.context_cache.get(&ctx) {
                                    sublime_syntax::ContextChange::Set(vec!(name.clone()))
                                } else {
                                    let rule_name = m.rule(rule);
                                    let name = self.gen_context_name(rule_name, &branch_point);
                                    self.context_cache.insert(ctx.clone(), name.clone());

                                    next_contexts.push((name.clone(), ctx));

                                    sublime_syntax::ContextChange::Set(vec!(name))
                                }
                            } else {
                                sublime_syntax::ContextChange::Pop(1)
                            };

                        patterns.push(Compiler::compile_terminal(m.terminal(), exit)?);
                    } else {
                        patterns.push(self.compile_simple_match(rule, m)?);
                    }
                } else {
                    // TOOD: Handle continue_branch?

                    // Start new branch
                    let bp_name = self.gen_branch_point(rule);
                    let branch_point = Some(bp_name.clone());

                    let lookahead = format!("(?={})", regex);

                    let mut branches = vec!();

                    for (i, m) in matches.iter().enumerate() {
                        let rule_name = m.rule(rule);
                        let ctx_name = self.gen_context_name(rule_name, &branch_point);
                        branches.push(ctx_name.clone());

                        let next_name: Option<String>;

                        if let Some(mut ctx) = self.collect_branch_context(m)? {
                            if i != matches.len() - 1 {
                                ctx.branch_point = branch_point.clone();

                                assert!(self.context_cache.get(&ctx).is_none());
                            }

                            if let Some(name) = self.context_cache.get(&ctx) {
                                next_name = Some(name.clone());
                            } else {
                                let name = self.gen_context_name(rule_name, &branch_point);
                                self.context_cache.insert(ctx.clone(), name.clone());

                                next_contexts.push((name.clone(), ctx));
                                next_name = Some(name);
                            }
                        } else {
                            next_name = None;
                        }

                        let exit = if let Some(name) = next_name {
                                let set =
                                    match context.match_end {
                                        ContextEnd::None => vec!(name),
                                        _ => {
                                            self.ensure_pop2_compiled();
                                            vec!("pop-2".to_string(), name)
                                        }
                                    };

                                sublime_syntax::ContextChange::Set(set)
                            } else {
                                sublime_syntax::ContextChange::Pop(2)
                            };

                        self.contexts.insert(ctx_name, sublime_syntax::Context {
                            meta_scope: sublime_syntax::Scope::empty(),
                            meta_content_scope: sublime_syntax::Scope::empty(),
                            meta_include_prototype: false,
                            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                            matches: vec!(
                                Compiler::compile_terminal(m.terminal(), exit)?,
                            ),
                        });
                    }

                    patterns.push(sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::new(lookahead),
                        scope: sublime_syntax::Scope::empty(),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Branch(bp_name, branches),
                    }));
                }
            }

            if let Some(pattern) = self.compile_end_match(context) {
                patterns.push(pattern);
            }

            {
                let r = self.rules.get(rule).unwrap();

                assert!(self.contexts.get(name).is_none());
                self.contexts.insert(name.clone(), sublime_syntax::Context {
                    meta_scope: r.meta_scope.clone(),
                    meta_content_scope: sublime_syntax::Scope::empty(),
                    meta_include_prototype: r.meta_include_prototype,
                    clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                    matches: patterns,
                });
            }
        }

        if !next_contexts.is_empty() {
            self.compile_contexts(rule, next_contexts)?;
        }

        Ok(())
    }

    fn ensure_pop2_compiled(&mut self) {
        if self.contexts.get("pop-2").is_some() {
            return;
        }

        self.contexts.insert("pop-2".to_string(), sublime_syntax::Context {
            meta_scope: sublime_syntax::Scope::empty(),
            meta_content_scope: sublime_syntax::Scope::empty(),
            meta_include_prototype: false,
            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
            matches: vec!(
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str(""),
                    scope: sublime_syntax::Scope::empty(),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::Pop(2),
                }),
            ),
        });
    }

    fn compile_terminal(node: &'a Node<'a>, exit: sublime_syntax::ContextChange) -> Result<sublime_syntax::ContextPattern, CompileError<'a>> {
        let regex = node.get_regex();
        let arguments = node.get_arguments();

        let mut scope = sublime_syntax::Scope::empty();
        let mut captures: HashMap<u16, sublime_syntax::Scope> = HashMap::new();
        for (i, argument) in arguments.iter().enumerate() {
            if i == 0 && argument.data == NodeData::PositionalArgument {
                scope = parse_scope(argument.text);
            } else if argument.data == NodeData::PositionalArgument {
                return Err(CompileError::from_str(
                    "Positional argument for terminal scope may only be the first argument", Some(&argument)));
            } else if let NodeData::KeyworkArgument(value) = &argument.data {
                let key = trim_ascii(argument.text).parse::<u16>().ok().ok_or(
                    CompileError::from_str(
                        "Expected integer key for regex capture scope",
                        Some(argument)))?;

                assert!(value.data == NodeData::KeywordArgumentValue);
                if captures.contains_key(&key) {
                    return Err(CompileError::from_str(
                        "Duplicate keyword argument", Some(argument)));
                } else {
                    captures.insert(key, parse_scope(value.text));
                }
            } else {
                panic!();
            }
        }

        Ok(sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
            pattern: sublime_syntax::Pattern::new(regex.to_string()),
            scope: scope,
            captures: captures,
            change_context: exit,
        }))
    }

    fn compile_end_match(&mut self, context: &Context<'a>) -> Option<sublime_syntax::ContextPattern> {
        match context.match_end {
            ContextEnd::Match => Some(
                if context.allow_empty {
                    sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::from_str(r#"(?=\S)"#),
                        scope: sublime_syntax::Scope::empty(),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Pop(1),
                    }
                } else if let Some(branch_point) = &context.branch_point {
                    sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::from_str(r#"\S"#),
                        scope: sublime_syntax::Scope::empty(),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Fail(branch_point.clone()),
                    }
                } else {
                    sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::from_str(r#"\S"#),
                        scope: sublime_syntax::Scope::from_str(&["invalid.illegal"]),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Pop(1),
                    }
                }),
            ContextEnd::None => None,
            ContextEnd::Push(_) => panic!(),
        }.map(&sublime_syntax::ContextPattern::Match)
    }

    fn compile_simple_match(&mut self, rule: &'a str, _match: &ContextMatch<'a>) -> Result<sublime_syntax::ContextPattern, CompileError<'a>> {
        let mut set = vec!();
        let mut rule_name = rule;
        let mut current_match = _match;
        loop {
            if !current_match.remaining.is_empty() {
                let ctx = self.collect_context_nodes_concatenation(&current_match.remaining)?;

                if let Some(name) = self.context_cache.get(&ctx) {
                    set.push(name.clone());
                } else {
                    let name = self.gen_context_name(rule_name, &None);
                    self.context_cache.insert(ctx.clone(), name.clone());
                    set.push(name.clone());

                    self.compile_contexts(rule_name, vec!((name, ctx)))?;
                }
            }

            if let Some(child) = &current_match.child {
                rule_name = current_match.node.text;
                current_match = &child;
            } else {
                break;
            }
        }

        let exit = if set.is_empty() {
                sublime_syntax::ContextChange::Pop(1)
            } else {
                sublime_syntax::ContextChange::Set(set)
            };

        Compiler::compile_terminal(current_match.node, exit)
    }

    fn collect_branch_context(&mut self, _match: &ContextMatch<'a>) -> Result<Option<Context<'a>>, CompileError<'a>> {
        if let Some(child) = &_match.child {
            if let Some(mut context) = self.collect_branch_context(&child)? {
                let mut new_matches = vec!();
                for node in context.matches {
                    new_matches.push(_match.clone_with_new_child(node));
                }
                context.matches = new_matches;

                return Ok(Some(context))
            }
        }

        if _match.remaining.is_empty() {
            Ok(None)
        } else {
            Ok(Some(self.collect_context_nodes_concatenation(&_match.remaining)?))
        }
    }

    // Transform and collect nodes that the context for node needs to match
    fn collect_context_nodes(&mut self, node: &'a Node<'a>) -> Result<Context<'a>, CompileError<'a>> {
        Ok(match &node.data {
            NodeData::RegexTerminal { .. }
            | NodeData::LiteralTerminal { .. }
            | NodeData::Embed { .. } => {
                let term = ContextMatch { node: &node, child: None, remaining: vec!() };
                Context {
                    matches: vec!(term),
                    match_end: ContextEnd::Match,
                    allow_empty: false,
                    branch_point: None,
                }
            },
            NodeData::Variable => {
                let pattern = self.rules.get(node.text)
                                        .map(|r| r.pattern)
                                        .ok_or_else(||
                    CompileError::from_str("Variable references rule that has not been defined", Some(node)))?;

                let mut r = self.collect_context_nodes(pattern)?;

                let mut matches = vec!();
                for child in r.matches {
                    matches.push(ContextMatch {
                        node: &node,
                        child: Some(Box::new(child)),
                        remaining: vec!(),
                    });
                }
                r.matches = matches;

                let end =
                    match r.match_end {
                        ContextEnd::Match => ContextEnd::Match,
                        ContextEnd::None => ContextEnd::Push(Box::new(r.clone())),
                        ContextEnd::Push(_) => r.match_end.clone(),
                    };

                Context {
                    matches: r.matches,
                    match_end: end,
                    allow_empty: r.allow_empty,
                    branch_point: None,
                }
            },
            NodeData::Passive(node) => {
                let r = self.collect_context_nodes(&node)?;

                match r.match_end {
                    ContextEnd::Match => {},
                    _ => panic!(),
                }

                Context {
                    matches: r.matches,
                    match_end: ContextEnd::None,
                    allow_empty: r.allow_empty,
                    branch_point: None,
                }
            },
            NodeData::Optional(node) => {
                let r = self.collect_context_nodes(&node)?;

                match r.match_end {
                    ContextEnd::Match => {
                        Context {
                            matches: r.matches,
                            match_end: r.match_end,
                            allow_empty: true,
                            branch_point: None,
                        }
                    },
                    ContextEnd::None | ContextEnd::Push(_) => {
                        Context {
                            matches: vec!(),
                            match_end: ContextEnd::Push(Box::new(r)),
                            allow_empty: true,
                            branch_point: None,
                        }
                    },
                }
            },
            NodeData::Repetition(child) => {
                let r = self.collect_context_nodes(&child)?;

                let matches = r.matches.iter().map(|_match| {
                    let mut remaining = _match.remaining.clone();
                    remaining.push(node);

                    ContextMatch {
                        node: _match.node,
                        child: _match.child.clone(),
                        remaining: remaining,
                    }
                }).collect::<Vec<_>>();

                let end =
                    match r.match_end {
                        ContextEnd::Match => ContextEnd::Match,
                        ContextEnd::None => ContextEnd::None,
                        _ => panic!(),
                    };

                Context {
                    matches: matches,
                    match_end: end,
                    allow_empty: true,
                    branch_point: None,
                }
            },
            NodeData::Alternation(nodes) => {
                let mut result = Context {
                    matches: vec!(),
                    match_end: ContextEnd::Match,
                    allow_empty: false,
                    branch_point: None,
                };

                for node in nodes {
                    result.append(self.collect_context_nodes(&node)?);
                }

                result
            },
            NodeData::Concatenation(nodes) => {
                let n = nodes.iter().collect::<Vec<_>>();
                self.collect_context_nodes_concatenation(&n)?
            },
            _ => {
                panic!();
            },
        })
    }

    fn collect_context_nodes_concatenation(&mut self, nodes: &[&'a Node<'a>]) -> Result<Context<'a>, CompileError<'a>> {
        assert!(nodes.len() >= 1);
        if nodes.len() == 1 {
            return Ok(self.collect_context_nodes(nodes[0])?);
        }

        let first = nodes[0];
        let rest = &nodes[1..];

        let mut result = self.collect_context_nodes(first)?;

        for _match in &mut result.matches {
            _match.remaining.extend(rest.iter().map(|n| *n));
        }

        if !result.allow_empty {
            return Ok(result);
        }

        let mut next = self.collect_context_nodes_concatenation(rest)?;

        result.match_end =
            match result.match_end {
                ContextEnd::Match => {
                    match next.match_end {
                        ContextEnd::Match => ContextEnd::Match,
                        ContextEnd::None => {
                            ContextEnd::Push(Box::new(Context {
                                matches: next.matches.clone(),
                                match_end: ContextEnd::None,
                                allow_empty: next.allow_empty,
                                branch_point: None,
                            }))
                        },
                        _ => panic!(),
                    }
                },
                ContextEnd::None => {
                    match next.match_end {
                        ContextEnd::Match => {
                            ContextEnd::Push(Box::new(Context {
                                matches: result.matches.clone(),
                                match_end: ContextEnd::None,
                                allow_empty: next.allow_empty,
                                branch_point: None,
                            }))
                        },
                        ContextEnd::None => ContextEnd::None,
                        _ => panic!(),
                    }
                },
                _ => panic!(),
            };

        result.matches.append(&mut next.matches);
        result.allow_empty = next.allow_empty;
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    extern crate matches;
    use matches::assert_matches;

    use crate::compiler::*;

    fn collect_node<F>(source: &str, rule: &str, fun: F) where F: Fn(Context) -> () {
        let grammar = sbnf::parse(source).unwrap();

        let mut compiler = Compiler::new(&grammar).unwrap();
        let node = compiler.rules.get(rule).unwrap().pattern;

        let cn = compiler.collect_context_nodes(node).unwrap();
        fun(cn);
    }

    #[derive(Debug, Clone)]
    enum TestNode<'a> {
        Variable(&'a str),
        RegexTerminal(&'a str),
        LiteralTerminal(&'a str),
        Embed,
        Passive(Box<TestNode<'a>>),
        Repetition(Box<TestNode<'a>>),
        Optional(Box<TestNode<'a>>),
        Alternation(Vec<TestNode<'a>>),
        Concatenation(Vec<TestNode<'a>>),
    }

    impl<'a, 'b> PartialEq<TestNode<'a>> for Node<'b> {
        fn eq(&self, other: &TestNode<'a>) -> bool {
            match &self.data {
                NodeData::Variable => match other {
                    TestNode::Variable(name) => &self.text == name,
                    _ => false,
                },
                NodeData::RegexTerminal { .. } => match other {
                    TestNode::RegexTerminal(s) => &self.text == s,
                    _ => false,
                },
                NodeData::LiteralTerminal { .. } => match other {
                    TestNode::LiteralTerminal(s) => &self.text == s,
                    _ => false,
                },
                NodeData::Embed { .. } => match other {
                    TestNode::Embed => true,
                    _ => false,
                },
                NodeData::Passive(node) => match other {
                    TestNode::Passive(other_node) => **node == **other_node,
                    _ => false,
                },
                NodeData::Repetition(node) => match other {
                    TestNode::Repetition(other_node) => **node == **other_node,
                    _ => false,
                },
                NodeData::Optional(node) => match other {
                    TestNode::Optional(other_node) => **node == **other_node,
                    _ => false,
                },
                NodeData::Alternation(node) => match other {
                    TestNode::Alternation(other_node) => *node == *other_node,
                    _ => false,
                },
                NodeData::Concatenation(node) => match other {
                    TestNode::Concatenation(other_node) => *node == *other_node,
                    _ => false,
                },
                _ => panic!(),
            }
        }
    }

    fn variable<'a>(s: &'a str) -> TestNode<'a> { TestNode::Variable(s) }
    fn regex<'a>(s: &'a str) -> TestNode<'a> { TestNode::RegexTerminal(s) }
    fn literal<'a>(s: &'a str) -> TestNode<'a> { TestNode::LiteralTerminal(s) }
    fn embed<'a>() -> TestNode<'a> { TestNode::Embed }
    fn passive<'a>(n: TestNode<'a>) -> TestNode<'a> {
        TestNode::Passive(Box::new(n))
    }
    fn repetition<'a>(n: TestNode<'a>) -> TestNode<'a> {
        TestNode::Repetition(Box::new(n))
    }
    fn optional<'a>(n: TestNode<'a>) -> TestNode<'a> {
        TestNode::Optional(Box::new(n))
    }
    fn alternation<'a>(ns: &[TestNode<'a>]) -> TestNode<'a> {
        TestNode::Alternation(ns.to_vec())
    }
    fn concatenation<'a>(ns: &[TestNode<'a>]) -> TestNode<'a> {
        TestNode::Concatenation(ns.to_vec())
    }

    #[derive(Debug, Clone)]
    struct TestContextMatch<'a> {
        node: String,
        child: Option<Box<TestContextMatch<'a>>>,
        remaining: Vec<TestNode<'a>>,
    }

    impl<'a, 'b> PartialEq<TestContextMatch<'a>> for ContextMatch<'b> {
        fn eq(&self, other: &TestContextMatch<'a>) -> bool {
            (if let Some(child) = &self.child {
                self.node.text == other.node
                    && other.child.is_some()
                    && **child == **other.child.as_ref().unwrap()
            } else {
                self.node.get_regex() == other.node
            }) && self.remaining.iter().map(|&n| n).eq(other.remaining.iter())
        }
    }

    fn m_term<'a>(s: &str) -> TestContextMatch<'a> {
        TestContextMatch {
            node: s.to_string(),
            child: None,
            remaining: vec!(),
        }
    }
    fn m_concat<'a>(s: &str, r: &[TestNode<'a>]) -> TestContextMatch<'a> {
        TestContextMatch {
            node: s.to_string(),
            child: None,
            remaining: r.to_vec(),
        }
    }
    fn m_var<'a>(s: &str, child: TestContextMatch<'a>, r: &[TestNode<'a>]) -> TestContextMatch<'a> {
        TestContextMatch {
            node: s.to_string(),
            child: Some(Box::new(child)),
            remaining: r.to_vec(),
        }
    }

    #[test]
    fn collect_passive() {
        collect_node("m = ~'a';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert_eq!(cn.matches, [
                m_term("a"),
            ]);
        });

        collect_node("m = ~'a' 'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[regex("b")]),
            ]);
        });

        collect_node("m = ~'a'* ~'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[repetition(regex("a")), passive(regex("b"))]),
                m_term("b"),
            ]);
        });

        collect_node("m = (~'a')* ~'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[repetition(passive(regex("a"))), passive(regex("b"))]),
                m_term("b"),
            ]);
        });

        collect_node("m = ~('a' | 'b') 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[regex("c")]),
                m_concat("b", &[regex("c")]),
            ]);
        });

        collect_node("m = ~'a'?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a"),
            ]);
        });

        collect_node("m = (~'a')?;", "m", |cn| {
            match cn.match_end {
                ContextEnd::Push(cn2) => {
                    assert_matches!(cn2.match_end, ContextEnd::None);
                    assert!(!cn2.allow_empty);
                    assert_eq!(cn2.matches, [
                        m_term("a"),
                    ]);
                },
                _ => panic!(),
            }
            assert!(cn.allow_empty);
            assert!(cn.matches.is_empty());
        });

        collect_node("m = (~'a')* 'b';", "m", |cn| {
            match cn.match_end {
                ContextEnd::Push(cn2) => {
                    assert_matches!(cn2.match_end, ContextEnd::None);
                    assert!(!cn2.allow_empty);
                    assert_eq!(cn2.matches, [
                        m_concat("a", &[repetition(passive(regex("a"))), regex("b")]),
                    ]);
                },
                _ => panic!(),
            }
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[repetition(passive(regex("a"))), regex("b")]),
                m_term("b"),
            ]);
        });

        collect_node("m = 'a'? ~'b';", "m", |cn| {
            match cn.match_end {
                ContextEnd::Push(cn2) => {
                    assert_matches!(cn2.match_end, ContextEnd::None);
                    assert!(!cn2.allow_empty);
                    assert_eq!(cn2.matches, [
                        m_term("b"),
                    ]);
                },
                _ => panic!(),
            }
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[passive(regex("b"))]),
                m_term("b"),
            ]);
        });
    }

    #[test]
    fn collect_repetition() {
        collect_node("m = 'a'*;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[repetition(regex("a"))]),
            ]);
        });

        collect_node("m = ('a'? 'b' | 'c')*;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[
                    regex("b"),
                    repetition(alternation(&[
                        concatenation(&[
                            optional(regex("a")),
                            regex("b"),
                        ]),
                        regex("c"),
                    ])),
                ]),
                m_concat("b", &[
                    repetition(alternation(&[
                        concatenation(&[
                            optional(regex("a")),
                            regex("b"),
                        ]),
                        regex("c"),
                    ])),
                ]),
                m_concat("c", &[
                    repetition(alternation(&[
                        concatenation(&[
                            optional(regex("a")),
                            regex("b"),
                        ]),
                        regex("c"),
                    ])),
                ]),
            ]);
        });
    }

    #[test]
    fn collect_optional() {
        collect_node("m = 'a'?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a"),
            ]);
        });

        collect_node("m = ('a' | 'b'* 'c')?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a"),
                m_concat("b", &[repetition(regex("b")), regex("c")]),
                m_term("c"),
            ]);
        });
    }

    #[test]
    fn collect_alternation() {
        collect_node("m = 'a' | 'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a"),
                m_term("b"),
            ]);
        });

        collect_node("m = 'a' | 'b' 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a"),
                m_concat("b", &[regex("c")]),
            ]);
        });

        collect_node("m = 'a'? | 'b' | 'c'*;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a"),
                m_term("b"),
                m_concat("c", &[repetition(regex("c"))]),
            ]);
        });
    }

    #[test]
    fn collect_concat() {
        collect_node("m = 'a' 'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[regex("b")]),
            ]);
        });

        collect_node("m = ('a' | 'b') 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[regex("c")]),
                m_concat("b", &[regex("c")]),
            ]);
        });

        collect_node("m = 'a'? 'b'* 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[repetition(regex("b")), regex("c")]),
                m_concat("b", &[repetition(regex("b")), regex("c")]),
                m_term("c"),
            ]);
        });

        collect_node("m = 'a'* 'b'?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_concat("a", &[repetition(regex("a")), optional(regex("b"))]),
                m_term("b"),
            ]);
        });
    }
}
