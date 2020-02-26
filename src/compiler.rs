/// This file implements a compiler from a SBNF grammar to sublime-syntax
use std::collections::HashMap;
use indexmap::IndexMap;

use crate::sbnf;
use crate::sbnf::{Node, NodeData};
use crate::sublime_syntax;

#[derive(Debug)]
struct Compiler<'a> {
    name: String,
    file_extensions: Vec<String>,
    first_line_match: Option<sublime_syntax::Pattern>,
    scope: sublime_syntax::Scope,
    hidden: bool,

    scope_postfix: String,
    rules: HashMap<&'a str, Rule<'a>>,
    context_cache: HashMap<Context<'a>, String>,
    contexts: HashMap<String, sublime_syntax::Context>,
    warnings: Vec<Error<'a>>,
}

struct SyntaxMetadata {
    name: Option<String>,
    file_extensions: Option<String>,
    first_line_match: Option<String>,
    scope: Option<String>,
    scope_postfix: Option<String>,
    hidden: Option<bool>,
}

impl<'a> SyntaxMetadata {
    fn compile_header(&mut self, node: &'a Node<'a>) -> Result<(), Error<'a>> {
        let value =
            if let NodeData::Header(value_node) = &node.data {
                value_node.text
            } else {
                panic!();
            };

        match node.text {
            "name" => {
                if self.name.is_some() {
                    return Err(Error::from_str("Duplicate 'name' header", Some(node)));
                }

                self.name = Some(trim_ascii(value).to_string());
            },
            "extensions" => {
                if self.file_extensions.is_some() {
                    return Err(Error::from_str("Duplicate 'extensions' header", Some(node)));
                }

                self.file_extensions = Some(value.to_string());
            },
            "first-line" => {
                if self.first_line_match.is_some() {
                    return Err(Error::from_str("Duplicate 'first-line' header", Some(node)));
                }

                self.first_line_match = Some(trim_ascii(value).to_string());
            },
            "scope" => {
                if self.scope.is_some() {
                    return Err(Error::from_str("Duplicate 'scope' header", Some(node)));
                }

                self.scope = Some(value.to_string());
            },
            "scope-postfix" => {
                if self.scope_postfix.is_some() {
                    return Err(Error::from_str("Duplicate 'scope-postfix' header", Some(node)));
                }

                self.scope_postfix = Some(trim_ascii(value).to_string());
            },
            "hidden" => {
                if self.hidden.is_some() {
                    return Err(Error::from_str("Duplicate 'hidden' header", Some(node)));
                }

                if let Some(v) = trim_ascii(value).parse::<bool>().ok() {
                    self.hidden = Some(v);
                } else {
                    return Err(Error::from_str("Expected either 'true' or 'false' for header 'hidden'", Some(node)));
                }
            },
            _ => {
                return Err(Error::new(format!("Unknown header '{}'", node.text), Some(node)));
            }
        }

        Ok(())
    }
}

impl<'a> Compiler<'a> {
    fn new(name_hint: Option<&str>, grammar: &'a sbnf::Grammar<'a>) -> Result<Compiler<'a>, Error<'a>> {
        let mut metadata = SyntaxMetadata {
            name: None,
            file_extensions: None,
            first_line_match: None,
            scope: None,
            scope_postfix: None,
            hidden: None,
        };

        // Collect headers
        for node in &grammar.nodes {
            match node.data {
                NodeData::Header(_) => {
                    metadata.compile_header(&node)?;
                },
                NodeData::Rule { .. } => {},
                _ => panic!(),
            }
        }

        // Use the name_hint as a default name
        let name = metadata.name.clone().or_else(
                || name_hint.map(|s| trim_ascii(s).to_string())
            ).ok_or(Error::from_str("No syntax name provided", None))?;

        // Default the scope postfix to the name lowercased
        let scope_postfix = metadata.scope_postfix.unwrap_or_else(|| name.to_lowercase());

        // Propagate defaults
        let file_extensions = metadata.file_extensions.map_or(vec!(),
            |s| s.split_ascii_whitespace()
                 .map(|s| s.to_string())
                 .collect::<Vec<String>>());

        let first_line_match = metadata.first_line_match.map(
            |r| sublime_syntax::Pattern::new(r));

        // Default scope to source.{name}
        let scope = metadata.scope.map_or_else(
            || sublime_syntax::Scope::new(
                vec!(format!("source.{}", name.to_lowercase()))),
            |s| parse_top_level_scope(&s));

        let hidden = metadata.hidden.unwrap_or(false);

        let mut compiler = Compiler {
            name,
            file_extensions,
            first_line_match,
            scope,
            hidden,
            scope_postfix,
            rules: HashMap::new(),
            context_cache: HashMap::new(),
            contexts: HashMap::new(),
            warnings: vec!(),
        };

        // Collect rules
        for node in &grammar.nodes {
            match node.data {
                NodeData::Header(_) => {},
                NodeData::Rule { .. } => {
                    compiler.collect_rule(&node)?;
                },
                _ => panic!(),
            }
        }

        Ok(compiler)
    }
}

#[derive(Debug)]
struct Rule<'a> {
    rule_node: &'a Node<'a>,
    pattern: &'a Node<'a>,
    used: bool,

    inner_context_count: usize,
    branch_point_count: usize,
    scope: sublime_syntax::Scope,
    include_prototype: bool,
}

#[derive(Debug)]
pub struct Error<'a> {
    error: String,
    node: Option<&'a Node<'a>>,
}

impl Error<'_> {
    fn new<'a>(err: String, node: Option<&'a Node<'a>>) -> Error<'a> {
        Error { error: err, node: node }
    }

    fn from_str<'a>(err: &str, node: Option<&'a Node<'a>>) -> Error<'a> {
        Error { error: err.to_string(), node: node }
    }

    pub fn fmt(&self, typ: &str, origin: &str, source: &str) -> String {
        match self.node {
            Some(node) => {
                format!("{}: {} ({}:{})\n\n{}", typ, self.error, origin, node.location, node.location.fmt_source(source))
            },
            None => {
                format!("{}: {}", typ, self.error)
            }
        }
    }
}

pub struct CompilerOptions {
    pub debug_contexts: bool,
}

pub struct CompilerOutput<'a> {
    pub syntax: sublime_syntax::Syntax,
    pub warnings: Vec<Error<'a>>,
}

pub fn compile<'a>(name_hint: Option<&str>, options: &CompilerOptions, grammar: &'a sbnf::Grammar<'a>) -> Result<CompilerOutput<'a>, Error<'a>> {
    let mut compiler = Compiler::new(name_hint, grammar)?;

    // When debugging contexts we simple add the rule name to each context
    if options.debug_contexts {
        for (name, rule) in &mut compiler.rules {
            rule.scope.scopes.insert(0, format!("sbnf.{}", name));
        }
    }

    // Compile rules
    for entry_point in &["main", "prototype"] {
        if compiler.rules.get(entry_point).is_some() {
            compiler.compile_rule(entry_point)?;
        }
    }

    // Add warmings for unused rules
    for (_, rule) in &compiler.rules {
        if !rule.used {
            compiler.warnings.push(
                Error::from_str("Unused rule", Some(rule.rule_node)));
        }
    }

    let syntax = sublime_syntax::Syntax {
        name: compiler.name,
        file_extensions: compiler.file_extensions,
        first_line_match: compiler.first_line_match,
        scope: compiler.scope,
        hidden: compiler.hidden,
        variables: HashMap::new(),
        contexts: compiler.contexts,
    };

    Ok(CompilerOutput {
        syntax: syntax,
        warnings: compiler.warnings,
    })
}

fn parse_top_level_scope(s: &str) -> sublime_syntax::Scope {
    sublime_syntax::Scope::new(
        s.split_ascii_whitespace().map(|s| s.to_string()).collect::<Vec<String>>())
}

fn trim_ascii<'a>(s: &'a str) -> &'a str {
    s.trim_matches(|c: char| c.is_ascii_whitespace())
}

impl<'a> Compiler<'a> {
    fn parse_scope(&self, s: &str) -> sublime_syntax::Scope {
        let mut s = parse_top_level_scope(s);
        for scope in &mut s.scopes {
            let postfix = &self.scope_postfix;
            if !postfix.is_empty() {
                scope.push('.');
                scope.push_str(postfix);
            }
        }
        s
    }

    fn collect_rule(&mut self, node: &'a Node<'a>) -> Result<(), Error<'a>> {
        let name = node.text;

        if self.rules.contains_key(name) {
            return Err(Error::new(format!("Rule '{}' has already been defined", name), Some(node)));
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
        let mut scope = sublime_syntax::Scope::empty();
        let mut include_prototype: Option<bool> = None;

        for (i, argument) in arguments.iter().enumerate() {
            if i == 0 && argument.data == NodeData::PositionalArgument {
                scope = self.parse_scope(argument.text);
            } else if argument.data == NodeData::PositionalArgument {
                return Err(Error::from_str(
                    "Rules may only have one positional argument specifying the meta scope", Some(argument)));
            } else if let NodeData::KeyworkArgument(value_node) = &argument.data {
                if trim_ascii(argument.text) == "include-prototype" {
                    if include_prototype.is_none() {
                        if let Ok(v) = trim_ascii(value_node.text).parse::<bool>() {
                            include_prototype = Some(v);
                        } else {
                            return Err(Error::from_str(
                                "Expected 'true' or 'false'", Some(&value_node)));
                        }
                    } else {
                        return Err(Error::from_str(
                            "Duplicate 'include-prototype' argument", Some(argument)));
                    }
                } else {
                    return Err(Error::from_str(
                        "Unknown argument", Some(argument)));
                }
            }
        }

        self.rules.insert(name, Rule {
            rule_node: node,
            pattern: pattern,
            used: false,

            inner_context_count: 0,
            branch_point_count: 0,
            scope: scope,
            include_prototype: include_prototype.unwrap_or(true),
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
enum ContextMatchData<'a> {
    Terminal {
        node: &'a Node<'a>,
    },
    Variable {
        name: &'a str,
        child: Box<ContextMatch<'a>>,
    },
    Repetition {
        child: Box<ContextMatch<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ContextMatch<'a> {
    data: ContextMatchData<'a>,
    remaining: Vec<&'a Node<'a>>,
}

impl<'a> ContextMatch<'a> {
    fn terminal(&self) -> &'a Node<'a> {
        match &self.data {
            ContextMatchData::Terminal { node } => node,
            ContextMatchData::Variable { child, .. }
            | ContextMatchData::Repetition { child } => child.terminal(),
        }
    }

    fn rule(&self, top_level: &'a str) -> &'a str {
        match &self.data {
            ContextMatchData::Terminal { .. } => top_level,
            ContextMatchData::Variable { name, child } => child.rule(name),
            ContextMatchData::Repetition { child } => child.rule(top_level),
        }
    }

    fn is_repetition(&self) -> bool {
        match &self.data {
            ContextMatchData::Repetition { .. } => true,
            _ => false,
        }
    }

    fn clone_with_new_child(&self, child: ContextMatch<'a>) -> ContextMatch<'a> {
        match &self.data {
            ContextMatchData::Variable { name, .. } => {
                ContextMatch {
                    data: ContextMatchData::Variable {
                        name,
                        child: Box::new(child),
                    },
                    remaining: self.remaining.clone(),
                }
            },
            ContextMatchData::Repetition { .. } => {
                ContextMatch {
                    data: ContextMatchData::Repetition {
                        child: Box::new(child),
                    },
                    remaining: self.remaining.clone(),
                }
            },
            _ => panic!(),
        }
    }
}

// TODO: Use identity for PartialEQ instead of checking actual equality.
// Requires use of interned strings.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ContextRule<'a> {
    name: &'a str,
    // Transparent rules aren't considered on the stack, but are still required
    // to apply include-prototype
    transparent: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context<'a> {
    rule: ContextRule<'a>,
    matches: Vec<ContextMatch<'a>>,
    match_end: ContextEnd<'a>,
    allow_empty: bool,
    // Used for compiling branch points. Irrelevant for collection.
    branch_point: Option<String>,
}

impl<'a> Context<'a> {
    fn append(&mut self, mut other: Context<'a>) {
        assert!(self.rule == other.rule);

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

    fn compile_rule(&mut self, name: &'a str) -> Result<(), Error<'a>> {
        // Mark rule as used
        self.rules.get_mut(name).unwrap().used = true;

        let node = self.rules.get(name).unwrap().pattern;

        let ctx = self.collect_context_nodes(ContextRule { name, transparent: false }, node)?;

        // Entry point rules don't pop
        // ctx.match_end = ContextEnd::None;
        // ctx.allow_empty = false;

        assert!(self.context_cache.get(&ctx).is_none());
        self.context_cache.insert(ctx.clone(), name.to_string());

        self.compile_contexts(vec!((name.to_string(), ctx)))?;

        Ok(())
    }

    fn compile_contexts(&mut self, contexts: Vec<(String, Context<'a>)>) -> Result<(), Error<'a>> {
        assert!(!contexts.is_empty());
        let branch_point = contexts[0].1.branch_point.clone();

        // If we've got more than one context we must have a branch point
        assert!(contexts.len() == 1 || branch_point.is_some());

        // Collect unique regexes and unique matches in each context. We use
        // unique regexes to determine which contexts need to continue, and we
        // use unique matches per context to determine when a branch needs to be
        // made.
        let mut regexes: HashMap<&'a str, usize> = HashMap::new();
        let mut context_maps: Vec<IndexMap<&'a str, Vec<&ContextMatch<'a>>>> = vec!();

        for (_, context) in &contexts {
            let mut map: IndexMap<&'a str, Vec<&ContextMatch<'a>>> = IndexMap::new();

            for context_match in &context.matches {
                let regex = context_match.terminal().get_regex();

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

        for (i, ((name, context), matches_map)) in contexts.iter().zip(context_maps.iter()).enumerate() {
            let rule = context.rule;

            let is_last = i == contexts.len() - 1;

            let mut patterns = vec!();

            for (regex, matches) in matches_map {
                let continue_branch = *regexes.get(regex).unwrap() > 1;

                if matches.len() == 1 {
                    let m = matches[0];

                    // Continue branch
                    if continue_branch {
                        let scope = self.scope_for_match_stack(rule, m);

                        let exit =
                            if let Some(mut ctx) = self.collect_branch_context(rule, m)? {
                                ctx.branch_point = branch_point.clone();

                                if let Some(name) = self.context_cache.get(&ctx) {
                                    sublime_syntax::ContextChange::Set(vec!(name.clone()))
                                } else {
                                    let rule_name = m.rule(rule.name);
                                    let name = self.gen_context_name(rule_name, &branch_point);
                                    self.context_cache.insert(ctx.clone(), name.clone());

                                    next_contexts.push((name.clone(), ctx));

                                    sublime_syntax::ContextChange::Set(vec!(name))
                                }
                            } else {
                                sublime_syntax::ContextChange::Pop(1)
                            };

                        patterns.push(self.compile_terminal(scope, m.terminal(), exit)?);
                    } else {
                        // End points of branch points need to use
                        let scope = if branch_point.is_some() {
                                self.scope_for_match_stack(rule, m)
                            } else {
                                sublime_syntax::Scope::empty()
                            };

                        patterns.push(self.compile_simple_match(scope, rule, context, m)?);
                    }
                } else {
                    // TOOD: Handle continue_branch?

                    // Start new branch
                    let bp_name = self.gen_branch_point(rule.name);
                    let branch_point = Some(bp_name.clone());

                    let lookahead = format!("(?={})", regex);

                    let mut branches = vec!();

                    for _match in matches {
                        // Determine if the branch is a simple repetition. If so
                        // we can ignore the repetition in the branch. This
                        // avoids context stack leaks in most cases.
                        let branch_match =
                            match &_match.data {
                                ContextMatchData::Repetition { child } => {
                                    if self.collect_context_nodes_concatenation(rule, &_match.remaining)? == *context {
                                        &**child
                                    } else {
                                        _match
                                    }
                                },
                                _ => _match
                            };

                        let rule_name = branch_match.rule(rule.name);
                        let ctx_name = self.gen_context_name(rule_name, &branch_point);
                        branches.push(ctx_name.clone());

                        let next_name: Option<String>;
                        let branch_rule = ContextRule { name: rule.name, transparent: true };

                        if let Some(mut ctx) = self.collect_branch_context(branch_rule, branch_match)? {
                            ctx.branch_point = branch_point.clone();

                            assert!(self.context_cache.get(&ctx).is_none());

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

                        let scope = self.scope_for_match_stack(branch_rule, branch_match);

                        let exit = if let Some(name) = next_name {
                                let push =
                                    // TODO: is_repetition or is_simple_repetition here?
                                    if context.match_end == ContextEnd::None || _match.is_repetition() {
                                        vec!(self.compile_pop(2), name)
                                    } else {
                                        vec!(self.compile_pop(3), name)
                                    };

                                // Using set in branch_point is broken, so we
                                // have to use push.
                                sublime_syntax::ContextChange::Push(push)
                            } else {
                                sublime_syntax::ContextChange::Pop(2)
                            };

                        self.contexts.insert(ctx_name, sublime_syntax::Context {
                            meta_scope: sublime_syntax::Scope::empty(),
                            meta_content_scope: sublime_syntax::Scope::empty(),
                            meta_include_prototype: false,
                            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                            matches: vec!(
                                self.compile_terminal(scope, branch_match.terminal(), exit)?,
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

            if let Some(pattern) = self.compile_end_match(context, is_last) {
                patterns.push(pattern);
            }

            {
                // Branch points have an "invalid" rule at the top of the stack
                let r = self.rules.get(rule.name).unwrap();

                let meta_content_scope = if !rule.transparent {
                        r.scope.clone()
                    } else {
                        sublime_syntax::Scope::empty()
                    };

                let meta_include_prototype = r.include_prototype;

                assert!(self.contexts.get(name).is_none());
                self.contexts.insert(name.clone(), sublime_syntax::Context {
                    meta_scope: sublime_syntax::Scope::empty(),
                    meta_content_scope,
                    meta_include_prototype,
                    clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                    matches: patterns,
                });
            }
        }

        if !next_contexts.is_empty() {
            self.compile_contexts(next_contexts)?;
        }

        Ok(())
    }

    fn compile_pop(&mut self, amount: u16) -> String {
        let name = format!("pop-{}", amount);

        if self.contexts.get(&name).is_some() {
            return name;
        }

        self.contexts.insert(name.clone(), sublime_syntax::Context {
            meta_scope: sublime_syntax::Scope::empty(),
            meta_content_scope: sublime_syntax::Scope::empty(),
            meta_include_prototype: false,
            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
            matches: vec!(
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str(""),
                    scope: sublime_syntax::Scope::empty(),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::Pop(amount),
                }),
            ),
        });

        name
    }

    fn scope_for_match_stack(&self, mut rule: ContextRule<'a>, mut _match: &ContextMatch<'a>) -> sublime_syntax::Scope {
        let mut scopes: Vec<String> = vec!();
        loop {
            if !rule.transparent {
                let r = self.rules.get(rule.name).unwrap();

                scopes.extend(r.scope.scopes.iter().cloned());
            }

            match &_match.data {
                ContextMatchData::Terminal { .. } => {
                    break;
                },
                ContextMatchData::Variable { child, name } => {
                    rule = ContextRule { name, transparent: false };
                    _match = &child;
                },
                ContextMatchData::Repetition { child } => {
                    _match = &child;
                },
            }
        }
        sublime_syntax::Scope::new(scopes)
    }

    fn compile_terminal(&self, mut scope: sublime_syntax::Scope, node: &'a Node<'a>, exit: sublime_syntax::ContextChange) -> Result<sublime_syntax::ContextPattern, Error<'a>> {
        let regex = node.get_regex();
        let arguments = node.get_arguments();

        let mut captures: HashMap<u16, sublime_syntax::Scope> = HashMap::new();
        for (i, argument) in arguments.iter().enumerate() {
            if i == 0 && argument.data == NodeData::PositionalArgument {
                let mut parsed_scope = self.parse_scope(argument.text);
                scope.scopes.append(&mut parsed_scope.scopes);
            } else if argument.data == NodeData::PositionalArgument {
                return Err(Error::from_str(
                    "Positional argument for terminal scope may only be the first argument", Some(&argument)));
            } else if let NodeData::KeyworkArgument(value) = &argument.data {
                let key = trim_ascii(argument.text).parse::<u16>().ok().ok_or(
                    Error::from_str(
                        "Expected integer key for regex capture scope",
                        Some(argument)))?;

                assert!(value.data == NodeData::KeywordArgumentValue);
                if captures.contains_key(&key) {
                    return Err(Error::from_str(
                        "Duplicate keyword argument", Some(argument)));
                } else {
                    captures.insert(key, self.parse_scope(value.text));
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

    fn compile_end_match(&mut self, context: &Context<'a>, is_last: bool) -> Option<sublime_syntax::ContextPattern> {
        match context.match_end {
            ContextEnd::Match => Some(
                if context.allow_empty {
                    sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::from_str(r#"(?=\S)"#),
                        scope: sublime_syntax::Scope::empty(),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Pop(1),
                    }
                } else if context.branch_point.is_some() && !is_last {
                    sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::from_str(r#"\S"#),
                        scope: sublime_syntax::Scope::empty(),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Fail(context.branch_point.as_ref().unwrap().clone()),
                    }
                } else {
                    sublime_syntax::Match {
                        pattern: sublime_syntax::Pattern::from_str(r#"\S"#),
                        scope: self.parse_scope("invalid.illegal"),
                        captures: HashMap::new(),
                        change_context: sublime_syntax::ContextChange::Pop(1),
                    }
                }),
            ContextEnd::None => None,
            ContextEnd::Push(_) => panic!(),
        }.map(&sublime_syntax::ContextPattern::Match)
    }

    fn compile_simple_match(&mut self, scope: sublime_syntax::Scope, rule: ContextRule<'a>, top_level_context: &Context<'a>, _match: &ContextMatch<'a>) -> Result<sublime_syntax::ContextPattern, Error<'a>> {
        let mut contexts: Vec<String>;

        if let ContextMatchData::Repetition { child: child_match } = &_match.data {
            assert!(!_match.remaining.is_empty());
            let ctx = self.collect_context_nodes_concatenation(rule, &_match.remaining)?;

            contexts = self.compile_simple_match_impl(rule, &child_match)?;

            if ctx == *top_level_context {
                // If the remaining of a top-level repetition leads to the same
                // context, then we have a simple repetition. We can just push
                // the child match.

                // Contexts are in reverse order
                contexts.reverse();

                let exit =
                    if contexts.is_empty() {
                        sublime_syntax::ContextChange::None
                    } else {
                        sublime_syntax::ContextChange::Push(contexts)
                    };

                return self.compile_terminal(scope, _match.terminal(), exit);
            } else {
                // Otherwise we have a complex repetition, which behaves the
                // same way as a regular match.
                if let Some(name) = self.context_cache.get(&ctx) {
                    contexts.push(name.to_string());
                } else {
                    let name = self.gen_context_name(rule.name, &None);
                    self.context_cache.insert(ctx.clone(), name.clone());
                    contexts.push(name.clone());

                    self.compile_contexts(vec!((name, ctx)))?;
                }
            }
        } else {
            contexts = self.compile_simple_match_impl(rule, _match)?;
        }

        // Contexts are in reverse order
        contexts.reverse();

        let exit =
            if contexts.is_empty() {
                sublime_syntax::ContextChange::Pop(1)
            } else {
                sublime_syntax::ContextChange::Set(contexts)
            };

        self.compile_terminal(scope, _match.terminal(), exit)
    }

    fn compile_simple_match_impl(&mut self, rule: ContextRule<'a>, _match: &ContextMatch<'a>) -> Result<Vec<String>, Error<'a>> {
        let mut contexts = match &_match.data {
                ContextMatchData::Terminal { .. } => vec!(),
                ContextMatchData::Variable { name, child: child_match } =>
                    self.compile_simple_match_impl(ContextRule { name, transparent: false }, child_match)?,
                ContextMatchData::Repetition { child: child_match } =>
                    self.compile_simple_match_impl(rule, child_match)?,
            };

        println!("{} {:?} {:?} {:?} {:?}", rule.name, _match.remaining.is_empty(), !rule.transparent, !_match.is_repetition(), !contexts.is_empty());
        if _match.remaining.is_empty() && !rule.transparent && !_match.is_repetition() {
            // If a match has no remaining nodes it can generally be ignored,
            // unless it has a meta scope and there are child matches that were
            // not ignored. In those cases we create a special meta context.
            // Meta scopes are ignored for transparent rules and repetitions.
            let meta_content_scope = self.rules.get(rule.name).unwrap().scope.clone();

            if !meta_content_scope.is_empty() {
                let rule_meta_ctx_name = format!("{}|meta", rule.name);

                if self.contexts.get(&rule_meta_ctx_name).is_none() {
                    self.contexts.insert(rule_meta_ctx_name.clone(), sublime_syntax::Context {
                        meta_scope: sublime_syntax::Scope::empty(),
                        meta_content_scope,
                        meta_include_prototype: true,
                        clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                        matches: vec!(sublime_syntax::ContextPattern::Match(sublime_syntax::Match{
                            pattern: sublime_syntax::Pattern::from_str(""),
                            scope: sublime_syntax::Scope::empty(),
                            captures: HashMap::new(),
                            change_context: sublime_syntax::ContextChange::Pop(1),
                        })),
                    });
                }

                contexts.push(rule_meta_ctx_name);
            }
        } else if !_match.remaining.is_empty() {
            let ctx = self.collect_context_nodes_concatenation(rule, &_match.remaining)?;

            if let Some(name) = self.context_cache.get(&ctx) {
                contexts.push(name.to_string());
            } else {
                let name = self.gen_context_name(rule.name, &None);
                self.context_cache.insert(ctx.clone(), name.clone());
                contexts.push(name.clone());

                self.compile_contexts(vec!((name, ctx)))?;
            }
        }

        Ok(contexts)
    }

    fn collect_branch_context(&mut self, rule: ContextRule<'a>, _match: &ContextMatch<'a>) -> Result<Option<Context<'a>>, Error<'a>> {
        match &_match.data {
            ContextMatchData::Terminal { .. } => {},
            ContextMatchData::Variable { child, .. }
            | ContextMatchData::Repetition { child } => {
                if let Some(mut context) = self.collect_branch_context(rule, &child)? {
                    let mut new_matches = vec!();
                    for node in context.matches {
                        new_matches.push(_match.clone_with_new_child(node));
                    }
                    context.matches = new_matches;

                    return Ok(Some(context))
                }
            }
        }

        if _match.remaining.is_empty() {
            Ok(None)
        } else {
            Ok(Some(self.collect_context_nodes_concatenation(rule, &_match.remaining)?))
        }
    }

    // Transform and collect nodes that the context for node needs to match
    fn collect_context_nodes(&mut self, rule: ContextRule<'a>, node: &'a Node<'a>) -> Result<Context<'a>, Error<'a>> {
        Ok(match &node.data {
            NodeData::RegexTerminal { .. }
            | NodeData::LiteralTerminal { .. }
            | NodeData::Embed { .. } => {
                Context {
                    rule,
                    matches: vec!(
                        ContextMatch {
                            data: ContextMatchData::Terminal { node },
                            remaining: vec!()
                        },
                    ),
                    match_end: ContextEnd::Match,
                    allow_empty: false,
                    branch_point: None,
                }
            },
            NodeData::Variable => {
                let pattern = self.rules.get(node.text)
                                        .map(|r| r.pattern)
                                        .ok_or_else(||
                    Error::from_str("Variable references rule that has not been defined", Some(node)))?;

                // Mark rule as used
                self.rules.get_mut(node.text).unwrap().used = true;

                let rl = ContextRule { name: node.text, transparent: false };
                let mut r = self.collect_context_nodes(rl, pattern)?;

                let mut matches = vec!();
                for child in r.matches {
                    matches.push(ContextMatch {
                        data: ContextMatchData::Variable {
                            name: node.text,
                            child: Box::new(child),
                        },
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
                    rule,
                    matches: r.matches,
                    match_end: end,
                    allow_empty: r.allow_empty,
                    branch_point: None,
                }
            },
            NodeData::Passive(node) => {
                let r = self.collect_context_nodes(rule, &node)?;

                match r.match_end {
                    ContextEnd::Match => {},
                    _ => panic!(),
                }

                Context {
                    rule,
                    matches: r.matches,
                    match_end: ContextEnd::None,
                    allow_empty: r.allow_empty,
                    branch_point: None,
                }
            },
            NodeData::Optional(node) => {
                let r = self.collect_context_nodes(rule, &node)?;

                match r.match_end {
                    ContextEnd::Match => {
                        Context {
                            rule,
                            matches: r.matches,
                            match_end: r.match_end,
                            allow_empty: true,
                            branch_point: None,
                        }
                    },
                    ContextEnd::None | ContextEnd::Push(_) => {
                        Context {
                            rule,
                            matches: vec!(),
                            match_end: ContextEnd::Push(Box::new(r)),
                            allow_empty: true,
                            branch_point: None,
                        }
                    },
                }
            },
            NodeData::Repetition(child) => {
                let r = self.collect_context_nodes(rule, &child)?;

                let mut matches = vec!();
                for child in r.matches {
                    matches.push(ContextMatch {
                        data: ContextMatchData::Repetition {
                            child: Box::new(child),
                        },
                        remaining: vec!(node),
                    });
                }

                let end =
                    match r.match_end {
                        ContextEnd::Match => ContextEnd::Match,
                        ContextEnd::None => ContextEnd::None,
                        _ => panic!(),
                    };

                Context {
                    rule,
                    matches: matches,
                    match_end: end,
                    allow_empty: true,
                    branch_point: None,
                }
            },
            NodeData::Alternation(nodes) => {
                let mut result = Context {
                    rule,
                    matches: vec!(),
                    match_end: ContextEnd::Match,
                    allow_empty: false,
                    branch_point: None,
                };

                for node in nodes {
                    result.append(self.collect_context_nodes(rule, &node)?);
                }

                result
            },
            NodeData::Concatenation(nodes) => {
                let n = nodes.iter().collect::<Vec<_>>();
                self.collect_context_nodes_concatenation(rule, &n)?
            },
            _ => {
                panic!();
            },
        })
    }

    fn collect_context_nodes_concatenation(&mut self, rule: ContextRule<'a>, nodes: &[&'a Node<'a>]) -> Result<Context<'a>, Error<'a>> {
        assert!(nodes.len() >= 1);
        if nodes.len() == 1 {
            return Ok(self.collect_context_nodes(rule, nodes[0])?);
        }

        let first = nodes[0];
        let rest = &nodes[1..];

        let mut result = self.collect_context_nodes(rule, first)?;

        for _match in &mut result.matches {
            _match.remaining.extend(rest.iter().map(|n| *n));
        }

        if !result.allow_empty {
            return Ok(result);
        }

        let mut next = self.collect_context_nodes_concatenation(rule, rest)?;

        result.match_end =
            match result.match_end {
                ContextEnd::Match => {
                    match next.match_end {
                        ContextEnd::Match => ContextEnd::Match,
                        ContextEnd::None => {
                            ContextEnd::Push(Box::new(Context {
                                rule,
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
                                rule,
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

        assert!(result.rule == rule);

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

        let mut compiler = Compiler::new(Some(rule), &grammar).unwrap();

        let node = compiler.rules.get(rule).unwrap().pattern;

        let cn = compiler.collect_context_nodes(ContextRule { name: rule, transparent: false }, node).unwrap();
        fun(cn);
    }

    #[derive(Debug, Clone)]
    enum TestNode {
        Variable(&'static str),
        RegexTerminal(&'static str),
        LiteralTerminal(&'static str),
        Embed,
        Passive(Box<TestNode>),
        Repetition(Box<TestNode>),
        Optional(Box<TestNode>),
        Alternation(Vec<TestNode>),
        Concatenation(Vec<TestNode>),
    }

    impl<'a> PartialEq<TestNode> for Node<'a> {
        fn eq(&self, other: &TestNode) -> bool {
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

    fn variable(s: &'static str) -> TestNode { TestNode::Variable(s) }
    fn regex(s: &'static str) -> TestNode { TestNode::RegexTerminal(s) }
    fn literal(s: &'static str) -> TestNode { TestNode::LiteralTerminal(s) }
    fn embed() -> TestNode { TestNode::Embed }
    fn passive(n: TestNode) -> TestNode {
        TestNode::Passive(Box::new(n))
    }
    fn repetition(n: TestNode) -> TestNode {
        TestNode::Repetition(Box::new(n))
    }
    fn optional(n: TestNode) -> TestNode {
        TestNode::Optional(Box::new(n))
    }
    fn alternation(ns: &[TestNode]) -> TestNode {
        TestNode::Alternation(ns.to_vec())
    }
    fn concatenation(ns: &[TestNode]) -> TestNode {
        TestNode::Concatenation(ns.to_vec())
    }

    #[derive(Debug, Clone)]
    enum TestContextMatchData {
        Terminal { regex: String },
        Variable { name: String, child: Box<TestContextMatch> },
        Repetition { child: Box<TestContextMatch> },
    }

    #[derive(Debug, Clone)]
    struct TestContextMatch {
        data: TestContextMatchData,
        remaining: Vec<TestNode>,
    }

    impl<'a> PartialEq<TestContextMatch> for ContextMatch<'a> {
        fn eq(&self, other: &TestContextMatch) -> bool {
            (match &self.data {
                ContextMatchData::Terminal { node } => {
                    match &other.data {
                        TestContextMatchData::Terminal { regex } => node.get_regex() == regex,
                        _ => false,
                    }
                },
                ContextMatchData::Variable { name, child } => {
                    match &other.data {
                        TestContextMatchData::Variable { name: oname, child: ochild } => *name == oname && **child == **ochild,
                        _ => false,
                    }
                },
                ContextMatchData::Repetition { child } => {
                    match &other.data {
                        TestContextMatchData::Repetition { child: ochild } => **child == **ochild,
                        _ => false,
                    }
                },
            }) && self.remaining.iter().map(|&n| n).eq(other.remaining.iter())
        }
    }

    fn m_term(s: &str, r: &[TestNode]) -> TestContextMatch {
        TestContextMatch {
            data: TestContextMatchData::Terminal { regex: s.to_string() },
            remaining: r.to_vec(),
        }
    }

    fn m_var(s: &str, child: TestContextMatch, r: &[TestNode]) -> TestContextMatch {
        TestContextMatch {
            data: TestContextMatchData::Variable { name: s.to_string(), child: Box::new(child) },
            remaining: r.to_vec(),
        }
    }

    fn m_rep(child: TestContextMatch, r: &[TestNode]) -> TestContextMatch {
        TestContextMatch {
            data: TestContextMatchData::Repetition { child: Box::new(child) },
            remaining: r.to_vec(),
        }
    }

    #[test]
    fn collect_passive() {
        collect_node("m = ~'a';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
            ]);
        });

        collect_node("m = ~'a' 'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[regex("b")]),
            ]);
        });

        collect_node("m = ~'a'* ~'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_rep(m_term("a", &[]), &[repetition(regex("a")), passive(regex("b"))]),
                m_term("b", &[]),
            ]);
        });

        collect_node("m = (~'a')* ~'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_rep(m_term("a", &[]), &[repetition(passive(regex("a"))), passive(regex("b"))]),
                m_term("b", &[]),
            ]);
        });

        collect_node("m = ~('a' | 'b') 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[regex("c")]),
                m_term("b", &[regex("c")]),
            ]);
        });

        collect_node("m = ~'a'?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::None);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
            ]);
        });

        collect_node("m = (~'a')?;", "m", |cn| {
            match cn.match_end {
                ContextEnd::Push(cn2) => {
                    assert_matches!(cn2.match_end, ContextEnd::None);
                    assert!(!cn2.allow_empty);
                    assert_eq!(cn2.matches, [
                        m_term("a", &[]),
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
                        m_rep(m_term("a", &[]), &[repetition(passive(regex("a"))), regex("b")]),
                    ]);
                },
                _ => panic!(),
            }
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_rep(m_term("a", &[]), &[repetition(passive(regex("a"))), regex("b")]),
                m_term("b", &[]),
            ]);
        });

        collect_node("m = 'a'? ~'b';", "m", |cn| {
            match cn.match_end {
                ContextEnd::Push(cn2) => {
                    assert_matches!(cn2.match_end, ContextEnd::None);
                    assert!(!cn2.allow_empty);
                    assert_eq!(cn2.matches, [
                        m_term("b", &[]),
                    ]);
                },
                _ => panic!(),
            }
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[passive(regex("b"))]),
                m_term("b", &[]),
            ]);
        });
    }

    #[test]
    fn collect_repetition() {
        collect_node("m = 'a'*;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_rep(m_term("a", &[]), &[repetition(regex("a"))]),
            ]);
        });

        collect_node("m = ('a'? 'b' | 'c')*;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            let rep = repetition(alternation(&[
                concatenation(&[optional(regex("a")), regex("b")]),
                regex("c"),
            ]));
            assert_eq!(cn.matches, [
                m_rep(m_term("a", &[regex("b")]), &[rep.clone()]),
                m_rep(m_term("b", &[]), &[rep.clone()]),
                m_rep(m_term("c", &[]), &[rep.clone()]),
            ]);
        });
    }

    #[test]
    fn collect_optional() {
        collect_node("m = 'a'?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
            ]);
        });

        collect_node("m = ('a' | 'b'* 'c')?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
                m_rep(m_term("b", &[]), &[repetition(regex("b")), regex("c")]),
                m_term("c", &[]),
            ]);
        });
    }

    #[test]
    fn collect_alternation() {
        collect_node("m = 'a' | 'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
                m_term("b", &[]),
            ]);
        });

        collect_node("m = 'a' | 'b' 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
                m_term("b", &[regex("c")]),
            ]);
        });

        collect_node("m = 'a'? | 'b' | 'c'*;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[]),
                m_term("b", &[]),
                m_rep(m_term("c", &[]), &[repetition(regex("c"))]),
            ]);
        });
    }

    #[test]
    fn collect_concat() {
        collect_node("m = 'a' 'b';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[regex("b")]),
            ]);
        });

        collect_node("m = ('a' | 'b') 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[regex("c")]),
                m_term("b", &[regex("c")]),
            ]);
        });

        collect_node("m = 'a'? 'b'* 'c';", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(!cn.allow_empty);
            assert_eq!(cn.matches, [
                m_term("a", &[repetition(regex("b")), regex("c")]),
                m_rep(m_term("b", &[]), &[repetition(regex("b")), regex("c")]),
                m_term("c", &[]),
            ]);
        });

        collect_node("m = 'a'* 'b'?;", "m", |cn| {
            assert_matches!(cn.match_end, ContextEnd::Match);
            assert!(cn.allow_empty);
            assert_eq!(cn.matches, [
                m_rep(m_term("a", &[]), &[repetition(regex("a")), optional(regex("b"))]),
                m_term("b", &[]),
            ]);
        });
    }

    fn compile_matches(source: &str) -> HashMap<String, sublime_syntax::Context> {
        let grammar = sbnf::parse(source).unwrap();

        let options = CompilerOptions { debug_contexts: false };
        let output = compile(Some("Test"), &options, &grammar).unwrap();

        assert!(output.warnings.is_empty());

        let mut buf = String::new();
        output.syntax.serialize(&mut buf).unwrap();
        println!("{}", buf);

        output.syntax.contexts
    }

    #[test]
    fn compile_simple_repetition() {
        let contexts = compile_matches("main = ('a'{a} 'b'{b})*;");
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(main.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("a"),
                scope: sublime_syntax::Scope::from_str(&["a.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!("main|0".to_string())),
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("(?=\\S)"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
        ]);
        let main0 = contexts.get("main|0").unwrap();
        assert_eq!(main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("b"),
                scope: sublime_syntax::Scope::from_str(&["b.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::from_str(&["invalid.illegal"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
        ]);
    }

    #[test]
    fn compile_simple_branch() {
        let contexts = compile_matches("main = (a | b)*; a{a} = 'c'{ac} 'a'; b{b} = 'c'{bc} 'b';");
        assert_eq!(contexts.len(), 6);
        let main = contexts.get("main").unwrap();
        assert_eq!(main.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("(?=c)"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Branch("main@0".to_string(), vec!("a|0|main@0".to_string(), "b|0|main@0".to_string())),
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("(?=\\S)"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
        ]);
        // First branch
        let a0main0 = contexts.get("a|0|main@0").unwrap();
        assert_eq!(a0main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("c"),
                scope: sublime_syntax::Scope::from_str(&["ac.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!("pop-2".to_string(), "a|1|main@0".to_string())),
            }),
        ]);
        let a1main0 = contexts.get("a|1|main@0").unwrap();
        assert_eq!(a1main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("a"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Fail("main@0".to_string()),
            }),
        ]);
        // Second branch
        let b0main0 = contexts.get("b|0|main@0").unwrap();
        assert_eq!(b0main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("c"),
                scope: sublime_syntax::Scope::from_str(&["bc.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!("pop-2".to_string(), "b|1|main@0".to_string())),
            }),
        ]);
        let b1main0 = contexts.get("b|1|main@0").unwrap();
        assert_eq!(b1main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("b"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::from_str(&["invalid.illegal"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Pop(1),
            }),
        ]);
    }
}
