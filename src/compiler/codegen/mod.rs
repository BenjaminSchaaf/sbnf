use std::fmt::Write;

use base64;
use hashbrown::HashMap;
use indexmap::IndexMap;

use super::common::{parse_scope, Compiler, Symbol};
use super::interpreter::{Expression, Interpreted, Key, TerminalEmbed};
use crate::sublime_syntax;

pub mod lookahead;

use lookahead::{Lookahead, StackEntry, StackEntryData, Terminal};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BranchPoint<'a> {
    name: &'a str,
    can_fail: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ContextKey<'a> {
    rule_key: Key,
    is_top_level: bool,
    lookahead: Lookahead<'a>,
    branch_point: Option<BranchPoint<'a>>,
}

impl<'a> ContextKey<'a> {
    #[allow(dead_code)]
    fn with_compiler(
        &'a self,
        compiler: &'a Compiler,
    ) -> ContextKeyWithCompiler<'a> {
        ContextKeyWithCompiler { key: self, compiler }
    }
}

struct ContextKeyWithCompiler<'a> {
    key: &'a ContextKey<'a>,
    compiler: &'a Compiler,
}

impl std::fmt::Debug for ContextKeyWithCompiler<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self)?;
        for terminal in &self.key.lookahead.terminals {
            writeln!(f, "{:?}", terminal.with_compiler(self.compiler))?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ContextKeyWithCompiler<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.key.rule_key.with_compiler(self.compiler))?;
        if let Some(branch_point) = &self.key.branch_point {
            write!(f, " (branch point: '{}')", branch_point.name)?;
        }
        if self.key.is_top_level {
            write!(f, " top level")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Rule {
    context_count: usize,
    branch_point_count: usize,
    entry_context_count: usize,
}

struct State<'a> {
    compiler: &'a Compiler,
    rules: HashMap<Key, Rule>,
    context_queue: Vec<ContextKey<'a>>,
    context_cache: HashMap<ContextKey<'a>, &'a str>,
    include_context_cache: HashMap<ContextKey<'a>, &'a str>,
    entry_contexts: HashMap<&'a [&'a str], &'a str>,
    contexts: HashMap<&'a str, sublime_syntax::Context<'a>>,
}

pub fn codegen<'a>(
    compiler: &'a Compiler,
    interpreted: Interpreted<'a>,
) -> sublime_syntax::Syntax<'a> {
    let mut state = State {
        compiler,
        rules: HashMap::new(),
        context_queue: vec![],
        context_cache: HashMap::new(),
        include_context_cache: HashMap::new(),
        entry_contexts: HashMap::new(),
        contexts: HashMap::new(),
    };

    for rule_key in &interpreted.entry_points {
        gen_rule(&mut state, &interpreted, *rule_key);
    }

    while let Some(item) = state.context_queue.pop() {
        gen_contexts(&mut state, &interpreted, vec![item]);
    }

    let contexts = compiler.allocator.alloc_slice_fill_iter(state.contexts);
    contexts.sort_by_key(|v| v.0);

    sublime_syntax::Syntax {
        name: interpreted.metadata.name,
        file_extensions: interpreted.metadata.file_extensions,
        first_line_match: interpreted.metadata.first_line_match.clone(),
        scope: interpreted.metadata.scope,
        hidden: interpreted.metadata.hidden,
        variables: &[],
        contexts,
    }
}

fn lookahead_rule<'a>(
    state: &State<'a>,
    interpreted: &Interpreted<'a>,
    rule_key: Key,
) -> lookahead::Lookahead<'a> {
    let rule = &interpreted.rules[&rule_key];

    let mut lookahead_state = lookahead::LookaheadState::new(state.compiler);
    lookahead_state.push_variable(rule_key);

    let mut lookahead = lookahead::lookahead(
        interpreted,
        rule.expression,
        &mut lookahead_state,
    );

    lookahead_state.pop_variable(rule_key, &mut lookahead);

    lookahead
}

fn gen_rule<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    rule_key: Key,
) {
    let lookahead = lookahead_rule(state, interpreted, rule_key);

    let context_key = ContextKey {
        rule_key,
        is_top_level: true,
        lookahead,
        branch_point: None,
    };

    let name = rule_key.get_name(state.compiler);

    let old_entry = state.context_cache.insert(context_key.clone(), name);
    assert!(old_entry.is_none());

    state.context_queue.push(context_key);
}

fn count_duplicate_regexes<'a, I>(iter: I) -> HashMap<Symbol, usize>
where
    I: std::iter::Iterator<Item = &'a Lookahead<'a>>,
{
    let mut map = HashMap::new();
    for lookahead in iter {
        for terminal in &lookahead.terminals {
            if let Some(count) = map.get_mut(&terminal.regex) {
                *count += 1;
            } else {
                map.insert(terminal.regex, 1);
            }
        }
    }
    map
}

fn index_terminals(lookahead: &Lookahead<'_>) -> IndexMap<Symbol, Vec<usize>> {
    let mut map = IndexMap::<Symbol, Vec<usize>>::new();
    for (i, terminal) in lookahead.terminals.iter().enumerate() {
        if let Some(m) = map.get_mut(&terminal.regex) {
            m.push(i);
        } else {
            map.insert(terminal.regex, vec![i]);
        }
    }
    map
}

/*
*/
fn gen_contexts<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    contexts: Vec<ContextKey<'a>>,
) {
    assert!(!contexts.is_empty());
    if contexts.len() > 1 {
        assert!(contexts.iter().all(|c| c.branch_point.is_some()));
    }

    // println!("GEN CONTEXTS {}", contexts.len());
    // for (name, context_key) in &contexts {
    //     println!("GEN {} {:?}", name, context_key.with_compiler(state.compiler));
    // }

    let regexes =
        count_duplicate_regexes(contexts.iter().map(|c| &c.lookahead));

    let mut next_contexts: Vec<ContextKey<'_>> = vec![];

    for context_key in contexts {
        let name = state.context_cache[&context_key];

        let mut patterns = vec![];

        let rule_key = context_key.rule_key;
        let is_top_level = context_key.is_top_level;
        let lookahead = context_key.lookahead;
        let branch_point = context_key.branch_point;

        let meta_content_scope: sublime_syntax::Scope;
        let meta_include_prototype: bool;
        let capture: bool = false;
        {
            // Branch points have an "invalid" rule at the top of the stack
            let rule = interpreted.rules.get(&rule_key).unwrap();

            meta_content_scope = if branch_point.is_none() && is_top_level {
                rule.options.scope
            } else {
                sublime_syntax::Scope::EMPTY
            };

            meta_include_prototype = rule.options.include_prototype;
            // capture = rule.options.capture && !branch_point.is_some();
        }

        for (regex, terminal_indexes) in index_terminals(&lookahead) {
            let continue_branch = *regexes.get(&regex).unwrap() > 1;

            if terminal_indexes.len() == 1 {
                let terminal = &lookahead.terminals[terminal_indexes[0]];

                // Continue branch
                if continue_branch {
                    let scope = scope_for_match_stack(
                        state,
                        interpreted,
                        Some(rule_key),
                        terminal,
                    );

                    let exit = if let Some(lookahead) =
                        lookahead::advance_terminal(
                            interpreted,
                            terminal,
                            state.compiler,
                        ) {
                        let next_key = ContextKey {
                            rule_key,
                            is_top_level,
                            lookahead: lookahead.clone(),
                            branch_point: branch_point.clone(),
                        };

                        let name = if let Some(entry) =
                            state.context_cache.get(&next_key)
                        {
                            entry
                        } else {
                            let name =
                                create_context_name(state, next_key.clone());

                            next_contexts.push(next_key);
                            name
                        };

                        sublime_syntax::ContextChange::PushOne(name)
                    } else {
                        sublime_syntax::ContextChange::None
                    };

                    patterns.push(gen_terminal(
                        state,
                        interpreted,
                        name,
                        rule_key,
                        &meta_content_scope,
                        &branch_point,
                        scope,
                        terminal,
                        exit,
                        1,
                    ));
                } else {
                    let scope = scope_for_match_stack(
                        state,
                        interpreted,
                        if branch_point.is_some() {
                            Some(rule_key)
                        } else {
                            None
                        },
                        terminal,
                    );

                    patterns.push(gen_simple_match(
                        state,
                        interpreted,
                        name,
                        rule_key,
                        is_top_level,
                        &meta_content_scope,
                        &branch_point,
                        &lookahead,
                        scope,
                        terminal,
                    ));
                }
            } else {
                // Start a branch point or use an existing one
                let branch_point_name: &str;
                let include_context_name: &str;
                {
                    // TODO: No need for context.end, context.empty or
                    // branch_point in this struct.
                    let key = ContextKey {
                        rule_key,
                        is_top_level,
                        lookahead: Lookahead {
                            terminals: terminal_indexes
                                .iter()
                                .map(|i| lookahead.terminals[*i].clone())
                                .collect::<Vec<_>>(),
                            end: lookahead::End::None,
                            empty: true,
                        },
                        branch_point: None,
                    };

                    if let Some(include_context_name) =
                        state.include_context_cache.get(&key)
                    {
                        patterns.push(sublime_syntax::ContextPattern::Include(
                            include_context_name,
                        ));
                        continue;
                    } else {
                        // Start new branch
                        branch_point_name =
                            create_branch_point_name(state, rule_key);

                        include_context_name =
                            create_branch_point_include_context_name(
                                state,
                                branch_point_name,
                            );
                        assert!(!state
                            .contexts
                            .contains_key(&include_context_name));

                        state
                            .include_context_cache
                            .insert(key, include_context_name);
                    }
                }

                let mut branches = vec![];

                let num_terminals = terminal_indexes.len();

                // println!("START BRANCH {:?}", branch_point_name);
                let mut is_repetition = true;

                for (i, terminal_index) in
                    terminal_indexes.into_iter().enumerate()
                {
                    let terminal = &lookahead.terminals[terminal_index];

                    if !matches!(
                        terminal.stack.last(),
                        Some(StackEntry {
                            data: StackEntryData::Repetition { .. },
                            ..
                        })
                    ) {
                        is_repetition = false;
                    }

                    /*
                    The last branch of a branch point can't fail the branch;
                    instead failing as illegal.

                    If a branch point starts within another branch point the
                    inner branch point's last branch fails the parent branch
                    point, unless it is on the last branch. Effectively
                    can_fail is always false for only the last branch in a tree
                    of branch points.
                    */
                    let is_last = i != num_terminals - 1;
                    let can_fail = is_last
                        && branch_point.as_ref().is_none_or(|bp| bp.can_fail);
                    let branch_point_name = match &branch_point {
                        Some(branch_point) if !can_fail => branch_point.name,
                        _ => branch_point_name,
                    };

                    // let branch_rule_key = branch_match.local_key(rule_key);
                    let branch_rule_key = terminal.local_key(rule_key);

                    let branch_key = ContextKey {
                        rule_key: branch_rule_key,
                        is_top_level, // TODO: correctness
                        lookahead: Lookahead {
                            terminals: vec![terminal.clone()],
                            end: lookahead::End::Illegal,
                            empty: false,
                        },
                        branch_point: Some(BranchPoint {
                            name: branch_point_name,
                            can_fail,
                        }),
                    };

                    let ctx_name = if let Some(name) =
                        state.context_cache.get(&branch_key)
                    {
                        branches.push(*name);
                        None
                    } else {
                        let name =
                            create_context_name(state, branch_key.clone());
                        branches.push(name);
                        Some(name)
                    };

                    let next_name = if let Some(lookahead) =
                        lookahead::advance_terminal(
                            interpreted,
                            terminal,
                            state.compiler,
                        ) {
                        let next_key = ContextKey {
                            rule_key,
                            is_top_level, // TODO: correctness
                            lookahead,
                            branch_point: branch_key.branch_point,
                        };

                        if let Some(name) = state.context_cache.get(&next_key) {
                            Some(*name)
                        } else {
                            let name =
                                create_context_name(state, next_key.clone());

                            next_contexts.push(next_key);
                            Some(name)
                        }
                    } else {
                        None
                    };

                    if let Some(ctx_name) = ctx_name {
                        let scope = scope_for_match_stack(
                            state,
                            interpreted,
                            None,
                            terminal,
                        );

                        let (exit, pop) = if let Some(name) = next_name {
                            // Using set in branch_point is broken, so we
                            // have to use push.
                            (sublime_syntax::ContextChange::PushOne(name), 1)
                        } else {
                            (sublime_syntax::ContextChange::None, 1)
                        };

                        let terminal_match = gen_terminal(
                            state,
                            interpreted,
                            ctx_name,
                            rule_key,
                            &meta_content_scope,
                            &branch_point,
                            scope,
                            terminal,
                            exit,
                            pop,
                        );

                        let matches = state
                            .compiler
                            .allocator
                            .alloc_slice_clone(&[terminal_match]);

                        state.contexts.insert(
                            ctx_name,
                            sublime_syntax::Context {
                                meta_scope: sublime_syntax::Scope::EMPTY,
                                meta_content_scope:
                                    sublime_syntax::Scope::EMPTY,
                                meta_include_prototype: false,
                                clear_scopes:
                                    sublime_syntax::ScopeClear::Amount(0),
                                matches,
                                comment: None,
                            },
                        );
                    }
                }

                assert!(branches.len() > 1);
                let branches =
                    state.compiler.allocator.alloc_slice_clone(&branches);

                let lookahead_regex =
                    bumpalo::format!(in &state.compiler.allocator,
                        "(?={})", state.compiler.resolve_symbol(regex),
                    )
                    .into_bump_str();

                let comment = bumpalo::format!(in &state.compiler.allocator,
                    "Include context for branch point {}",
                    branch_point_name
                )
                .into_bump_str();

                let pop = if is_repetition { 0 } else { 1 };

                let matches = state.compiler.allocator.alloc_slice_clone(&[
                    sublime_syntax::ContextPattern::Match(
                        sublime_syntax::Match {
                            pattern: sublime_syntax::Pattern(lookahead_regex),
                            scope: sublime_syntax::Scope::EMPTY,
                            captures: &[],
                            change_context:
                                sublime_syntax::ContextChange::Branch(
                                    branch_point_name,
                                    branches,
                                ),
                            pop,
                        },
                    ),
                ]);

                state.contexts.insert(
                    include_context_name,
                    sublime_syntax::Context {
                        meta_scope: sublime_syntax::Scope::EMPTY,
                        meta_content_scope: sublime_syntax::Scope::EMPTY,
                        meta_include_prototype: true,
                        clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                        matches,
                        comment: Some(comment),
                    },
                );

                patterns.push(sublime_syntax::ContextPattern::Include(
                    include_context_name,
                ));
            }
        }

        // Need to add the meta_content_scope to all patterns that pop. This
        // matches expected behaviour in that the rule scope applies to all
        // matches in this context.
        // for p in &mut patterns {
        //     match p {
        //         sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
        //             scope,
        //             change_context: sublime_syntax::ContextChange::Pop(_),
        //             ..
        //         }) => {
        //             scope.scopes = meta_content_scope.scopes.iter().chain(scope.scopes.iter()).cloned().collect::<Vec<_>>();
        //         },
        //         _ => {},
        //     }
        // }

        if let Some(pattern) = gen_end_match(
            state,
            interpreted,
            rule_key,
            is_top_level,
            &branch_point,
            &lookahead,
            capture,
        ) {
            patterns.push(pattern);
        }

        let patterns = state.compiler.allocator.alloc_slice_clone(&patterns);

        let comment = bumpalo::format!(in &state.compiler.allocator,
            "Rule: {}",
            rule_key.with_compiler(state.compiler),
            // context_key.with_compiler(state.compiler)
        )
        .into_bump_str();

        assert!(state.contexts.get(&name).is_none());
        state.contexts.insert(
            name,
            sublime_syntax::Context {
                meta_content_scope,
                meta_scope: sublime_syntax::Scope::EMPTY,
                // meta_scope,
                meta_include_prototype,
                clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                matches: patterns,
                comment: Some(comment),
            },
        );
    }

    if !next_contexts.is_empty() {
        gen_contexts(state, interpreted, next_contexts);
    }
}

fn gen_end_match<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    rule_key: Key,
    is_top_level: bool,
    branch_point: &Option<BranchPoint<'a>>,
    lookahead: &Lookahead<'a>,
    capture: bool,
) -> Option<sublime_syntax::ContextPattern<'a>> {
    match &lookahead.end {
        lookahead::End::Illegal => Some(if lookahead.empty && !capture {
            sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from(r"(?=\S)"),
                scope: sublime_syntax::Scope::EMPTY,
                captures: &[],
                change_context: sublime_syntax::ContextChange::None,
                pop: 1,
            }
        } else if branch_point.is_some()
            && branch_point.as_ref().unwrap().can_fail
        {
            sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from(r"\S"),
                scope: sublime_syntax::Scope::EMPTY,
                captures: &[],
                change_context: sublime_syntax::ContextChange::Fail(
                    branch_point.as_ref().unwrap().name,
                ),
                pop: 0,
            }
        } else {
            sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from(r"\S"),
                scope: parse_scope(
                    &interpreted.metadata,
                    "invalid.illegal",
                    state.compiler,
                ),
                captures: &[],
                change_context: sublime_syntax::ContextChange::None,
                pop: if capture { 0 } else { 1 },
            }
        }),
        lookahead::End::None => None,
        lookahead::End::Push(lookahead) => {
            let push_context_key = ContextKey {
                rule_key,
                is_top_level,
                lookahead: (**lookahead).clone(),
                branch_point: branch_point.clone(),
            };

            let name = if let Some(name) =
                state.context_cache.get(&push_context_key)
            {
                name
            } else {
                let name = create_context_name(state, push_context_key.clone());

                state.context_queue.push(push_context_key);
                name
            };

            Some(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from(r"(?=\S)"),
                scope: sublime_syntax::Scope::EMPTY,
                captures: &[],
                change_context: sublime_syntax::ContextChange::PushOne(name),
                pop: 1,
            })
        }
    }
    .map(sublime_syntax::ContextPattern::Match)
}

fn gen_terminal<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    context_name: &str,
    rule_key: Key,
    meta_content_scope: &sublime_syntax::Scope<'a>,
    branch_point: &Option<BranchPoint>,
    mut scope: sublime_syntax::Scope<'a>,
    terminal: &Terminal<'a>,
    mut exit: sublime_syntax::ContextChange<'a>,
    mut pop_amount: u16,
) -> sublime_syntax::ContextPattern<'a> {
    match &terminal.options.unwrap().embed {
        TerminalEmbed::Embed {
            embed,
            embed_scope,
            escape,
            escape_captures,
        } => {
            let embed_exit =
                sublime_syntax::ContextChange::Embed(sublime_syntax::Embed {
                    embed,
                    embed_scope: *embed_scope,
                    escape: Some(sublime_syntax::Pattern(escape)),
                    escape_captures,
                });

            match &mut exit {
                sublime_syntax::ContextChange::None => {
                    exit = embed_exit;
                }
                sublime_syntax::ContextChange::Set(ref mut contexts)
                | sublime_syntax::ContextChange::Push(ref mut contexts) => {
                    // TODO: This generates duplicate contexts
                    let embed_context = create_uncached_context_name(
                        state,
                        rule_key,
                        branch_point,
                    );

                    let matches =
                        state.compiler.allocator.alloc_slice_clone(&[
                            sublime_syntax::ContextPattern::Match(
                                sublime_syntax::Match {
                                    pattern: sublime_syntax::Pattern::from(""),
                                    scope: sublime_syntax::Scope::EMPTY,
                                    captures: &[],
                                    change_context: embed_exit,
                                    pop: 1,
                                },
                            ),
                        ]);

                    state.contexts.insert(
                        embed_context,
                        sublime_syntax::Context {
                            meta_scope: sublime_syntax::Scope::EMPTY,
                            meta_content_scope: sublime_syntax::Scope::EMPTY,
                            meta_include_prototype: true,
                            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                            matches,
                            comment: None,
                        },
                    );

                    // TODO: Avoid this extra allocation
                    let mut ctx = contexts.to_vec();
                    ctx.push(embed_context);
                    *contexts =
                        state.compiler.allocator.alloc_slice_clone(&ctx);
                }
                _ => panic!(),
            }
        }
        TerminalEmbed::Include { context: path, prototype } => {
            // Generate the prototype context
            let prototype_context = {
                let lookahead = lookahead_rule(state, interpreted, *prototype);

                let prototype_key = ContextKey {
                    rule_key: *prototype,
                    is_top_level: true,
                    lookahead: lookahead.clone(),
                    branch_point: None,
                };

                if let Some(name) = state.context_cache.get(&prototype_key) {
                    name
                } else {
                    let name =
                        create_context_name(state, prototype_key.clone());

                    state.context_queue.push(prototype_key);
                    name
                }
            };

            let include_exit = sublime_syntax::ContextChange::IncludeEmbed(
                sublime_syntax::IncludeEmbed {
                    path,
                    use_push: false,
                    with_prototype: state.compiler.allocator.alloc_slice_clone(
                        &[sublime_syntax::ContextPattern::Include(
                            prototype_context,
                        )],
                    ),
                },
            );

            match exit {
                sublime_syntax::ContextChange::None => {
                    exit = include_exit;
                }
                sublime_syntax::ContextChange::Set(ref mut contexts)
                | sublime_syntax::ContextChange::Push(ref mut contexts) => {
                    // TODO: This generates duplicate contexts
                    let embed_context = create_uncached_context_name(
                        state,
                        rule_key,
                        branch_point,
                    );

                    let matches =
                        state.compiler.allocator.alloc_slice_clone(&[
                            sublime_syntax::ContextPattern::Match(
                                sublime_syntax::Match {
                                    pattern: sublime_syntax::Pattern::from(""),
                                    scope: sublime_syntax::Scope::EMPTY,
                                    captures: &[],
                                    change_context: include_exit,
                                    pop: 0,
                                },
                            ),
                        ]);

                    state.contexts.insert(
                        embed_context,
                        sublime_syntax::Context {
                            meta_scope: sublime_syntax::Scope::EMPTY,
                            meta_content_scope: sublime_syntax::Scope::EMPTY,
                            meta_include_prototype: false,
                            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                            matches,
                            comment: None,
                        },
                    );

                    // TODO: Avoid this extra allocation
                    let mut ctx = contexts.to_vec();
                    ctx.push(embed_context);
                    *contexts =
                        state.compiler.allocator.alloc_slice_clone(&ctx);
                }
                _ => panic!(),
            }
        }
        TerminalEmbed::None => {}
    }

    // Translate Set into Push/Pop if we're setting back to the same context
    if let sublime_syntax::ContextChange::Push(contexts) = &exit {
        if pop_amount > 0 && contexts[0] == context_name {
            if contexts.len() > 1 {
                exit = sublime_syntax::ContextChange::Push(&contexts[1..]);
            } else {
                exit = sublime_syntax::ContextChange::None;
            }
            pop_amount -= 1;
        }
    }

    if let sublime_syntax::ContextChange::None = &exit {
        if pop_amount > 0 {
            scope =
                meta_content_scope.extended(scope, &state.compiler.allocator);
        }
    }

    sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
        pattern: sublime_syntax::Pattern(
            state.compiler.resolve_symbol(terminal.regex),
        ),
        scope,
        captures: terminal.options.unwrap().captures,
        change_context: exit,
        pop: pop_amount,
    })
}

fn gen_simple_match<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    context_name: &str,
    rule_key: Key,
    is_top_level: bool,
    meta_content_scope: &sublime_syntax::Scope<'a>,
    branch_point: &Option<BranchPoint<'a>>,
    lookahead: &Lookahead<'a>,
    scope: sublime_syntax::Scope<'a>,
    terminal: &Terminal<'a>,
) -> sublime_syntax::ContextPattern<'a> {
    let contexts = if let Some(StackEntry {
        data: StackEntryData::Repetition { expression },
        remaining,
    }) = terminal.stack.last()
    {
        let next_lookahead = lookahead::lookahead_concatenation(
            interpreted,
            [expression]
                .iter()
                .cloned()
                .cloned()
                .chain(remaining.iter().cloned()),
            &mut lookahead::LookaheadState::new(state.compiler),
        );

        let is_top_level = false;
        let mut contexts = gen_simple_match_contexts(
            state,
            interpreted,
            rule_key,
            is_top_level,
            &terminal.remaining,
            &terminal.stack[..terminal.stack.len() - 1],
        );

        if next_lookahead == *lookahead {
            // If the remaining of a top-level repetition leads to the same
            // lookahead, then we have a simple repetition. We can just push
            // the child match.
            let exit = if contexts.is_empty() {
                sublime_syntax::ContextChange::None
            } else {
                sublime_syntax::ContextChange::Push(
                    state.compiler.allocator.alloc_slice_clone(&contexts),
                )
            };

            return gen_terminal(
                state,
                interpreted,
                context_name,
                rule_key,
                meta_content_scope,
                branch_point,
                scope,
                terminal,
                exit,
                0,
            );
        } else if branch_point.is_none() {
            // Unclear if correct??
            // Otherwise we have a complex repetition, which behaves the
            // same way as a regular match.
            let repetition_context_key = ContextKey {
                rule_key,
                is_top_level: false,
                lookahead: next_lookahead,
                branch_point: None,
            };

            if let Some(name) = state.context_cache.get(&repetition_context_key)
            {
                contexts.insert(0, name);
            } else {
                let name =
                    create_context_name(state, repetition_context_key.clone());
                state.context_queue.push(repetition_context_key);
                contexts.insert(0, name);
            }
        }

        contexts
    } else {
        gen_simple_match_contexts(
            state,
            interpreted,
            rule_key,
            is_top_level,
            &terminal.remaining,
            &terminal.stack,
        )
    };

    let pop = 1;
    let exit = if contexts.is_empty() {
        sublime_syntax::ContextChange::None
    } else {
        sublime_syntax::ContextChange::Push(
            state.compiler.allocator.alloc_slice_clone(&contexts),
        )
    };

    gen_terminal(
        state,
        interpreted,
        context_name,
        rule_key,
        meta_content_scope,
        branch_point,
        scope,
        terminal,
        exit,
        pop,
    )
}

fn gen_simple_match_contexts<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    mut rule_key: Key,
    mut is_top_level: bool,
    remaining: &[&'a Expression],
    stack: &[StackEntry<'a>],
) -> Vec<&'a str> {
    // Skip stack entries that don't have any remaining expressions. This avoids
    // creating meta-scope contexts when those get immediately popped anyway.
    let offset = if remaining.is_empty() {
        stack.iter().take_while(|e| e.remaining.is_empty()).count()
    } else {
        0
    };

    // Create a context for each item in the match stack that needs it.
    let mut contexts = vec![];

    if let Some(StackEntry { remaining, .. }) = stack[offset..].last() {
        if is_top_level && remaining.is_empty() {
            if let Some(context) =
                gen_meta_content_scope_context(state, interpreted, rule_key)
            {
                contexts.push(context);
            }
        }
    }

    // We need to create entry contexts for remaining terminals that have a meta
    // content scope, otherwise they stack onto earlier terminals.
    // See issue #36
    let mut groups = vec![];

    for (i, entry) in stack[offset..].iter().enumerate().rev() {
        match &entry.data {
            StackEntryData::Variable { .. } => {
                is_top_level = true;
            }
            _ => {
                is_top_level = false;
            }
        }

        if !entry.remaining.is_empty() {
            let lookahead = lookahead::lookahead_concatenation(
                interpreted,
                entry.remaining.iter().cloned(),
                &mut lookahead::LookaheadState::new(state.compiler),
            );

            let (contexts, key) = gen_simple_match_remaining_context(
                state,
                interpreted,
                is_top_level,
                rule_key,
                lookahead,
            );
            groups.push((contexts, key, Some(rule_key)));
        }

        if let StackEntryData::Variable { key } = &entry.data {
            rule_key = *key;

            let rem = if i > 0 { &stack[i - 1].remaining } else { remaining };
            if rem.is_empty() && (i != 0 || !remaining.is_empty()) {
                // If a match has no remaining nodes it can generally be
                // ignored, unless it has a meta scope and there are child
                // matches that were not ignored. In those cases we create
                // a meta scope context.
                if let Some(context) =
                    gen_meta_content_scope_context(state, interpreted, rule_key)
                {
                    groups.push((vec![context], None, None));
                }
            }
        }
    }

    if !remaining.is_empty() {
        let lookahead = lookahead::lookahead_concatenation(
            interpreted,
            remaining.iter().cloned(),
            &mut lookahead::LookaheadState::new(state.compiler),
        );

        let (contexts, key) = gen_simple_match_remaining_context(
            state,
            interpreted,
            is_top_level,
            rule_key,
            lookahead,
        );
        groups.push((contexts, key, None));
    }

    let last = groups.len();
    for (i, (c, key, meta_key)) in groups.into_iter().enumerate() {
        if let Some(key) = key {
            if i != last - 1 {
                contexts.push(gen_entry_context(state, key, c));
                if let Some(meta_key) = meta_key {
                    if let Some(context) = gen_meta_content_scope_context(
                        state,
                        interpreted,
                        meta_key,
                    ) {
                        contexts.push(context);
                    }
                }
            } else {
                contexts.extend(c);
            }
        } else {
            contexts.extend(c);
        }
    }

    contexts
}

fn gen_meta_content_scope_context<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    rule_key: Key,
) -> Option<&'a str> {
    let meta_content_scope = interpreted.rules[&rule_key].options.scope;

    if !meta_content_scope.is_empty() {
        let mut rule_meta_ctx_name = build_rule_key_name(state, rule_key);
        rule_meta_ctx_name.push_str("|meta");

        if let Some((name, _)) =
            state.contexts.get_key_value(rule_meta_ctx_name.as_str())
        {
            Some(name)
        } else {
            let name = state.compiler.allocator.alloc_str(&rule_meta_ctx_name);

            let matches = state.compiler.allocator.alloc_slice_clone(&[
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from(""),
                    scope: sublime_syntax::Scope::EMPTY,
                    captures: &[],
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
            ]);

            let comment = bumpalo::format!(in &state.compiler.allocator,
                "Meta scope context for {}",
                rule_key.with_compiler(state.compiler),
            )
            .into_bump_str();

            state.contexts.insert(
                name,
                sublime_syntax::Context {
                    meta_content_scope,
                    meta_scope: sublime_syntax::Scope::EMPTY,
                    meta_include_prototype: true,
                    clear_scopes: sublime_syntax::ScopeClear::Amount(0),
                    matches,
                    comment: Some(comment),
                },
            );

            Some(name)
        }
    } else {
        None
    }
}

fn gen_entry_context<'a>(
    state: &mut State<'a>,
    rule_key: Key,
    contexts: Vec<&'a str>,
) -> &'a str {
    if let Some(context) = state.entry_contexts.get(&contexts.as_slice()) {
        return context;
    }

    let index = if let Some(rule) = state.rules.get_mut(&rule_key) {
        let i = rule.entry_context_count;
        rule.entry_context_count += 1;
        i
    } else {
        state.rules.insert(
            rule_key,
            Rule {
                context_count: 0,
                branch_point_count: 0,
                entry_context_count: 1,
            },
        );
        0
    };

    let entry_ctx = bumpalo::format!(in &state.compiler.allocator, "{}|entry-{}", contexts.last().unwrap(), index).into_bump_str();

    assert!(!state.contexts.contains_key(entry_ctx));

    let contexts = state.compiler.allocator.alloc_slice_clone(&contexts);

    let matches = state.compiler.allocator.alloc_slice_clone(&[
        sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
            pattern: sublime_syntax::Pattern::from(""),
            scope: sublime_syntax::Scope::EMPTY,
            captures: &[],
            change_context: sublime_syntax::ContextChange::Set(contexts),
            pop: 0,
        }),
    ]);

    state.contexts.insert(
        entry_ctx,
        sublime_syntax::Context {
            meta_content_scope: sublime_syntax::Scope::EMPTY,
            meta_scope: sublime_syntax::Scope::EMPTY,
            meta_include_prototype: true,
            clear_scopes: sublime_syntax::ScopeClear::Amount(0),
            matches,
            comment: None,
        },
    );
    state.entry_contexts.insert(contexts, entry_ctx);
    entry_ctx
}

fn gen_simple_match_remaining_context<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    mut is_top_level: bool,
    mut rule_key: Key,
    mut lookahead: Lookahead<'a>,
) -> (Vec<&'a str>, Option<Key>) {
    // We can end up in situations where we have a context/rule_key that has
    // redundant variables, ie. every match stack has the same variable at the
    // end. Using a similar algorithm to gen_simple_match_contexts we can
    // de-duplicate this, which may also require making meta scope contexts.
    // See issue#18
    let min_matches_count =
        lookahead.terminals.iter().map(|term| term.stack.len()).min().unwrap();

    let mut contexts = vec![];

    for _ in 0..min_matches_count {
        // Take the last item in the match stack for the first lookahead match,
        // then make sure all the others are the same. Only then can we
        // de-duplicate.
        let sample = lookahead.terminals[0].stack.last().unwrap();

        if !sample.remaining.is_empty() {
            break;
        }

        let next_rule_key: Key = match &sample.data {
            StackEntryData::Variable { key } => *key,
            _ => break,
        };

        let all_match = lookahead.terminals[1..].iter().all(|term| {
            let last = term.stack.last().unwrap();

            let key: Key = match &last.data {
                StackEntryData::Variable { key } => *key,
                _ => return false,
            };

            key == next_rule_key && last.remaining.is_empty()
        });

        if !all_match {
            break;
        }

        for terminal in &mut lookahead.terminals {
            terminal.stack.pop();
        }

        if let Some(context_name) =
            gen_meta_content_scope_context(state, interpreted, rule_key)
        {
            contexts.push(context_name);
        }

        rule_key = next_rule_key;
        is_top_level = true;
    }

    let context_key =
        ContextKey { rule_key, is_top_level, lookahead, branch_point: None };

    if let Some(name) = state.context_cache.get(&context_key) {
        contexts.push(name);
    } else {
        let name = create_context_name(state, context_key.clone());
        state.context_queue.push(context_key);

        contexts.push(name);
    }

    let key = if contexts.len() > 1
        || !interpreted.rules[&rule_key].options.scope.is_empty()
    {
        Some(rule_key)
    } else {
        None
    };

    (contexts, key)
}

fn build_rule_key_name(state: &State, rule_key: Key) -> String {
    let mut result = rule_key.get_name(state.compiler).to_string();

    // Encode arguments
    if let Some(arguments) = rule_key.get_arguments(state.compiler) {
        result.push('@');

        // Arguments can be in any format, so convert them to a string
        // representation first and then base-64 encode them to make them safe
        // to use in a context name.
        use base64::Engine;
        base64::engine::general_purpose::URL_SAFE_NO_PAD
            .encode_string(arguments.as_bytes(), &mut result);
    }

    result
}

// Generate an uncached unique name for a context key
fn create_uncached_context_name<'a>(
    state: &mut State<'a>,
    rule_key: Key,
    branch_point: &Option<BranchPoint>,
) -> &'a str {
    let mut result = build_rule_key_name(state, rule_key);

    // Add inner context count to prevent context name collisions in inner contexts
    let index = if let Some(rule) = state.rules.get_mut(&rule_key) {
        let i = rule.context_count;
        rule.context_count += 1;
        i
    } else {
        state.rules.insert(
            rule_key,
            Rule {
                context_count: 1,
                branch_point_count: 0,
                entry_context_count: 0,
            },
        );
        0
    };
    write!(result, "|{}", index).unwrap();

    // Add optional branch point
    if let Some(branch_point) = &branch_point {
        result.push('|');
        result.push_str(branch_point.name);
    }

    state.compiler.allocator.alloc_str(&result)
}

// Generate a unique name for a context key
fn create_context_name<'a>(
    state: &mut State<'a>,
    key: ContextKey<'a>,
) -> &'a str {
    let name =
        create_uncached_context_name(state, key.rule_key, &key.branch_point);

    let old_entry = state.context_cache.insert(key, name);
    assert!(old_entry.is_none());

    name
}

// Generate a new branch point for a rule
fn create_branch_point_name<'a>(state: &mut State<'a>, key: Key) -> &'a str {
    let index = if let Some(rule) = state.rules.get_mut(&key) {
        rule.branch_point_count += 1;
        rule.branch_point_count
    } else {
        state.rules.insert(
            key,
            Rule {
                context_count: 0,
                branch_point_count: 1,
                entry_context_count: 0,
            },
        );
        1
    };

    bumpalo::format!(
        in &state.compiler.allocator,
        "{}@{}",
        key.get_name(state.compiler),
        index
    )
    .into_bump_str()
}

fn create_branch_point_include_context_name<'a>(
    state: &mut State<'a>,
    branch_point: &str,
) -> &'a str {
    bumpalo::format!(
        in &state.compiler.allocator,
        "include!{}", branch_point)
    .into_bump_str()
}

fn scope_for_match_stack<'a>(
    state: &mut State<'a>,
    interpreted: &Interpreted<'a>,
    rule_key: Option<Key>,
    terminal: &Terminal<'a>,
) -> sublime_syntax::Scope<'a> {
    let mut scope = sublime_syntax::Scope::EMPTY;

    if let Some(rule_key) = rule_key {
        scope = interpreted.rules[&rule_key].options.scope;
    }

    for entry in terminal.stack.iter().rev() {
        if let StackEntryData::Variable { key } = &entry.data {
            let rule_options = &interpreted.rules[key].options;

            scope =
                scope.extended(rule_options.scope, &state.compiler.allocator);
        }
    }

    scope.extended(terminal.options.unwrap().scope, &state.compiler.allocator)
}
