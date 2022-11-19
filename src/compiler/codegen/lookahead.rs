/*
Given an SBNF expression we build a list of all valid terminals that a parser
needs to check against, effectively the lookahead of LL1 parsers. We also track
the stack of non-terminals used to reach each terminal, as well as the
expressions that may follow each terminal.

Say we have the expression `b` for the following grammar:
a = ('1' | '2') 'c' ;
b = a* ';'? ;

This produces the following stacks:
'1' remaining: 'c'
| loop 'a'
| b remaining: ';'?

'2' remaining: 'c'
| loop 'a'
| b remaining: ';'?

';'

The expression may also be empty.
*/

use super::super::common::{Compiler, Symbol};
use super::super::interpreter::{
    Expression, Interpreted, Key, TerminalOptions,
};
use crate::sbnf::TextLocation;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StackEntryData<'a> {
    Variable { key: &'a Key },
    Repetition { expression: &'a Expression<'a> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StackEntry<'a> {
    pub data: StackEntryData<'a>,
    pub remaining: Vec<&'a Expression<'a>>,
}

impl<'a> StackEntry<'a> {
    pub fn with_compiler(
        &'a self,
        compiler: &'a Compiler,
    ) -> StackEntryWithCompiler {
        StackEntryWithCompiler { entry: self, compiler }
    }
}

pub struct StackEntryWithCompiler<'a> {
    entry: &'a StackEntry<'a>,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Debug for StackEntryWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.entry.data {
            StackEntryData::Variable { key } => {
                write!(f, "var {}", key.with_compiler(self.compiler))?;
            }
            StackEntryData::Repetition { expression } => {
                write!(
                    f,
                    "repetition {:?}",
                    expression.with_compiler(self.compiler)
                )?;
            }
        }
        if !self.entry.remaining.is_empty() {
            write!(f, " [")?;
            for expr in &self.entry.remaining {
                write!(f, "{:?}, ", expr.with_compiler(self.compiler))?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Terminal<'a> {
    pub regex: Symbol,
    // Only None for sentinel terminals used to track left recursion
    pub options: Option<&'a TerminalOptions>,
    pub remaining: Vec<&'a Expression<'a>>,
    pub stack: Vec<StackEntry<'a>>,
}

impl<'a> Terminal<'a> {
    fn new(regex: Symbol, options: Option<&'a TerminalOptions>) -> Terminal {
        Terminal { regex, options, remaining: vec![], stack: vec![] }
    }

    fn get_last_remaining(&mut self) -> &mut Vec<&'a Expression<'a>> {
        self.get_mut_remaining(self.stack.len())
    }

    fn get_mut_remaining(
        &mut self,
        index: usize,
    ) -> &mut Vec<&'a Expression<'a>> {
        if index == 0 {
            &mut self.remaining
        } else {
            &mut self.stack[index - 1].remaining
        }
    }

    pub fn get_remaining(&self, index: usize) -> &Vec<&'a Expression<'a>> {
        if index == 0 {
            &self.remaining
        } else {
            &self.stack[index - 1].remaining
        }
    }

    pub fn local_key(&self, topmost: &'a Key) -> &'a Key {
        for entry in &self.stack {
            match &entry.data {
                StackEntryData::Variable { key } => {
                    return key;
                }
                _ => {}
            }
        }

        return topmost;
    }

    pub fn iter<'b>(&'b self) -> TerminalStackIterator<'a, 'b> {
        TerminalStackIterator {
            terminal: self,
            index: 0,
            size: self.stack.len() + 1,
        }
    }

    pub fn with_compiler(
        &'a self,
        compiler: &'a Compiler,
    ) -> TerminalWithCompiler<'a> {
        TerminalWithCompiler { terminal: self, compiler }
    }
}

impl std::hash::Hash for Terminal<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.regex.hash(state);
        self.remaining.hash(state);
        self.stack.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct TerminalStackIterator<'a, 'b> {
    terminal: &'b Terminal<'a>,
    index: usize,
    size: usize,
}

#[derive(Debug, Clone)]
pub struct TerminalStackIteratorItem<'a, 'b> {
    pub data: Option<&'b StackEntryData<'a>>,
    pub remaining: &'b Vec<&'a Expression<'a>>,
}

impl<'a, 'b> TerminalStackIteratorItem<'a, 'b> {
    fn clone_stack_entry(&self) -> StackEntry<'a> {
        StackEntry {
            data: self.data.unwrap().clone(),
            remaining: self.remaining.clone(),
        }
    }
}

impl<'a, 'b> PartialEq<StackEntry<'a>> for TerminalStackIteratorItem<'a, 'b> {
    fn eq(&self, other: &StackEntry<'a>) -> bool {
        self.data.map_or(false, |d| *d == other.data)
            && *self.remaining == other.remaining
    }
}

impl<'a, 'b> TerminalStackIterator<'a, 'b> {
    fn get_item(&self, index: usize) -> TerminalStackIteratorItem<'a, 'b> {
        if index == 0 {
            TerminalStackIteratorItem {
                data: None,
                remaining: &self.terminal.remaining,
            }
        } else {
            let stack_entry = &self.terminal.stack[index - 1];

            TerminalStackIteratorItem {
                data: Some(&stack_entry.data),
                remaining: &stack_entry.remaining,
            }
        }
    }

    fn _is_empty(&self) -> bool {
        self.index == self.size
    }
}

impl<'a, 'b> Iterator for TerminalStackIterator<'a, 'b> {
    type Item = TerminalStackIteratorItem<'a, 'b>;

    fn next(&mut self) -> Option<Self::Item> {
        if self._is_empty() {
            return None;
        }

        let result = self.get_item(self.index);
        self.index += 1;
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let l = self.len();
        (l, Some(l))
    }
}

impl<'a, 'b> DoubleEndedIterator for TerminalStackIterator<'a, 'b> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self._is_empty() {
            return None;
        }

        self.size -= 1;
        Some(self.get_item(self.size))
    }
}

impl<'a, 'b> ExactSizeIterator for TerminalStackIterator<'a, 'b> {
    fn len(&self) -> usize {
        self.size - self.index
    }
}

pub struct TerminalWithCompiler<'a> {
    terminal: &'a Terminal<'a>,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Debug for TerminalWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "match {} {:?}\n",
            self.compiler.resolve_symbol(self.terminal.regex),
            self.terminal.options
        )?;
        if !self.terminal.remaining.is_empty() {
            write!(f, "remaining [")?;
            for expr in &self.terminal.remaining {
                write!(f, "{:?}, ", expr.with_compiler(self.compiler))?;
            }
            write!(f, "]\n")?;
        }
        if !self.terminal.stack.is_empty() {
            write!(f, "stack\n")?;
            for entry in &self.terminal.stack {
                write!(f, "* {:?}\n", entry.with_compiler(self.compiler))?;
            }
        }
        Ok(())
    }
}

// Determines how a context should end, ie. what the last match in a context
// should be.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum End<'a> {
    // Reaching the end of the context is illegal
    Illegal,
    // Ignore things that don't match, ie. don't do anything at the end of the context
    None,
    // If the end of the context is reached, push another context
    Push(Box<Lookahead<'a>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lookahead<'a> {
    pub terminals: Vec<Terminal<'a>>,
    pub end: End<'a>,
    pub empty: bool,
}

impl<'a> Lookahead<'a> {
    fn append(&mut self, mut other: Self) {
        self.terminals.append(&mut other.terminals);

        self.empty = self.empty || other.empty;

        match self.end {
            End::Illegal => {
                self.end = other.end;
            }
            End::None => {
                self.end = match other.end {
                    End::Illegal | End::None => End::None,
                    End::Push(ref mut _push) => {
                        // push.append_update_end(End::None);
                        other.end
                    }
                };
            }
            End::Push(ref mut self_push) => {
                match other.end {
                    End::Illegal | End::None => {
                        // self_push.append_update_end(End::None);
                    }
                    End::Push(other_push) => {
                        self_push.append(*other_push);
                    }
                }
            }
        }
    }

    fn concatenate(&mut self, mut next: Self) {
        // Should only be concatenating to empty lookaheads
        assert!(self.empty);

        self.end = match self.end {
            End::Illegal => match next.end {
                End::Illegal => End::Illegal,
                End::None => End::Push(Box::new(Lookahead {
                    terminals: next.terminals.clone(),
                    end: End::None,
                    empty: next.empty,
                })),
                _ => todo!(),
            },
            End::None => match next.end {
                End::Illegal => End::Push(Box::new(Lookahead {
                    terminals: self.terminals.clone(),
                    end: End::None,
                    empty: next.empty,
                })),
                End::None => End::None,
                _ => todo!(),
            },
            _ => todo!(),
        };

        self.terminals.append(&mut next.terminals);
        self.empty = next.empty;
    }

    pub fn with_compiler(
        &'a self,
        compiler: &'a Compiler,
    ) -> LookaheadWithCompiler<'a> {
        LookaheadWithCompiler { lookahead: self, compiler }
    }
}

pub struct LookaheadWithCompiler<'a> {
    lookahead: &'a Lookahead<'a>,
    compiler: &'a Compiler,
}

impl<'a> std::fmt::Debug for LookaheadWithCompiler<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LOOKAHEAD empty:{:?} end:{:?}\n",
            self.lookahead.empty, self.lookahead.end
        )?;
        for terminal in &self.lookahead.terminals {
            write!(f, "{:?}\n", terminal.with_compiler(self.compiler))?;
        }
        write!(f, "LOOKAHEAD END")
    }
}

pub struct LookaheadState<'a> {
    visited_variables: HashMap<&'a Key, bool>,
    pub compiler: &'a Compiler,
}

impl<'a> LookaheadState<'a> {
    pub fn new(compiler: &'a Compiler) -> LookaheadState<'a> {
        LookaheadState { visited_variables: HashMap::new(), compiler }
    }

    pub fn push_variable(&mut self, key: &'a Key) -> Option<Lookahead<'a>> {
        // If we're already in the stack of variables then we've got left recursion
        if let Some(left_recursion) = self.visited_variables.get_mut(key) {
            *left_recursion = true;

            // Create a sentinel terminal with no terminal options
            Some(Lookahead {
                terminals: vec![Terminal::new(key.name, None)],
                end: End::Illegal,
                empty: false,
            })
        } else {
            self.visited_variables.insert(key, false);
            None
        }
    }

    pub fn pop_variable(&mut self, key: &Key, lookahead: &mut Lookahead<'a>) {
        // Check if we have left recursion
        if self.visited_variables.remove(key).unwrap() {
            // Extract terminals that follow a left recursion
            let left_recursion_terminals = {
                let mut result = vec![];
                let mut i = 0;
                while i < lookahead.terminals.len() {
                    if lookahead.terminals[i].options.is_none() {
                        result.push(lookahead.terminals.remove(i));
                    } else {
                        i += 1;
                    }
                }
                result
            };

            // Build expressions for the terminals following a left recursion as
            // a repetition of an alternation of concatenations.
            // TODO: This doesn't seem right, but waiting on examples to work on
            // this further.
            let expressions = left_recursion_terminals
                .into_iter()
                .filter_map(|rt| {
                    if rt.remaining.len() > 1 {
                        Some(Expression::Concatenation {
                            expressions: self
                                .compiler
                                .allocator
                                .alloc_slice_fill_iter(
                                    rt.remaining.iter().map(|e| (*e).clone()),
                                ),
                            location: TextLocation::invalid(),
                        })
                    } else if rt.remaining.len() == 1 {
                        Some((*rt.remaining[0]).clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            let expressions = self
                .compiler
                .allocator
                .alloc_slice_fill_iter(expressions.into_iter());

            if !expressions.is_empty() {
                let expression = if expressions.len() > 1 {
                    self.compiler.allocator.alloc(Expression::Alternation {
                        expressions,
                        location: TextLocation::invalid(),
                    })
                } else {
                    expressions.into_iter().next().unwrap()
                };
                let rep =
                    self.compiler.allocator.alloc(Expression::Repetition {
                        expression,
                        location: TextLocation::invalid(),
                    });

                for term in &mut lookahead.terminals {
                    term.remaining.insert(0, rep);
                }
            } else {
                lookahead.empty = true;
            }
        }
    }
}

// Transform and collect matches that the context for the expression needs to match
pub fn lookahead<'a>(
    interpreted: &'a Interpreted,
    expression: &'a Expression,
    state: &mut LookaheadState<'a>,
) -> Lookahead<'a> {
    match expression {
        Expression::Variable { key, .. } => {
            if let Some(lookahead) = state.push_variable(key) {
                return lookahead;
            }

            let rule = interpreted.rules.get(key).unwrap();

            let mut la = lookahead(interpreted, &rule.expression, state);

            state.pop_variable(key, &mut la);

            // Add the variable to the stacks
            for term in &mut la.terminals {
                term.stack.push(StackEntry {
                    data: StackEntryData::Variable { key },
                    remaining: vec![],
                });
            }

            la
        }
        Expression::Terminal { regex, options, .. } => Lookahead {
            terminals: vec![Terminal::new(*regex, Some(options))],
            end: End::Illegal,
            empty: false,
        },
        Expression::Passive { expression, .. } => {
            let mut la = lookahead(interpreted, &expression, state);
            la.end = End::None;
            la
        }
        Expression::Repetition { expression: child, .. } => {
            let mut la = lookahead(interpreted, &child, state);

            // Add the repetition to the front of each match stack
            for term in &mut la.terminals {
                term.stack.push(StackEntry {
                    data: StackEntryData::Repetition { expression },
                    remaining: vec![],
                });
            }

            // Treat repetitions optional
            la.empty = true;

            la
        }
        Expression::Optional { expression: child, .. } => {
            let la = lookahead(interpreted, &child, state);

            match la.end {
                End::Illegal => Lookahead {
                    terminals: la.terminals,
                    end: la.end,
                    empty: true,
                },
                End::None | End::Push(_) => Lookahead {
                    terminals: vec![],
                    end: End::Push(Box::new(la)),
                    empty: true,
                },
            }
        }
        Expression::Alternation { expressions, .. } => {
            let mut la = Lookahead {
                terminals: vec![],
                end: End::Illegal,
                empty: false,
            };

            for expression in *expressions {
                la.append(lookahead(interpreted, &expression, state));
            }

            la
        }
        Expression::Concatenation { expressions, .. } => {
            lookahead_concatenation(interpreted, expressions.iter(), state)
        }
    }
}

// A concatenation of contexts is the first context that can't be empty, with
// those before being alternations
pub fn lookahead_concatenation<'a, I>(
    interpreted: &'a Interpreted,
    mut expressions: I,
    state: &mut LookaheadState<'a>,
) -> Lookahead<'a>
where
    I: std::iter::Iterator<Item = &'a Expression<'a>>,
    I: std::clone::Clone,
{
    let mut f = |expr, remaining: I| {
        let mut l: Lookahead<'a> = lookahead(interpreted, expr, state);

        for terminal in &mut l.terminals {
            terminal.get_last_remaining().extend(remaining.clone());
        }
        l
    };

    let mut la = f(expressions.next().unwrap(), expressions.clone());

    if !la.empty {
        return la;
    }

    while let Some(expression) = expressions.next() {
        let l = f(expression, expressions.clone());

        la.concatenate(l);

        if !la.empty {
            break;
        }
    }

    la
}

// Collect the next context following the context stack
pub fn advance_terminal<'a>(
    interpreted: &'a Interpreted,
    terminal: &Terminal<'a>,
    compiler: &'a Compiler,
) -> Option<Lookahead<'a>> {
    let mut iter = terminal.iter();

    let mut lookahead = None;

    while let Some(entry) = iter.next() {
        if entry.remaining.is_empty() {
            continue;
        }

        let mut las = LookaheadState::new(compiler);

        let mut la = match &entry.data {
            Some(StackEntryData::Repetition { expression }) => {
                lookahead_concatenation(
                    interpreted,
                    [expression]
                        .iter()
                        .cloned()
                        .chain(entry.remaining.iter())
                        .cloned(),
                    &mut las,
                )
            }
            _ => lookahead_concatenation(
                interpreted,
                entry.remaining.iter().cloned(),
                &mut las,
            ),
        };

        for t in &mut la.terminals {
            // Check for recursion and convert it to a repetition
            // TODO: The cloned here seems unnecessary
            if !t.stack.is_empty()
                && iter.clone().take(t.stack.len()).eq(t.stack.iter().cloned())
            {
                t.stack.extend(
                    iter.clone()
                        .take(t.stack.len())
                        .map(|e| e.clone_stack_entry()),
                );
                continue;
            }

            t.stack.extend(iter.clone().map(|e| e.clone_stack_entry()));
        }

        match &mut lookahead {
            None => {
                lookahead = Some(la);
            }
            Some(lookahead) => {
                lookahead.concatenate(la);
            }
        }

        if !lookahead.as_ref().unwrap().empty {
            break;
        }
    }

    lookahead
}

#[cfg(test)]
mod tests {
    extern crate matches;
    use matches::assert_matches;

    use super::*;
    use crate::compiler::interpreter::tests::*;
    use crate::compiler::{collector, interpreter, CompileOptions, Compiler};
    use crate::sbnf;

    struct Harness {
        compiler: Compiler,
    }

    impl Harness {
        fn new() -> Harness {
            Harness { compiler: Compiler::new() }
        }

        fn symbol(&self, name: &str) -> Symbol {
            self.compiler.get_symbol(name)
        }

        fn lookahead<F>(&mut self, source: &str, rule_name: &str, fun: F)
        where
            F: Fn(Lookahead, &Compiler) -> (),
        {
            let grammar = sbnf::parse(source).unwrap();

            let options = CompileOptions {
                name_hint: Some("test"),
                arguments: vec![],
                debug_contexts: false,
                entry_points: vec!["m"],
            };

            let collection =
                collector::collect(&mut self.compiler, &options, &grammar);
            assert!(collection.warnings.is_empty());

            let collection = collection.result.unwrap();

            let interpreter_result =
                interpreter::interpret(&self.compiler, &options, collection);
            assert!(interpreter_result.warnings.is_empty());

            let interpreted = interpreter_result.result.as_ref().unwrap();

            let key = interpreter::Key {
                name: self.symbol(rule_name),
                arguments: vec![],
            };
            let rule = &interpreted.rules[&key];

            let mut lookahead_state = LookaheadState::new(&self.compiler);
            assert!(lookahead_state.push_variable(&key).is_none());

            let mut la =
                lookahead(interpreted, &rule.expression, &mut lookahead_state);

            lookahead_state.pop_variable(&key, &mut la);

            println!("{:?}", la.with_compiler(&self.compiler));
            fun(la, &self.compiler);
        }
    }

    fn sed_rep<'a>(expression: &'a Expression) -> StackEntryData<'a> {
        StackEntryData::Repetition { expression }
    }

    fn sed_var<'a>(key: &'a Key) -> StackEntryData<'a> {
        StackEntryData::Variable { key }
    }

    #[test]
    fn collect_passive() {
        let mut harness = Harness::new();
        let sym_a = harness.symbol("a");
        let sym_b = harness.symbol("b");
        let sym_c = harness.symbol("c");

        harness.lookahead("m : ~'a';", "m", |lookahead, _c| {
            assert_eq!(lookahead.end, End::None);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            assert_eq!(lookahead.terminals[0].regex, sym_a);
            assert_eq!(lookahead.terminals[0].remaining.len(), 0);
            assert_eq!(lookahead.terminals[0].stack.len(), 0);
        });

        harness.lookahead("m : ~'a' 'b';", "m", |lookahead, _c| {
            assert_eq!(lookahead.end, End::None);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_trm_noopt(sym_b));
            assert_eq!(term0.stack.len(), 0);
        });

        harness.lookahead("m : ~'a'* ~'b';", "m", |lookahead, c| {
            assert_eq!(lookahead.end, End::None);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(
                term0.stack[0].data,
                sed_rep(&expr_rep(c, expr_trm_noopt(sym_a)))
            );
            assert_eq!(term0.stack[0].remaining.len(), 1);
            assert_eq!(
                term0.stack[0].remaining[0],
                &expr_pas(c, expr_trm_noopt(sym_b))
            );
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : (~'a')* ~'b';", "m", |lookahead, c| {
            assert_eq!(lookahead.end, End::None);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(
                term0.stack[0].data,
                sed_rep(&expr_rep(c, expr_pas(c, expr_trm_noopt(sym_a))))
            );
            assert_eq!(term0.stack[0].remaining.len(), 1);
            assert_eq!(
                term0.stack[0].remaining[0],
                &expr_pas(c, expr_trm_noopt(sym_b))
            );
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : ~('a' | 'b') 'c';", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::None);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_trm_noopt(sym_c));
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 1);
            assert_eq!(term1.remaining[0], &expr_trm_noopt(sym_c));
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : ~'a'?;", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::None);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 0);
        });

        harness.lookahead("m : (~'a')?;", "m", |lookahead, _c| {
            match lookahead.end {
                End::Push(lookahead) => {
                    assert_matches!(lookahead.end, End::None);
                    assert!(!lookahead.empty);
                    assert_eq!(lookahead.terminals.len(), 1);
                    let term0 = &lookahead.terminals[0];
                    assert_eq!(term0.regex, sym_a);
                    assert_eq!(term0.remaining.len(), 0);
                    assert_eq!(term0.stack.len(), 0);
                }
                _ => panic!(),
            }
            assert!(lookahead.empty);
            // TODO: This is wrong!
            assert!(lookahead.terminals.is_empty());
        });

        harness.lookahead("m : (~'a')* 'b';", "m", |lookahead, c| {
            match lookahead.end {
                End::Push(lookahead) => {
                    assert_matches!(lookahead.end, End::None);
                    assert!(!lookahead.empty);
                    assert_eq!(lookahead.terminals.len(), 1);
                    let term0 = &lookahead.terminals[0];
                    assert_eq!(term0.regex, sym_a);
                    assert_eq!(term0.remaining.len(), 0);
                    assert_eq!(term0.stack.len(), 1);
                    assert_eq!(
                        term0.stack[0].data,
                        sed_rep(&expr_rep(
                            c,
                            expr_pas(c, expr_trm_noopt(sym_a))
                        ))
                    );
                    assert_eq!(term0.stack[0].remaining.len(), 1);
                    assert_eq!(
                        term0.stack[0].remaining[0],
                        &expr_trm_noopt(sym_b)
                    );
                }
                _ => panic!(),
            }
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(
                term0.stack[0].data,
                sed_rep(&expr_rep(c, expr_pas(c, expr_trm_noopt(sym_a))))
            );
            assert_eq!(term0.stack[0].remaining.len(), 1);
            assert_eq!(term0.stack[0].remaining[0], &expr_trm_noopt(sym_b));
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : 'a'? ~'b';", "m", |lookahead, c| {
            match lookahead.end {
                End::Push(lookahead) => {
                    assert_matches!(lookahead.end, End::None);
                    assert!(!lookahead.empty);
                    assert_eq!(lookahead.terminals.len(), 1);
                    let term0 = &lookahead.terminals[0];
                    assert_eq!(term0.regex, sym_b);
                    assert_eq!(term0.remaining.len(), 0);
                    assert_eq!(term0.stack.len(), 0);
                }
                _ => panic!(),
            }
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_pas(c, expr_trm_noopt(sym_b)));
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
        });
    }

    #[test]
    fn collect_repetition() {
        let mut harness = Harness::new();
        let sym_a = harness.symbol("a");
        let sym_b = harness.symbol("b");
        let sym_c = harness.symbol("c");

        harness.lookahead("m : 'a'*;", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(
                term0.stack[0].data,
                sed_rep(&expr_rep(c, expr_trm_noopt(sym_a)))
            );
            assert_eq!(term0.stack[0].remaining.len(), 0);
        });

        harness.lookahead("m : ('a'? 'b' | 'c')*;", "m", |lookahead, c| {
            let rep = expr_rep(
                c,
                expr_alt(
                    c,
                    &[
                        expr_cat(
                            c,
                            &[
                                expr_opt(c, expr_trm_noopt(sym_a)),
                                expr_trm_noopt(sym_b),
                            ],
                        ),
                        expr_trm_noopt(sym_c),
                    ],
                ),
            );

            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 3);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_trm_noopt(sym_b));
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(term0.stack[0].data, sed_rep(&rep));
            assert_eq!(term0.stack[0].remaining.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 1);
            assert_eq!(term1.stack[0].data, sed_rep(&rep));
            assert_eq!(term1.stack[0].remaining.len(), 0);
            let term2 = &lookahead.terminals[2];
            assert_eq!(term2.regex, sym_c);
            assert_eq!(term2.remaining.len(), 0);
            assert_eq!(term2.stack.len(), 1);
            assert_eq!(term2.stack[0].data, sed_rep(&rep));
            assert_eq!(term2.stack[0].remaining.len(), 0);
        });
    }

    #[test]
    fn collect_optional() {
        let mut harness = Harness::new();
        let sym_a = harness.symbol("a");
        let sym_b = harness.symbol("b");
        let sym_c = harness.symbol("c");

        harness.lookahead("m : 'a'?;", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 0);
        });

        harness.lookahead("m : ('a' | 'b'* 'c')?;", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 3);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 1);
            assert_eq!(
                term1.stack[0].data,
                sed_rep(&expr_rep(c, expr_trm_noopt(sym_b)))
            );
            assert_eq!(term1.stack[0].remaining.len(), 1);
            assert_eq!(term1.stack[0].remaining[0], &expr_trm_noopt(sym_c));
            let term2 = &lookahead.terminals[2];
            assert_eq!(term2.regex, sym_c);
            assert_eq!(term2.remaining.len(), 0);
            assert_eq!(term2.stack.len(), 0);
        });
    }

    #[test]
    fn collect_alternation() {
        let mut harness = Harness::new();
        let sym_a = harness.symbol("a");
        let sym_b = harness.symbol("b");
        let sym_c = harness.symbol("c");

        harness.lookahead("m : 'a' | 'b';", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : 'a' | 'b' 'c';", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 1);
            assert_eq!(term1.remaining[0], &expr_trm_noopt(sym_c));
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : 'a'? | 'b' | 'c'*;", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 3);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
            let term2 = &lookahead.terminals[2];
            assert_eq!(term2.regex, sym_c);
            assert_eq!(term2.remaining.len(), 0);
            assert_eq!(term2.stack.len(), 1);
            assert_eq!(
                term2.stack[0].data,
                sed_rep(&expr_rep(c, expr_trm_noopt(sym_c)))
            );
            assert_eq!(term2.stack[0].remaining.len(), 0);
        });
    }

    #[test]
    fn collect_concat() {
        let mut harness = Harness::new();
        let sym_a = harness.symbol("a");
        let sym_b = harness.symbol("b");
        let sym_c = harness.symbol("c");
        let r_key = Key { name: harness.symbol("r"), arguments: vec![] };

        harness.lookahead("m : 'a' 'b';", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_trm_noopt(sym_b));
            assert_eq!(term0.stack.len(), 0);
        });

        harness.lookahead("m : ('a' | 'b') 'c';", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_trm_noopt(sym_c));
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 1);
            assert_eq!(term1.remaining[0], &expr_trm_noopt(sym_c));
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead("m : 'a'? 'b'* 'c';", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 3);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 2);
            assert_eq!(term0.remaining[0], &expr_rep(c, expr_trm_noopt(sym_b)));
            assert_eq!(term0.remaining[1], &expr_trm_noopt(sym_c));
            assert_eq!(term0.stack.len(), 0);
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 1);
            assert_eq!(
                term1.stack[0].data,
                sed_rep(&expr_rep(c, expr_trm_noopt(sym_b)))
            );
            assert_eq!(term1.stack[0].remaining.len(), 1);
            assert_eq!(term1.stack[0].remaining[0], &expr_trm_noopt(sym_c));
            let term2 = &lookahead.terminals[2];
            assert_eq!(term2.regex, sym_c);
            assert_eq!(term2.remaining.len(), 0);
            assert_eq!(term2.stack.len(), 0);
        });

        harness.lookahead("m : 'a'* 'b'?;", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 2);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(
                term0.stack[0].data,
                sed_rep(&expr_rep(c, expr_trm_noopt(sym_a)))
            );
            assert_eq!(term0.stack[0].remaining.len(), 1);
            assert_eq!(
                term0.stack[0].remaining[0],
                &expr_opt(c, expr_trm_noopt(sym_b))
            );
            let term1 = &lookahead.terminals[1];
            assert_eq!(term1.regex, sym_b);
            assert_eq!(term1.remaining.len(), 0);
            assert_eq!(term1.stack.len(), 0);
        });

        harness.lookahead(
            "m : r? 'b' ; r : 'a' r? 'b' ;",
            "m",
            |lookahead, _c| {
                assert_matches!(lookahead.end, End::Illegal);
                assert!(!lookahead.empty);
                assert_eq!(lookahead.terminals.len(), 2);
                let term0 = &lookahead.terminals[0];
                assert_eq!(term0.regex, sym_a);
                assert_eq!(term0.remaining.len(), 2);
                assert_eq!(term0.stack.len(), 1);
                assert_eq!(term0.stack[0].data, sed_var(&r_key));
                assert_eq!(term0.stack[0].remaining.len(), 1);
                assert_eq!(term0.stack[0].remaining[0], &expr_trm_noopt(sym_b));
                let term1 = &lookahead.terminals[1];
                assert_eq!(term1.regex, sym_b);
                assert_eq!(term1.remaining.len(), 0);
                assert_eq!(term1.stack.len(), 0);
            },
        );
    }

    #[test]
    fn collect_left_recursion() {
        let mut harness = Harness::new();
        let sym_a = harness.symbol("a");
        let sym_b = harness.symbol("b");
        let sym_c = harness.symbol("c");
        let m_key = Key { name: harness.symbol("m"), arguments: vec![] };
        let r_key = Key { name: harness.symbol("r"), arguments: vec![] };

        harness.lookahead("m : m ;", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 0);
        });

        harness.lookahead("m : m 'a' | 'b';", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_b);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(term0.remaining[0], &expr_rep(c, expr_trm_noopt(sym_a)));
            assert_eq!(term0.stack.len(), 0);
        });

        harness.lookahead("m : m 'a' | m 'b' | 'c';", "m", |lookahead, c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(!lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_c);
            assert_eq!(term0.remaining.len(), 1);
            assert_eq!(
                term0.remaining[0],
                &expr_rep(
                    c,
                    expr_alt(
                        c,
                        &[expr_trm_noopt(sym_a), expr_trm_noopt(sym_b)]
                    )
                )
            );
            assert_eq!(term0.stack.len(), 0);
        });

        harness.lookahead("m : r? m ; r : 'a' ;", "m", |lookahead, _c| {
            assert_matches!(lookahead.end, End::Illegal);
            assert!(lookahead.empty);
            assert_eq!(lookahead.terminals.len(), 1);
            let term0 = &lookahead.terminals[0];
            assert_eq!(term0.regex, sym_a);
            assert_eq!(term0.remaining.len(), 0);
            assert_eq!(term0.stack.len(), 1);
            assert_eq!(term0.stack[0].data, sed_var(&r_key));
            assert_eq!(term0.stack[0].remaining.len(), 1);
            assert_eq!(term0.stack[0].remaining[0], &expr_var(m_key.clone()));
        });
    }
}
