#![allow(unused_assignments)]

/// This file describes the structure of sublime-syntax files and defines a
/// serializer for it.
use bumpalo::Bump;
use std::fmt::{Error, Write};

struct SerializeState<'a> {
    indent: u16,
    output: &'a mut dyn Write,
}

impl SerializeState<'_> {
    fn write_indentation(&mut self) -> Result<(), Error> {
        for _ in 0..self.indent {
            write!(&mut self.output, "  ")?;
        }

        Ok(())
    }
}

macro_rules! serializeln {
    ($state:expr, $($arg:tt)*) => ({
        $state.write_indentation()?;
        writeln!(&mut $state.output, $($arg)*)
    });
}

macro_rules! indent {
    ($state:expr, $fn:expr) => {
        #[allow(unused_assignments)]
        {
            $state.indent += 1;
            $fn;
            $state.indent -= 1;
        }
    };
}

#[derive(Debug)]
pub struct Syntax<'a> {
    pub name: &'a str,
    pub file_extensions: &'a [&'a str],
    pub first_line_match: Option<Pattern<'a>>,
    pub scope: Scope<'a>,
    pub hidden: bool,
    pub variables: &'a [(&'a str, Pattern<'a>)],
    pub contexts: &'a [(&'a str, Context<'a>)],
}

impl Syntax<'_> {
    pub fn serialize(&self, output: &mut dyn Write) -> Result<(), Error> {
        let mut state = SerializeState { indent: 0, output };

        serializeln!(state, "%YAML 1.2")?;
        serializeln!(state, "---")?;
        serializeln!(state, "# http://www.sublimetext.com/docs/syntax.html")?;
        serializeln!(state, "version: 2")?;
        serializeln!(state, "name: {}", self.name)?;

        if !self.file_extensions.is_empty() {
            serializeln!(state, "file_extensions:")?;

            for extension in self.file_extensions {
                serializeln!(state, "  - {}", extension)?;
            }
        }

        if let Some(pattern) = &self.first_line_match {
            state.write_indentation()?;
            write!(&mut state.output, "first_line_match: ")?;
            pattern.serializeln(&mut state)?;
        }

        if !self.scope.is_empty() {
            serializeln!(state, "scope: {}", self.scope)?;
        }

        if self.hidden {
            serializeln!(state, "hidden: true")?;
        }

        if !self.variables.is_empty() {
            serializeln!(state, "variables:")?;

            for (name, pattern) in self.variables {
                state.write_indentation()?;
                write!(&mut state.output, "  {}: ", name)?;
                pattern.serializeln(&mut state)?;
            }
        }

        if !self.contexts.is_empty() {
            serializeln!(state, "contexts:")?;

            indent!(state, {
                for (name, context) in self.contexts {
                    if let Some(comment) = context.comment {
                        for line in comment.lines() {
                            serializeln!(state, "# {}", line)?;
                        }
                    }

                    serializeln!(state, "{}:", name)?;
                    indent!(state, {
                        context.serialize(&mut state)?;
                    });
                }
            });
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Pattern<'a>(pub &'a str);

impl Pattern<'_> {
    fn serializeln(&self, state: &mut SerializeState) -> Result<(), Error> {
        if self.0.find('\n').is_some() {
            writeln!(state.output, "|-")?;
            indent!(state, {
                indent!(state, {
                    for line in self.0.split('\n') {
                        serializeln!(state, "{}", line)?;
                    }
                });
            });
        } else {
            writeln!(state.output, "'{}'", self.0.replace("\\'", "''"))?;
        }

        Ok(())
    }
}

impl<'a> From<&'a str> for Pattern<'a> {
    fn from(regex: &'a str) -> Pattern<'a> {
        Pattern(regex)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Scope<'a>(&'a str);

impl<'a> Scope<'a> {
    pub const EMPTY: Scope<'static> = Scope("");

    pub fn new(scope: &'a str) -> Scope<'a> {
        let tmp_alloc = bumpalo::Bump::new();
        debug_assert!(Scope::parse(scope, &tmp_alloc).0 == scope);
        Scope(scope)
    }

    pub fn as_str(&self) -> &'a str {
        self.0
    }

    pub fn parse(scopes: &str, allocator: &'a Bump) -> Self {
        let mut s = String::new();
        for (i, part) in scopes.split_ascii_whitespace().enumerate() {
            if i != 0 {
                write!(s, " ").unwrap();
            }
            s.push_str(part);
        }
        Scope(allocator.alloc_str(&s))
    }

    pub fn parse_with_postfix(
        scopes: &str,
        postfix: &str,
        allocator: &'a Bump,
    ) -> Scope<'a> {
        let mut s = String::new();
        for (i, part) in scopes.split_ascii_whitespace().enumerate() {
            if i != 0 {
                write!(s, " ").unwrap();
            }
            s.push_str(part);
            s.push('.');
            s.push_str(postfix);
        }
        Scope(allocator.alloc_str(&s))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn extended(&self, other: Self, allocator: &'a Bump) -> Self {
        if self.is_empty() {
            other
        } else if other.is_empty() {
            *self
        } else {
            Scope(
                bumpalo::format!(in allocator, "{} {}", self.0, other.0)
                    .into_bump_str(),
            )
        }
    }
}

impl std::fmt::Display for Scope<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }

        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ScopeClear {
    All,
    Amount(i32),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Context<'a> {
    pub meta_scope: Scope<'a>,
    pub meta_content_scope: Scope<'a>,
    pub meta_include_prototype: bool,
    pub clear_scopes: ScopeClear,
    pub matches: &'a [ContextPattern<'a>],
    pub comment: Option<&'a str>,
}

impl Context<'_> {
    fn serialize(&self, state: &mut SerializeState) -> Result<(), Error> {
        if !self.meta_scope.is_empty() {
            serializeln!(state, "- meta_scope: {}", self.meta_scope)?;
        }

        if !self.meta_content_scope.is_empty() {
            serializeln!(
                state,
                "- meta_content_scope: {}",
                self.meta_content_scope
            )?;
        }

        if !self.meta_include_prototype {
            serializeln!(state, "- meta_include_prototype: false")?;
        }

        match self.clear_scopes {
            ScopeClear::All => {
                serializeln!(state, "- clear_scopes: true")?;
            }
            ScopeClear::Amount(0) => {}
            ScopeClear::Amount(amount) => {
                serializeln!(state, "- clear_scopes: {}", amount)?;
            }
        }

        for pattern in self.matches {
            pattern.serialize(state)?;
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ContextPattern<'a> {
    Match(Match<'a>),
    Include(&'a str),
}

impl ContextPattern<'_> {
    fn serialize(&self, state: &mut SerializeState) -> Result<(), Error> {
        match self {
            ContextPattern::Match(m) => {
                m.serialize(state)?;
            }
            ContextPattern::Include(context) => {
                serializeln!(state, "- include: {}", context)?;
            }
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Match<'a> {
    pub pattern: Pattern<'a>,
    pub scope: Scope<'a>,
    pub captures: &'a [Scope<'a>],
    pub change_context: ContextChange<'a>,
    pub pop: u16,
}

impl Match<'_> {
    fn serialize(&self, state: &mut SerializeState) -> Result<(), Error> {
        state.write_indentation()?;
        write!(state.output, "- match: ")?;
        self.pattern.serializeln(state)?;

        indent!(state, {
            if !self.scope.is_empty() {
                serializeln!(state, "scope: {}", self.scope)?;
            }

            write_captures(state, "captures", self.captures)?;

            match &self.change_context {
                ContextChange::None => {}
                ContextChange::Push(contexts) => {
                    state.write_indentation()?;
                    write!(&mut state.output, "push: ")?;
                    write_context_list(state, contexts)?;
                }
                ContextChange::PushOne(context) => {
                    state.write_indentation()?;
                    writeln!(&mut state.output, "push: {}", context)?;
                }
                ContextChange::Set(contexts) => {
                    state.write_indentation()?;
                    write!(&mut state.output, "set: ")?;
                    write_context_list(state, contexts)?;
                }
                ContextChange::PushEmbed(context) => {
                    serializeln!(state, "push:")?;
                    indent!(state, {
                        context.serialize(state)?;
                    });
                }
                ContextChange::SetEmbed(context) => {
                    serializeln!(state, "set:")?;
                    indent!(state, {
                        context.serialize(state)?;
                    });
                }
                ContextChange::Embed(embed) => {
                    serializeln!(state, "embed: {}", embed.embed)?;
                    if !embed.embed_scope.is_empty() {
                        serializeln!(
                            state,
                            "embed_scope: {}",
                            embed.embed_scope
                        )?;
                    }
                    if let Some(pattern) = &embed.escape {
                        state.write_indentation()?;
                        write!(state.output, "escape: ")?;
                        pattern.serializeln(state)?;
                    }
                    write_captures(
                        state,
                        "escape_captures",
                        embed.escape_captures,
                    )?;
                }
                ContextChange::IncludeEmbed(embed) => {
                    if embed.use_push {
                        serializeln!(state, "push: {}", embed.path)?;
                    } else {
                        serializeln!(state, "set: {}", embed.path)?;
                    }

                    if !embed.with_prototype.is_empty() {
                        serializeln!(state, "with_prototype:")?;
                        indent!(state, {
                            for pattern in embed.with_prototype {
                                pattern.serialize(state)?;
                            }
                        });
                    }
                }
                ContextChange::Branch(branch_point, branches) => {
                    serializeln!(state, "branch_point: {}", branch_point)?;
                    serializeln!(state, "branch:")?;
                    assert!(branches.len() > 1);
                    for branch in *branches {
                        serializeln!(state, "  - {}", branch)?;
                    }
                }
                ContextChange::Fail(branch_point) => {
                    serializeln!(state, "fail: {}", branch_point)?;
                }
            }

            if self.pop == 1 {
                serializeln!(state, "pop: true")?;
            } else if self.pop > 0 {
                serializeln!(state, "pop: {}", self.pop)?;
            }
        });

        Ok(())
    }
}

fn write_captures(
    state: &mut SerializeState,
    name: &str,
    captures: &[Scope],
) -> Result<(), Error> {
    if !captures.is_empty() {
        serializeln!(state, "{}:", name)?;

        for (i, scope) in captures.iter().enumerate() {
            if !scope.is_empty() {
                serializeln!(state, "  {}: {}", i, scope)?;
            }
        }
    }

    Ok(())
}

fn write_context_list(
    state: &mut SerializeState,
    list: &[&str],
) -> Result<(), Error> {
    if list.len() == 1 {
        writeln!(&mut state.output, "{}", list[0])
    } else {
        assert!(list.len() > 1);

        write!(&mut state.output, "[{}", list[0])?;
        for c in &list[1..] {
            write!(&mut state.output, ", {}", c)?;
        }
        writeln!(&mut state.output, "]")
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Embed<'a> {
    pub embed: &'a str,
    pub embed_scope: Scope<'a>,
    pub escape: Option<Pattern<'a>>,
    pub escape_captures: &'a [Scope<'a>],
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct IncludeEmbed<'a> {
    pub path: &'a str,
    pub use_push: bool,
    pub with_prototype: &'a [ContextPattern<'a>],
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ContextChange<'a> {
    None,
    Push(&'a [&'a str]),
    PushOne(&'a str),
    Set(&'a [&'a str]),
    PushEmbed(Context<'a>),
    SetEmbed(Context<'a>),
    Embed(Embed<'a>),
    IncludeEmbed(IncludeEmbed<'a>),
    Branch(&'a str, &'a [&'a str]),
    Fail(&'a str),
}

#[cfg(test)]
mod tests {
    use crate::sublime_syntax::*;

    #[test]
    fn serialize_empty_syntax() {
        let syntax = Syntax {
            name: "Empty Lang",
            file_extensions: &["tes", "test"],
            first_line_match: Some(Pattern::from(".*\\bfoo\\b")),
            scope: Scope::new("source.empty"),
            hidden: true,
            variables: &[],
            contexts: &[],
        };

        let mut buf = String::new();
        syntax.serialize(&mut buf).unwrap();
        assert_eq!(
            buf,
            "\
%YAML 1.2
---
# http://www.sublimetext.com/docs/syntax.html
version: 2
name: Empty Lang
file_extensions:
  - tes
  - test
first_line_match: '.*\\bfoo\\b'
scope: source.empty
hidden: true\n"
        );
    }

    #[test]
    fn serialize_variables() {
        let syntax = Syntax {
            name: "Vars",
            file_extensions: &[],
            first_line_match: None,
            scope: Scope::new("source.vars text.vars"),
            hidden: false,
            variables: &[
                ("bar", Pattern::from("\\bbar\\b|\\bfoo\\b")),
                ("foo", Pattern::from("^foo\\b.*$")),
            ],
            contexts: &[],
        };

        let mut buf = String::new();
        syntax.serialize(&mut buf).unwrap();
        assert_eq!(
            buf,
            "\
%YAML 1.2
---
# http://www.sublimetext.com/docs/syntax.html
version: 2
name: Vars
scope: source.vars text.vars
variables:
  bar: '\\bbar\\b|\\bfoo\\b'
  foo: '^foo\\b.*$'\n"
        );
    }

    #[test]
    fn serialize_contexts() {
        let captures1 = [Scope::EMPTY, Scope::new("b")];
        let captures2 = [Scope::EMPTY, Scope::EMPTY, Scope::new("c")];
        let with_prototype = [ContextPattern::Match(Match {
            pattern: Pattern::from("c"),
            scope: Scope::new("c"),
            captures: &[],
            change_context: ContextChange::None,
            pop: 3,
        })];
        let matches = [
            ContextPattern::Match(Match {
                pattern: Pattern::from("(?=aa)"),
                scope: Scope::EMPTY,
                captures: &[],
                change_context: ContextChange::Embed(Embed {
                    embed: "Prolog.sublime-syntax",
                    embed_scope: Scope::EMPTY,
                    escape: Some(Pattern::from("</(p)>")),
                    escape_captures: &captures2,
                }),
                pop: 0,
            }),
            ContextPattern::Match(Match {
                pattern: Pattern::from("b"),
                scope: Scope::EMPTY,
                captures: &[],
                change_context: ContextChange::IncludeEmbed(IncludeEmbed {
                    path: "D.sublime-syntax",
                    use_push: true,
                    with_prototype: &with_prototype,
                }),
                pop: 0,
            }),
        ];

        let syntax = Syntax {
            name: "Ctx",
            file_extensions: &["ctx"],
            first_line_match: None,
            scope: Scope::new("source.ctx"),
            hidden: false,
            variables: &[],
            contexts: &[
                (
                    "bar",
                    Context {
                        meta_scope: Scope::EMPTY,
                        meta_content_scope: Scope::EMPTY,
                        meta_include_prototype: false,
                        clear_scopes: ScopeClear::Amount(0),
                        matches: &[ContextPattern::Match(Match {
                            pattern: Pattern::from("//"),
                            scope: Scope::new("b"),
                            captures: &[],
                            change_context: ContextChange::SetEmbed(Context {
                                meta_scope: Scope::new("c"),
                                meta_content_scope: Scope::EMPTY,
                                meta_include_prototype: true,
                                clear_scopes: ScopeClear::Amount(2),
                                matches: &matches,
                                comment: Some("inner"),
                            }),
                            pop: 2,
                        })],
                        comment: Some("foo\nbar"),
                    },
                ),
                (
                    "foo",
                    Context {
                        meta_scope: Scope::EMPTY,
                        meta_content_scope: Scope::new("a b"),
                        meta_include_prototype: true,
                        clear_scopes: ScopeClear::All,
                        matches: &[
                            ContextPattern::Include("bar"),
                            ContextPattern::Include("baz"),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from("\\ba(b)\\b"),
                                scope: Scope::EMPTY,
                                captures: &captures1,
                                change_context: ContextChange::None,
                                pop: 0,
                            }),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from("(?=\\()"),
                                scope: Scope::new("a b"),
                                captures: &[],
                                change_context: ContextChange::Push(&["foo"]),
                                pop: 0,
                            }),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from("(?={)"),
                                scope: Scope::new("a.b"),
                                captures: &[],
                                change_context: ContextChange::Push(&[
                                    "foo", "bar",
                                ]),
                                pop: 0,
                            }),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from(""),
                                scope: Scope::EMPTY,
                                captures: &[],
                                change_context: ContextChange::None,
                                pop: 1,
                            }),
                        ],
                        comment: None,
                    },
                ),
            ],
        };

        let mut buf = String::new();
        syntax.serialize(&mut buf).unwrap();
        assert_eq!(
            buf,
            r"%YAML 1.2
---
# http://www.sublimetext.com/docs/syntax.html
version: 2
name: Ctx
file_extensions:
  - ctx
scope: source.ctx
contexts:
  # foo
  # bar
  bar:
    - meta_include_prototype: false
    - match: '//'
      scope: b
      set:
        - meta_scope: c
        - clear_scopes: 2
        - match: '(?=aa)'
          embed: Prolog.sublime-syntax
          escape: '</(p)>'
          escape_captures:
            2: c
        - match: 'b'
          push: D.sublime-syntax
          with_prototype:
            - match: 'c'
              scope: c
              pop: 3
      pop: 2
  foo:
    - meta_content_scope: a b
    - clear_scopes: true
    - include: bar
    - include: baz
    - match: '\ba(b)\b'
      captures:
        1: b
    - match: '(?=\()'
      scope: a b
      push: foo
    - match: '(?={)'
      scope: a.b
      push: [foo, bar]
    - match: ''
      pop: true
"
        );
    }
}
