/// This file describes the structure of sublime-syntax files and defines a
/// serializer for it.
use hashbrown::HashMap;
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
    ($state:expr, $fn:expr) => {{
        $state.indent += 1;
        $fn;
        $state.indent -= 1;
    }};
}

#[derive(Debug)]
pub struct Syntax {
    pub name: String,
    pub file_extensions: Vec<String>,
    pub first_line_match: Option<Pattern>,
    pub scope: Scope,
    pub hidden: bool,
    pub variables: HashMap<String, Pattern>,
    pub contexts: HashMap<String, Context>,
}

impl Syntax {
    pub fn serialize(&self, output: &mut dyn Write) -> Result<(), Error> {
        let mut state = SerializeState { indent: 0, output };

        serializeln!(state, "%YAML 1.2")?;
        serializeln!(state, "---")?;
        serializeln!(state, "# http://www.sublimetext.com/docs/syntax.html")?;
        serializeln!(state, "version: 2")?;
        serializeln!(state, "name: {}", self.name)?;

        if !self.file_extensions.is_empty() {
            serializeln!(state, "file_extensions:")?;

            for extension in &self.file_extensions {
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

            let mut keys = self.variables.keys().collect::<Vec<&String>>();
            keys.sort();
            for key in &keys {
                state.write_indentation()?;
                write!(&mut state.output, "  {}: ", key)?;
                self.variables
                    .get::<str>(key)
                    .unwrap()
                    .serializeln(&mut state)?;
            }
        }

        if !self.contexts.is_empty() {
            serializeln!(state, "contexts:")?;

            let mut keys = self.contexts.keys().collect::<Vec<&String>>();
            keys.sort();
            indent!(state, {
                for key in &keys {
                    let context = self.contexts.get::<str>(key).unwrap();

                    if let Some(comment) = &context.comment {
                        for line in comment.lines() {
                            serializeln!(state, "# {}", line)?;
                        }
                    }

                    serializeln!(state, "{}:", key)?;
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
pub struct Pattern {
    pub regex: String,
}

impl Pattern {
    pub fn new(regex: String) -> Pattern {
        Pattern { regex }
    }

    fn serializeln(&self, state: &mut SerializeState) -> Result<(), Error> {
        if self.regex.find('\n').is_some() {
            writeln!(state.output, "|-")?;
            indent!(state, {
                indent!(state, {
                    for line in self.regex.split('\n') {
                        serializeln!(state, "{}", line)?;
                    }
                });
            });
        } else {
            writeln!(state.output, "'{}'", self.regex.replace("\\'", "''"))?;
        }

        Ok(())
    }
}

impl From<&str> for Pattern {
    fn from(regex: &str) -> Pattern {
        Pattern { regex: regex.to_string() }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Scope(pub String);

impl Scope {
    pub fn empty() -> Scope {
        Scope(String::new())
    }

    pub fn new(scope: String) -> Scope {
        debug_assert!(Self::parse(&scope).0 == scope);
        Scope(scope)
    }

    pub fn parse(scopes: &str) -> Scope {
        let mut s = String::new();
        for (i, part) in scopes.split_ascii_whitespace().enumerate() {
            if i != 0 {
                write!(s, " ").unwrap();
            }
            s.push_str(part);
        }
        Scope(s)
    }

    pub fn parse_with_postfix(scopes: &str, postfix: &str) -> Scope {
        let mut s = String::new();
        for (i, part) in scopes.split_ascii_whitespace().enumerate() {
            if i != 0 {
                write!(s, " ").unwrap();
            }
            s.push_str(part);
            s.push('.');
            s.push_str(postfix);
        }
        Scope(s)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn prepend(&mut self, other: &Scope) {
        if self.is_empty() {
            *self = other.clone();
        } else if !other.is_empty() {
            self.0.insert(0, ' ');
            self.0.insert_str(0, &other.0);
        }
    }

    pub fn extend(&mut self, other: &Scope) {
        if self.is_empty() {
            *self = other.clone();
        } else if !other.is_empty() {
            self.0.push(' ');
            self.0.push_str(&other.0);
        }
    }
}

impl std::fmt::Display for Scope {
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
pub struct Context {
    pub meta_scope: Scope,
    pub meta_content_scope: Scope,
    pub meta_include_prototype: bool,
    pub clear_scopes: ScopeClear,
    pub matches: Vec<ContextPattern>,
    pub comment: Option<String>,
}

impl Context {
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

        for pattern in &self.matches {
            pattern.serialize(state)?;
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ContextPattern {
    Match(Match),
    Include(String),
}

impl ContextPattern {
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
pub struct Match {
    pub pattern: Pattern,
    pub scope: Scope,
    pub captures: Vec<Scope>,
    pub change_context: ContextChange,
    pub pop: u16,
}

impl Match {
    fn serialize(&self, state: &mut SerializeState) -> Result<(), Error> {
        state.write_indentation()?;
        write!(state.output, "- match: ")?;
        self.pattern.serializeln(state)?;

        indent!(state, {
            if !self.scope.is_empty() {
                serializeln!(state, "scope: {}", self.scope)?;
            }

            write_captures(state, "captures", &self.captures)?;

            match &self.change_context {
                ContextChange::None => {}
                ContextChange::Push(contexts) => {
                    state.write_indentation()?;
                    write!(&mut state.output, "push: ")?;
                    write_context_list(state, contexts)?;
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
                        &embed.escape_captures,
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
                            for pattern in &embed.with_prototype {
                                pattern.serialize(state)?;
                            }
                        });
                    }
                }
                ContextChange::Branch(branch_point, branches) => {
                    serializeln!(state, "branch_point: {}", branch_point)?;
                    serializeln!(state, "branch:")?;
                    assert!(branches.len() > 1);
                    for branch in branches {
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
    captures: &Vec<Scope>,
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
    list: &Vec<String>,
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
pub struct Embed {
    pub embed: String,
    pub embed_scope: Scope,
    pub escape: Option<Pattern>,
    pub escape_captures: Vec<Scope>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct IncludeEmbed {
    pub path: String,
    pub use_push: bool,
    pub with_prototype: Vec<ContextPattern>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ContextChange {
    None,
    Push(Vec<String>),
    Set(Vec<String>),
    PushEmbed(Context),
    SetEmbed(Context),
    Embed(Embed),
    IncludeEmbed(IncludeEmbed),
    Branch(String, Vec<String>),
    Fail(String),
}

#[cfg(test)]
mod tests {
    use hashbrown::HashMap;

    use crate::sublime_syntax::*;

    #[test]
    fn serialize_empty_syntax() {
        let syntax = Syntax {
            name: "Empty Lang".to_string(),
            file_extensions: vec!["tes".to_string(), "test".to_string()],
            first_line_match: Some(Pattern::from(".*\\bfoo\\b")),
            scope: Scope::parse("source.empty"),
            hidden: true,
            variables: HashMap::new(),
            contexts: HashMap::new(),
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
            name: "Vars".to_string(),
            file_extensions: vec![],
            first_line_match: None,
            scope: Scope::parse("source.vars text.vars"),
            hidden: false,
            variables: {
                let mut m = HashMap::new();
                m.insert("foo".to_string(), Pattern::from("^foo\\b.*$"));
                m.insert(
                    "bar".to_string(),
                    Pattern::from("\\bbar\\b|\\bfoo\\b"),
                );
                m
            },
            contexts: HashMap::new(),
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
        let syntax = Syntax {
            name: "Ctx".to_string(),
            file_extensions: vec!["ctx".to_string()],
            first_line_match: None,
            scope: Scope::parse("source.ctx"),
            hidden: false,
            variables: HashMap::new(),
            contexts: {
                let mut m = HashMap::new();
                m.insert(
                    "foo".to_string(),
                    Context {
                        meta_scope: Scope::empty(),
                        meta_content_scope: Scope::parse("a b"),
                        meta_include_prototype: true,
                        clear_scopes: ScopeClear::All,
                        matches: vec![
                            ContextPattern::Include("bar".to_string()),
                            ContextPattern::Include("baz".to_string()),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from("\\ba(b)\\b"),
                                scope: Scope::empty(),
                                captures: vec![
                                    Scope::empty(),
                                    Scope::parse("b"),
                                ],
                                change_context: ContextChange::None,
                                pop: 0,
                            }),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from("(?=\\()"),
                                scope: Scope::parse("a b"),
                                captures: vec![],
                                change_context: ContextChange::Push(vec![
                                    "foo".to_string(),
                                ]),
                                pop: 0,
                            }),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from("(?={)"),
                                scope: Scope::parse("a.b"),
                                captures: vec![],
                                change_context: ContextChange::Push(vec![
                                    "foo".to_string(),
                                    "bar".to_string(),
                                ]),
                                pop: 0,
                            }),
                            ContextPattern::Match(Match {
                                pattern: Pattern::from(""),
                                scope: Scope::empty(),
                                captures: vec![],
                                change_context: ContextChange::None,
                                pop: 1,
                            }),
                        ],
                        comment: None,
                    },
                );
                m.insert(
                    "bar".to_string(),
                    Context {
                        meta_scope: Scope::empty(),
                        meta_content_scope: Scope::empty(),
                        meta_include_prototype: false,
                        clear_scopes: ScopeClear::Amount(0),
                        matches: vec![ContextPattern::Match(Match {
                            pattern: Pattern::from("//"),
                            scope: Scope::parse("b"),
                            captures: vec![],
                            change_context: ContextChange::SetEmbed(Context {
                                meta_scope: Scope::parse("c"),
                                meta_content_scope: Scope::empty(),
                                meta_include_prototype: true,
                                clear_scopes: ScopeClear::Amount(2),
                                matches: vec![
                                    ContextPattern::Match(Match {
                                        pattern: Pattern::from("(?=aa)"),
                                        scope: Scope::empty(),
                                        captures: vec![],
                                        change_context: ContextChange::Embed(
                                            Embed {
                                                embed: "Prolog.sublime-syntax"
                                                    .to_string(),
                                                embed_scope: Scope::empty(),
                                                escape: Some(Pattern::from(
                                                    "</(p)>",
                                                )),
                                                escape_captures: vec![
                                                    Scope::empty(),
                                                    Scope::empty(),
                                                    Scope::parse("c"),
                                                ],
                                            },
                                        ),
                                        pop: 0,
                                    }),
                                    ContextPattern::Match(Match {
                                        pattern: Pattern::from("b"),
                                        scope: Scope::empty(),
                                        captures: vec![],
                                        change_context:
                                            ContextChange::IncludeEmbed(
                                                IncludeEmbed {
                                                    path: "D.sublime-syntax"
                                                        .to_string(),
                                                    use_push: true,
                                                    with_prototype: vec!(
                                                ContextPattern::Match(Match {
                                                    pattern: Pattern::from("c"),
                                                    scope: Scope::parse("c"),
                                                    captures: vec![],
                                                    change_context:
                                                        ContextChange::None,
                                                    pop: 3,
                                                }),
                                            ),
                                                },
                                            ),
                                        pop: 0,
                                    }),
                                ],
                                comment: Some("inner".to_string()),
                            }),
                            pop: 2,
                        })],
                        comment: Some("foo\nbar".to_string()),
                    },
                );
                m
            },
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
