use crate::sublime_syntax;

pub mod codegen;
pub mod collector;
pub mod common;
pub mod interpreter;

use crate::sbnf::Grammar;
pub use common::{CompileOptions, CompileResult, Compiler, Error};

impl Compiler {
    pub fn compile<'a>(
        &mut self,
        options: &'a CompileOptions<'a>,
        grammar: &'a Grammar<'a>,
    ) -> CompileResult<sublime_syntax::Syntax> {
        let collection = collector::collect(self, options, grammar);

        let (mut warnings, collected) = match collection {
            CompileResult { result: Err(errors), warnings } => {
                return CompileResult::err(errors, warnings);
            }
            CompileResult { result: Ok(col), warnings } => (warnings, col),
        };

        let mut interpreter_result =
            interpreter::interpret(self, options, collected);

        warnings.append(&mut interpreter_result.warnings);

        if let Err(errors) = interpreter_result.result {
            return CompileResult::err(errors, warnings);
        }

        let interpreted = interpreter_result.result.unwrap();

        let syntax = codegen::codegen(self, interpreted);

        CompileResult::new(syntax, vec![], warnings)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::*;
    use crate::sbnf;
    use crate::sublime_syntax;
    use std::collections::HashMap;

    fn compile_matches(
        source: &str,
        arguments: Vec<&str>,
    ) -> HashMap<String, sublime_syntax::Context> {
        let grammar = sbnf::parse(source).unwrap();

        let options = CompileOptions {
            name_hint: Some("test"),
            arguments,
            debug_contexts: false,
            entry_points: vec!["main"],
        };
        let mut compiler = Compiler::new();
        let result = compiler.compile(&options, &grammar);

        if result.is_err() {
            for error in result.result.as_ref().unwrap_err() {
                println!(
                    "{}",
                    error.with_compiler_and_source(
                        &compiler, "ERROR", "test", source
                    )
                );
            }
        }
        assert!(result.is_ok());

        if !result.warnings.is_empty() {
            for warning in &result.warnings {
                println!(
                    "{}",
                    warning.with_compiler_and_source(
                        &compiler, "WARNING", "test", source
                    )
                );
            }
        }
        assert!(result.warnings.is_empty());

        let mut buf = String::new();
        result.result.as_ref().unwrap().serialize(&mut buf).unwrap();
        println!("{}", buf);

        result.result.unwrap().contexts
    }

    #[test]
    fn compile_simple_repetition() {
        let contexts = compile_matches("main : ('a'{a} 'b'{b})*;", vec![]);
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("a"),
                    scope: sublime_syntax::Scope::from_str(&["a.test"]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::Push(vec!(
                        "main|0".to_string()
                    )),
                    pop: 0,
                }),
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("\\S"),
                    scope: sublime_syntax::Scope::from_str(&[
                        "invalid.illegal.test"
                    ]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 0,
                }),
            ]
        );
        let main0 = contexts.get("main|0").unwrap();
        assert_eq!(
            main0.matches,
            [
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("b"),
                    scope: sublime_syntax::Scope::from_str(&["b.test"]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("\\S"),
                    scope: sublime_syntax::Scope::from_str(&[
                        "invalid.illegal.test"
                    ]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_branch() {
        let contexts = compile_matches(
            "main : (a | b)*; a{a} : 'c'{ac} 'a'; b{b} : 'c'{bc} 'b';",
            vec![],
        );
        assert_eq!(contexts.len(), 6);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                sublime_syntax::ContextPattern::Include(
                    "include!main@1".to_string()
                ),
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("\\S"),
                    scope: sublime_syntax::Scope::from_str(&[
                        "invalid.illegal.test"
                    ]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 0,
                }),
            ]
        );
        let branch_include = contexts.get("include!main@1").unwrap();
        assert_eq!(
            branch_include.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("(?=c)"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Branch(
                    "main@1".to_string(),
                    vec!("a|0|main@1".to_string(), "b|0|main@1".to_string())
                ),
                pop: 0,
            }),]
        );
        // First branch
        let a0main0 = contexts.get("a|0|main@1").unwrap();
        assert_eq!(
            a0main0.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("c"),
                scope: sublime_syntax::Scope::from_str(&["a.test", "ac.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!(
                    "main|0|main@1".to_string()
                )),
                pop: 1,
            }),]
        );
        let a1main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(
            a1main0.matches,
            [
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("a"),
                    scope: sublime_syntax::Scope::from_str(&["a.test"]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("\\S"),
                    scope: sublime_syntax::Scope::empty(),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::Fail(
                        "main@1".to_string()
                    ),
                    pop: 0,
                }),
            ]
        );
        // Second branch
        let b0main0 = contexts.get("b|0|main@1").unwrap();
        assert_eq!(
            b0main0.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("c"),
                scope: sublime_syntax::Scope::from_str(&["b.test", "bc.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!(
                    "main|1|main@1".to_string()
                )),
                pop: 1,
            }),]
        );
        let b1main0 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(
            b1main0.matches,
            [
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("b"),
                    scope: sublime_syntax::Scope::from_str(&["b.test"]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("\\S"),
                    scope: sublime_syntax::Scope::from_str(&[
                        "invalid.illegal.test"
                    ]),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_syntax_parameters() {
        let contexts = compile_matches(
            "[A]\n\
            NAME = '#[A]'\n\
            main : ( ~'a#[A]'{#[A]a} )* ;",
            vec!["b"],
        );
        assert_eq!(contexts.len(), 1);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("ab"),
                scope: sublime_syntax::Scope::from_str(&["ba.b"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 0,
            }),]
        );
    }

    #[test]
    fn compile_branch_repetition() {
        let contexts = compile_matches(
            "main : ( ~('start'{a} 'end' | 'start'{b} ) )* ;",
            vec![],
        );
        assert_eq!(contexts.len(), 5);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [sublime_syntax::ContextPattern::Include(
                "include!main@1".to_string()
            )]
        );

        let main_include = contexts.get("include!main@1").unwrap();
        assert_eq!(
            main_include.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("(?=start)"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Branch(
                    "main@1".to_string(),
                    vec![
                        "main|0|main@1".to_string(),
                        "main|2|main@1".to_string()
                    ]
                ),
                pop: 0,
            })]
        );

        let main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(
            main0.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("start"),
                scope: sublime_syntax::Scope::from_str(&["a.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec![
                    "main|1|main@1".to_string()
                ]),
                pop: 1,
            })]
        );

        let main1 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(
            main1.matches,
            [
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("end"),
                    scope: sublime_syntax::Scope::empty(),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::None,
                    pop: 1,
                }),
                sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                    pattern: sublime_syntax::Pattern::from_str("\\S"),
                    scope: sublime_syntax::Scope::empty(),
                    captures: HashMap::new(),
                    change_context: sublime_syntax::ContextChange::Fail(
                        "main@1".to_string()
                    ),
                    pop: 0,
                }),
            ]
        );

        let main2 = contexts.get("main|2|main@1").unwrap();
        assert_eq!(
            main2.matches,
            [sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("start"),
                scope: sublime_syntax::Scope::from_str(&["b.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 1,
            })]
        );
    }
}
