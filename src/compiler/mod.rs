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
    use crate::sublime_syntax::{
        Context, ContextChange, ContextPattern, Match, Pattern, Scope,
    };
    use hashbrown::HashMap;

    fn compile_matches(
        source: &str,
        arguments: Vec<&str>,
    ) -> HashMap<String, Context> {
        let grammar = sbnf::parse(source).unwrap();

        let options = CompileOptions {
            name_hint: Some("test"),
            arguments,
            debug_contexts: false,
            entry_points: vec!["main"],
        };
        let mut compiler = Compiler::default();
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
    fn compile_simple() {
        let contexts = compile_matches("main : 'a'{a};", vec![]);
        assert_eq!(contexts.len(), 1);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::parse("a.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_repetition() {
        let contexts = compile_matches("main : ('a'{a} 'b'{b})*;", vec![]);
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::parse("a.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec!(
                        "main|0".to_string()
                    )),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let main0 = contexts.get("main|0").unwrap();
        assert_eq!(
            main0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::parse("b.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_alternation() {
        let contexts = compile_matches("main : 'a'{a} | 'b'{b} ;", vec![]);
        assert_eq!(contexts.len(), 1);
        let main = &contexts["main"];
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::parse("a.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::parse("b.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_concatenation() {
        let contexts = compile_matches("main : 'a' 'b'? 'c';", vec![]);
        assert_eq!(contexts.len(), 3);
        let main = &contexts["main"];
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "main|0".to_string()
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let main0 = &contexts["main|0"];
        assert_eq!(
            main0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "main|1".to_string()
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("c"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let main1 = &contexts["main|1"];
        assert_eq!(
            main1.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("c"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_recursion() {
        let contexts =
            compile_matches("main : r* ; r{r} : '{' r* '}' ; ", vec![]);
        assert_eq!(contexts.len(), 2);
        let main = &contexts["main"];
        assert_eq!(main.meta_content_scope, Scope::empty());
        assert_eq!(main.meta_scope, Scope::empty());
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("{"),
                    scope: Scope::parse("r.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(
                        vec!["r|0".to_string()]
                    ),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );

        let r = &contexts["r|0"];
        assert_eq!(r.meta_content_scope, Scope::parse("r.test"));
        assert_eq!(r.meta_scope, Scope::empty());
        assert_eq!(
            r.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("{"),
                    scope: Scope::parse("r.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(
                        vec!["r|0".to_string()]
                    ),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("}"),
                    scope: Scope::parse("r.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_repetition_in_stack() {
        let contexts = compile_matches(
            "main : ( ~block )* ; block{block} : `{` ('a' | block)* `}` ;",
            vec![],
        );
        assert_eq!(contexts.len(), 2);
        let main = &contexts["main"];
        assert_eq!(main.meta_content_scope, Scope::empty());
        assert_eq!(main.meta_scope, Scope::empty());
        assert_eq!(
            main.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("\\{"),
                scope: Scope::parse("block.test"),
                captures: HashMap::new(),
                change_context: ContextChange::Push(
                    vec!["block|0".to_string()]
                ),
                pop: 0,
            }),]
        );

        let block0 = contexts.get("block|0").unwrap();
        assert_eq!(block0.meta_content_scope, Scope::parse("block.test"));
        assert_eq!(block0.meta_scope, Scope::empty());
        assert_eq!(
            block0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\{"),
                    scope: Scope::parse("block.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "block|0".to_string(),
                    ]),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\}"),
                    scope: Scope::parse("block.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_repeated_concatenation() {
        let contexts = compile_matches(
            "main : ( ~a )* ; a{a} : 'a' ('b' 'c')* 'd' ;",
            vec![],
        );
        assert_eq!(contexts.len(), 3);
        let main = &contexts["main"];
        assert_eq!(main.meta_content_scope, Scope::empty());
        assert_eq!(main.meta_scope, Scope::empty());
        assert_eq!(
            main.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("a"),
                scope: Scope::parse("a.test"),
                captures: HashMap::new(),
                change_context: ContextChange::Push(vec!["a|0".to_string(),]),
                pop: 0,
            }),]
        );
        let a0 = &contexts["a|0"];
        assert_eq!(a0.meta_content_scope, Scope::parse("a.test"));
        assert_eq!(a0.meta_scope, Scope::empty());
        assert_eq!(
            a0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(
                        vec!["a|1".to_string()],
                    ),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("d"),
                    scope: Scope::parse("a.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let a1 = &contexts["a|1"];
        assert_eq!(a1.meta_content_scope, Scope::empty());
        assert_eq!(a1.meta_scope, Scope::empty());
        assert_eq!(
            a1.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("c"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
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
                ContextPattern::Include("include!main@1".to_string()),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let branch_include = contexts.get("include!main@1").unwrap();
        assert_eq!(
            branch_include.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("(?=c)"),
                scope: Scope::empty(),
                captures: HashMap::new(),
                change_context: ContextChange::Branch(
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
            [ContextPattern::Match(Match {
                pattern: Pattern::from("c"),
                scope: Scope::parse("a.test ac.test"),
                captures: HashMap::new(),
                change_context: ContextChange::Push(vec!(
                    "main|0|main@1".to_string()
                )),
                pop: 1,
            }),]
        );
        let a1main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(
            a1main0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::parse("a.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 2,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Fail("main@1".to_string()),
                    pop: 0,
                }),
            ]
        );
        // Second branch
        let b0main0 = contexts.get("b|0|main@1").unwrap();
        assert_eq!(
            b0main0.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("c"),
                scope: Scope::parse("b.test bc.test"),
                captures: HashMap::new(),
                change_context: ContextChange::Push(vec!(
                    "main|1|main@1".to_string()
                )),
                pop: 1,
            }),]
        );
        let b1main0 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(
            b1main0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::parse("b.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 2,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
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
            [ContextPattern::Match(Match {
                pattern: Pattern::from("ab"),
                scope: Scope::parse("ba.b"),
                captures: HashMap::new(),
                change_context: ContextChange::None,
                pop: 0,
            }),]
        );
    }

    #[test]
    fn compile_branch_repetition() {
        let contexts = compile_matches(
            "main : ( ~('start'{a} 'end' | 'start'{b} | 'start'{c} 'mid' 'end' ) )* ;",
            vec![],
        );
        assert_eq!(contexts.len(), 8);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [ContextPattern::Include("include!main@1".to_string())]
        );

        let main_include = contexts.get("include!main@1").unwrap();
        assert_eq!(
            main_include.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("(?=start)"),
                scope: Scope::empty(),
                captures: HashMap::new(),
                change_context: ContextChange::Branch(
                    "main@1".to_string(),
                    vec![
                        "main|0|main@1".to_string(),
                        "main|2|main@1".to_string(),
                        "main|3|main@1".to_string(),
                    ]
                ),
                pop: 0,
            })]
        );

        let main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(
            main0.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("start"),
                scope: Scope::parse("a.test"),
                captures: HashMap::new(),
                change_context: ContextChange::Push(vec![
                    "main|1|main@1".to_string()
                ]),
                pop: 1,
            })]
        );

        let main1 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(
            main1.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("end"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 2,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Fail("main@1".to_string()),
                    pop: 0,
                }),
            ]
        );

        let main2 = contexts.get("main|2|main@1").unwrap();
        assert_eq!(
            main2.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("start"),
                scope: Scope::parse("b.test"),
                captures: HashMap::new(),
                change_context: ContextChange::None,
                pop: 1,
            })]
        );

        let main3 = contexts.get("main|3|main@1").unwrap();
        assert_eq!(
            main3.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("start"),
                scope: Scope::parse("c.test"),
                captures: HashMap::new(),
                change_context: ContextChange::Push(vec![
                    "main|4|main@1".to_string()
                ]),
                pop: 1,
            })]
        );

        let main4 = contexts.get("main|4|main@1").unwrap();
        assert_eq!(
            main4.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("mid"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "main|5".to_string()
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );

        let main5 = contexts.get("main|5").unwrap();
        assert_eq!(
            main5.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("end"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_repetition_scopes() {
        let contexts = compile_matches(
            "main : a (',' a)* ; a{a} : 'a'{ra} | b ; b{b} : 'b'{rb} 'c'{rc} ;",
            vec![],
        );
        assert_eq!(contexts.len(), 5);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::parse("a.test ra.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "main|0".to_string(),
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::parse("a.test b.test rb.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "main|0".to_string(),
                        "a|meta".to_string(),
                        "b|0".to_string(),
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_left_recursion() {
        let contexts = compile_matches("main : a ; a : a 'a' | 'b' ;", vec![]);
        // Gets rewritten as: main : 'b' main|lr0 ; main|lr0 : 'a' main|lr0 ;
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::Push(vec![
                        "a|0".to_string(),
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::parse("invalid.illegal.test"),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let a0 = contexts.get("a|0").unwrap();
        assert_eq!(
            a0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::empty(),
                    captures: HashMap::new(),
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        )
    }
}
