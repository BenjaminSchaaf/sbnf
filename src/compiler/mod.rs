use crate::sublime_syntax;

pub mod codegen;
pub mod collector;
pub mod common;
pub mod interpreter;

use crate::sbnf::Grammar;
pub use common::{CompileOptions, CompileResult, Compiler, Error};

impl Compiler {
    pub fn compile<'a>(
        &'a self,
        options: &CompileOptions<'a>,
        grammar: &Grammar<'a>,
    ) -> CompileResult<sublime_syntax::Syntax<'a>> {
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

    fn compile_matches<'a>(
        compiler: &'a Compiler,
        source: &'a str,
        arguments: Vec<&'a str>,
    ) -> HashMap<&'a str, Context<'a>> {
        // TODO: Fix needing these leaks.
        let grammar = sbnf::parse(source, &compiler.allocator).unwrap();

        let options = CompileOptions {
            name_hint: Some("test"),
            arguments,
            debug_contexts: false,
            entry_points: vec!["main"],
        };
        let result = compiler.compile(&options, &grammar);

        if result.is_err() {
            for error in result.result.as_ref().unwrap_err() {
                println!(
                    "{}",
                    error.with_compiler_and_source(
                        compiler, "ERROR", "test", source
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
                        compiler, "WARNING", "test", source
                    )
                );
            }
        }
        assert!(result.warnings.is_empty());

        let mut buf = String::new();
        result.result.as_ref().unwrap().serialize(&mut buf).unwrap();
        println!("{}", buf);

        result
            .result
            .unwrap()
            .contexts
            .iter()
            .cloned()
            .collect::<HashMap<_, _>>()
    }

    #[test]
    fn compile_simple() {
        let compiler = Compiler::default();
        let contexts = compile_matches(&compiler, "main : 'a'{a};", vec![]);
        assert_eq!(contexts.len(), 1);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::new("a.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_repetition() {
        let compiler = Compiler::default();
        let contexts =
            compile_matches(&compiler, "main : ('a'{a} 'b'{b})*;", vec![]);
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::new("a.test"),
                    captures: &[],
                    change_context: ContextChange::Push(&["main|0"]),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::EMPTY,
                    captures: &[],
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
                    scope: Scope::new("b.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_alternation() {
        let compiler = Compiler::default();
        let contexts =
            compile_matches(&compiler, "main : 'a'{a} | 'b'{b} ;", vec![]);
        assert_eq!(contexts.len(), 1);
        let main = &contexts["main"];
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::new("a.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::new("b.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_concatenation() {
        let compiler = Compiler::default();
        let contexts =
            compile_matches(&compiler, "main : 'a' 'b'? 'c';", vec![]);
        assert_eq!(contexts.len(), 3);
        let main = &contexts["main"];
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Push(&["main|0"]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
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
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Push(&["main|1"]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("c"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
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
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_recursion() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
            "main : r* ; r{r} : '{' r* '}' ; ",
            vec![],
        );
        assert_eq!(contexts.len(), 2);
        let main = &contexts["main"];
        assert_eq!(main.meta_content_scope, Scope::EMPTY);
        assert_eq!(main.meta_scope, Scope::EMPTY);
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("{"),
                    scope: Scope::new("r.test"),
                    captures: &[],
                    change_context: ContextChange::Push(&["r|0"]),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );

        let r = &contexts["r|0"];
        assert_eq!(r.meta_content_scope, Scope::new("r.test"));
        assert_eq!(r.meta_scope, Scope::EMPTY);
        assert_eq!(
            r.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("{"),
                    scope: Scope::new("r.test"),
                    captures: &[],
                    change_context: ContextChange::Push(&["r|0"]),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("}"),
                    scope: Scope::new("r.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_repetition_in_stack() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
            "main : ( ~block )* ; block{block} : `{` ('a' | block)* `}` ;",
            vec![],
        );
        assert_eq!(contexts.len(), 2);
        let main = &contexts["main"];
        assert_eq!(main.meta_content_scope, Scope::EMPTY);
        assert_eq!(main.meta_scope, Scope::EMPTY);
        assert_eq!(
            main.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("\\{"),
                scope: Scope::new("block.test"),
                captures: &[],
                change_context: ContextChange::Push(&["block|0"]),
                pop: 0,
            }),]
        );

        let block0 = contexts.get("block|0").unwrap();
        assert_eq!(block0.meta_content_scope, Scope::new("block.test"));
        assert_eq!(block0.meta_scope, Scope::EMPTY);
        assert_eq!(
            block0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\{"),
                    scope: Scope::new("block.test"),
                    captures: &[],
                    change_context: ContextChange::Push(&["block|0"]),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\}"),
                    scope: Scope::new("block.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_repeated_concatenation() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
            "main : ( ~a )* ; a{a} : 'a' ('b' 'c')* 'd' ;",
            vec![],
        );
        assert_eq!(contexts.len(), 3);
        let main = &contexts["main"];
        assert_eq!(main.meta_content_scope, Scope::EMPTY);
        assert_eq!(main.meta_scope, Scope::EMPTY);
        assert_eq!(
            main.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("a"),
                scope: Scope::new("a.test"),
                captures: &[],
                change_context: ContextChange::Push(&["a|0"]),
                pop: 0,
            }),]
        );
        let a0 = &contexts["a|0"];
        assert_eq!(a0.meta_content_scope, Scope::new("a.test"));
        assert_eq!(a0.meta_scope, Scope::EMPTY);
        assert_eq!(
            a0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Push(&["a|1"]),
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("d"),
                    scope: Scope::new("a.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let a1 = &contexts["a|1"];
        assert_eq!(a1.meta_content_scope, Scope::EMPTY);
        assert_eq!(a1.meta_scope, Scope::EMPTY);
        assert_eq!(
            a1.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("c"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_branch() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
            "main : (a | b)*; a{a} : 'c'{ac} 'a'; b{b} : 'c'{bc} 'b';",
            vec![],
        );
        assert_eq!(contexts.len(), 6);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Include("include!main@1"),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::EMPTY,
                    captures: &[],
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
                scope: Scope::EMPTY,
                captures: &[],
                change_context: ContextChange::Branch(
                    "main@1",
                    &["a|0|main@1", "b|0|main@1"]
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
                scope: Scope::new("a.test ac.test"),
                captures: &[],
                change_context: ContextChange::PushOne("main|0|main@1"),
                pop: 1,
            }),]
        );
        let a1main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(a1main0.meta_content_scope, Scope::new("a.test"));
        assert_eq!(
            a1main0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::new("a.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Fail("main@1"),
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
                scope: Scope::new("b.test bc.test"),
                captures: &[],
                change_context: ContextChange::PushOne("main|1|main@1"),
                pop: 1,
            }),]
        );
        let b1main0 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(
            b1main0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::new("b.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_syntax_parameters() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
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
                scope: Scope::new("ba.b"),
                captures: &[],
                change_context: ContextChange::None,
                pop: 0,
            }),]
        );
    }

    #[test]
    fn compile_branch_repetition() {
        let compiler = Compiler::default();
        let contexts = compile_matches(&compiler,
            "main : ( ~('start'{a} 'end' | 'start'{b} | 'start'{c} 'mid' 'end' ) )* ;",
            vec![],
        );
        assert_eq!(contexts.len(), 8);
        let main = contexts.get("main").unwrap();
        assert_eq!(main.matches, [ContextPattern::Include("include!main@1")]);

        let main_include = contexts.get("include!main@1").unwrap();
        assert_eq!(
            main_include.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("(?=start)"),
                scope: Scope::EMPTY,
                captures: &[],
                change_context: ContextChange::Branch(
                    "main@1",
                    &["main|0|main@1", "main|2|main@1", "main|3|main@1",]
                ),
                pop: 0,
            })]
        );

        let main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(
            main0.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("start"),
                scope: Scope::new("a.test"),
                captures: &[],
                change_context: ContextChange::PushOne("main|1|main@1"),
                pop: 1,
            })]
        );

        let main1 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(
            main1.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("end"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Fail("main@1"),
                    pop: 0,
                }),
            ]
        );

        let main2 = contexts.get("main|2|main@1").unwrap();
        assert_eq!(
            main2.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("start"),
                scope: Scope::new("b.test"),
                captures: &[],
                change_context: ContextChange::None,
                pop: 1,
            })]
        );

        let main3 = contexts.get("main|3|main@1").unwrap();
        assert_eq!(
            main3.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from("start"),
                scope: Scope::new("c.test"),
                captures: &[],
                change_context: ContextChange::PushOne("main|4|main@1"),
                pop: 1,
            })]
        );

        let main4 = contexts.get("main|4|main@1").unwrap();
        assert_eq!(
            main4.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("mid"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Push(&["main|5"]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
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
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_repetition_scopes() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
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
                    scope: Scope::new("a.test ra.test"),
                    captures: &[],
                    change_context: ContextChange::Push(&["main|0"]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::new("a.test b.test rb.test"),
                    captures: &[],
                    change_context: ContextChange::Push(&[
                        "main|0", "a|meta", "b|0",
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }

    #[test]
    fn compile_simple_left_recursion() {
        let compiler = Compiler::default();
        let contexts =
            compile_matches(&compiler, "main : a ; a : a 'a' | 'b' ;", vec![]);
        // Gets rewritten as: main : 'b' main|lr0 ; main|lr0 : 'a' main|lr0 ;
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Push(&["a|0"]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
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
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 0,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("(?=\\S)"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        )
    }

    #[test]
    fn compile_stacked_meta_scope() {
        let compiler = Compiler::default();
        let contexts = compile_matches(
            &compiler,
            "main : s c ; s : 'a' 'b' ; c{c} : 'c' ;",
            vec![],
        );
        // The meta scope on c necessitates an extra "entry" context
        assert_eq!(contexts.len(), 4);
        let main = contexts.get("main").unwrap();
        assert_eq!(
            main.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("a"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::Push(&[
                        "c|0|entry-0",
                        "s|0",
                    ]),
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let s0 = contexts.get("s|0").unwrap();
        assert_eq!(
            s0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("b"),
                    scope: Scope::EMPTY,
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
        let c0_entry = contexts.get("c|0|entry-0").unwrap();
        assert_eq!(
            c0_entry.matches,
            [ContextPattern::Match(Match {
                pattern: Pattern::from(""),
                scope: Scope::EMPTY,
                captures: &[],
                change_context: ContextChange::Set(&["c|0"]),
                pop: 0,
            })]
        );
        let c0 = contexts.get("c|0").unwrap();
        assert_eq!(
            c0.matches,
            [
                ContextPattern::Match(Match {
                    pattern: Pattern::from("c"),
                    scope: Scope::new("c.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
                ContextPattern::Match(Match {
                    pattern: Pattern::from("\\S"),
                    scope: Scope::new("invalid.illegal.test"),
                    captures: &[],
                    change_context: ContextChange::None,
                    pop: 1,
                }),
            ]
        );
    }
}
