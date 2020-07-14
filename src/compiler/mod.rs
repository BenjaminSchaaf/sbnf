use crate::sublime_syntax;

pub mod analysis;
pub mod codegen;
pub mod interpreter;
pub mod common;

pub use common::{CompileOptions, CompileResult};
use crate::sbnf::{Grammar};

pub fn compile<'a>(options: CompileOptions<'a>, grammar: &'a Grammar<'a>) -> CompileResult<'a, sublime_syntax::Syntax> {
    let analysis_result = analysis::analyze(&options, grammar);

    if let Err(errors) = analysis_result.result {
        return CompileResult::new(Err(errors), analysis_result.warnings);
    }

    let analysis = analysis_result.result.unwrap();
    let mut warnings = analysis_result.warnings;

    let mut interpreter_result = interpreter::interpret(&options, analysis);

    warnings.append(&mut interpreter_result.warnings);

    if let Err(errors) = interpreter_result.result {
        return CompileResult::new(Err(errors), warnings);
    }

    let interpreted = interpreter_result.result.unwrap();

    let mut codegen_result = codegen::codegen(&options, interpreted);

    warnings.append(&mut codegen_result.warnings);

    CompileResult::new(codegen_result.result, warnings)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::compiler::*;
    use crate::sublime_syntax;
    use crate::sbnf;

    fn compile_matches(source: &str, arguments: Vec<&str>) -> HashMap<String, sublime_syntax::Context> {
        let grammar = sbnf::parse(source).unwrap();

        let options = CompileOptions {
            name_hint: Some("test"),
            arguments,
            debug_contexts: false,
            entry_points: vec!("main"),
        };
        let result = compile(options, &grammar);

        if result.is_err() {
            for error in result.result.as_ref().unwrap_err() {
                println!("{}", error.fmt("ERROR", "test", source));
            }
        }
        assert!(result.is_ok());

        if !result.warnings.is_empty() {
            for warning in &result.warnings {
                println!("{}", warning.fmt("WARNING", "test", source));
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
        let contexts = compile_matches("main = ('a'{a} 'b'{b})*;", vec!());
        assert_eq!(contexts.len(), 2);
        let main = contexts.get("main").unwrap();
        assert_eq!(main.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("a"),
                scope: sublime_syntax::Scope::from_str(&["a.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!("main|0".to_string())),
                pop: 0,
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::from_str(&["invalid.illegal.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 0,
            }),
        ]);
        let main0 = contexts.get("main|0").unwrap();
        assert_eq!(main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("b"),
                scope: sublime_syntax::Scope::from_str(&["b.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 1,
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::from_str(&["invalid.illegal.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 1,
            }),
        ]);
    }

    #[test]
    fn compile_simple_branch() {
        let contexts = compile_matches("main = (a | b)*; a{a} = 'c'{ac} 'a'; b{b} = 'c'{bc} 'b';", vec!());
        assert_eq!(contexts.len(), 5);
        let main = contexts.get("main").unwrap();
        assert_eq!(main.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("(?=c)"),
                scope: sublime_syntax::Scope::empty(),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Branch("main@1".to_string(), vec!("a|0|main@1".to_string(), "b|0|main@1".to_string())),
                pop: 0,
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::from_str(&["invalid.illegal.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 0,
            }),
        ]);
        // First branch
        let a0main0 = contexts.get("a|0|main@1").unwrap();
        assert_eq!(a0main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("c"),
                scope: sublime_syntax::Scope::from_str(&["a.test", "ac.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!("main|0|main@1".to_string())),
                pop: 1,
            }),
        ]);
        let a1main0 = contexts.get("main|0|main@1").unwrap();
        assert_eq!(a1main0.matches, [
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
                change_context: sublime_syntax::ContextChange::Fail("main@1".to_string()),
                pop: 0,
            }),
        ]);
        // Second branch
        let b0main0 = contexts.get("b|0|main@1").unwrap();
        assert_eq!(b0main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("c"),
                scope: sublime_syntax::Scope::from_str(&["b.test", "bc.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::Push(vec!("main|1|main@1".to_string())),
                pop: 1,
            }),
        ]);
        let b1main0 = contexts.get("main|1|main@1").unwrap();
        assert_eq!(b1main0.matches, [
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("b"),
                scope: sublime_syntax::Scope::from_str(&["b.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 1,
            }),
            sublime_syntax::ContextPattern::Match(sublime_syntax::Match {
                pattern: sublime_syntax::Pattern::from_str("\\S"),
                scope: sublime_syntax::Scope::from_str(&["invalid.illegal.test"]),
                captures: HashMap::new(),
                change_context: sublime_syntax::ContextChange::None,
                pop: 1,
            }),
        ]);
    }
}
