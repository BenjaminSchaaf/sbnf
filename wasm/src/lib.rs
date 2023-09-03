#![deny(clippy::all)]

use std::fmt::Write;

extern crate sbnf;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct CompileResult {
    syntax: String,
    messages: String,
}

#[wasm_bindgen]
impl CompileResult {
    pub fn syntax(&self) -> String {
        self.syntax.clone()
    }

    pub fn messages(&self) -> String {
        self.messages.clone()
    }
}

#[wasm_bindgen]
pub fn compile(
    name: &str,
    source: &str,
    args: &str,
    quiet: bool,
    debug: bool,
) -> CompileResult {
    let input = "web";

    let grammar_result = sbnf::sbnf::parse(source);

    if let Err(error) = grammar_result {
        return CompileResult {
            syntax: "".to_string(),
            messages: format!("{}", error.with_source(input, source)),
        };
    }

    let grammar = grammar_result.unwrap();

    let arguments = if args.is_empty() {
        vec![]
    } else {
        args.split(' ').collect::<Vec<_>>()
    };

    let options = sbnf::compiler::CompileOptions {
        name_hint: Some(name),
        debug_contexts: debug,
        arguments,
        entry_points: vec!["main", "prototype"],
    };

    let mut compiler = sbnf::compiler::Compiler::default();

    let result = compiler.compile(&options, &grammar);

    let mut messages = String::new();

    let result_syntax = match &result.result {
        Err(errors) => {
            for error in errors {
                writeln!(
                    messages,
                    "{}",
                    error.with_compiler_and_source(
                        &compiler, "Error", input, source
                    )
                )
                .unwrap();
            }

            "".to_string()
        }
        Ok(syntax) => {
            let mut output_buffer = String::new();
            syntax.serialize(&mut output_buffer).unwrap_or_else(|e| {
                messages.write_fmt(format_args!("{}", e)).unwrap();
            });

            output_buffer
        }
    };

    if !quiet {
        for warning in result.warnings {
            writeln!(
                messages,
                "{}",
                warning.with_compiler_and_source(
                    &compiler, "Warning", input, source
                )
            )
            .unwrap();
        }
    }

    CompileResult { syntax: result_syntax, messages }
}
