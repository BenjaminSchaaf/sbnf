use std::fmt::{Write};

extern crate wasm_bindgen;
extern crate sbnf;

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
pub fn compile(name: &str, source: &str, args: &str, quiet: bool, debug: bool) -> CompileResult {
    let input = "web";

    let grammar_result = sbnf::sbnf::parse(&source);

    if grammar_result.is_err() {
        return CompileResult {
            syntax: "".to_string(),
            messages: grammar_result.unwrap_err().fmt(&input, &source),
        }
    }

    let grammar = grammar_result.unwrap();

    let arguments =
        if args.is_empty() {
            vec!()
        } else {
            args.split(" ").collect::<Vec<_>>()
        };

    let options = sbnf::compiler::CompileOptions {
        name_hint: Some(name),
        debug_contexts: debug,
        arguments,
        entry_points: vec!("main", "prototype"),
    };

    let result = sbnf::compiler::compile(options, &grammar);

    let mut messages = String::new();
    let result_syntax: String;

    match &result.result {
        Err(errors) => {
            for error in errors {
                messages.write_str(&error.fmt("Error", &input, &source)).unwrap();
                messages.write_char('\n').unwrap();
            }

            result_syntax = "Compilation Failed".to_string();
        },
        Ok(syntax) => {
            let mut output_buffer = String::new();
            syntax.serialize(&mut output_buffer).unwrap_or_else(|e| {
                messages.write_fmt(format_args!("{}", e)).unwrap();
            });

            result_syntax = output_buffer;
        },
    }

    if !quiet {
        for warning in result.warnings {
            messages.write_str(&warning.fmt("Warning", &input, &source)).unwrap();
            messages.write_char('\n').unwrap();
        }
    }

    CompileResult {
        syntax: result_syntax,
        messages,
    }
}
