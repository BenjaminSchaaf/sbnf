#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate sbnf;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        if let Ok(grammar) = sbnf::sbnf::parse(s) {
            let options = sbnf::compiler::CompileOptions {
                name_hint: Some("test"),
                arguments: vec![],
                debug_contexts: false,
                entry_points: vec!["main"],
            };
            let mut compiler = sbnf::compiler::Compiler::new();
            let _ = compiler.compile(&options, &grammar);
        }
    }
});
