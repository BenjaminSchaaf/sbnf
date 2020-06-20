use std::fs::File;
use std::path::Path;
use std::io::prelude::*;

pub mod sublime_syntax;
pub mod sbnf;
pub mod compiler;

#[macro_use]
extern crate clap;
extern crate base64;

fn main() {
    std::process::exit(
        match try_main() {
            Ok(_) => 0,
            Err(e) => {
                eprintln!("{}", e);

                1
            },
        });
}

fn fmt_io_err<T>(r: std::io::Result<T>) -> Result<T, String> {
    r.map_err(|e| format!("{}", e))
}

fn try_main() -> Result<(), String> {
    let matches = clap_app!(myapp =>
        (name: "SBNF compiler")
        (version: crate_version!())
        (@arg quiet: -q "Do not display warnings")
        (@arg debug: -g "Compile with debug scopes")
        (@arg INPUT: +required "The SBNF file to compile")
        (@arg OUTPUT: "The file to write the compiled sublime-syntax to. \
         Leaving this out will instead write to stdout")
        ).get_matches();

    let input = matches.value_of("INPUT").unwrap();
    let output = matches.value_of("OUTPUT");

    let mut contents = String::new();
    {
        let mut file = fmt_io_err(File::open(&input))?;
        fmt_io_err(file.read_to_string(&mut contents))?;
    }

    let grammar = sbnf::parse(&contents).map_err(|e| e.fmt(&input, &contents))?;

    // Use the base name of the input as a name hint
    let name_hint = Path::new(&input).file_stem().unwrap().to_str().unwrap();

    let options = compiler::CompileOptions {
        name_hint: Some(name_hint),
        debug_contexts: matches.is_present("debug"),
        arguments: vec!(),
        entry_points: vec!("main", "prototype"),
    };

    let result = compiler::compile(options, &grammar);

    match &result.result {
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error.fmt("Error", &input, &contents));
            }

            if !matches.is_present("quiet") {
                for warning in result.warnings {
                    eprintln!("{}", warning.fmt("Warning", &input, &contents));
                }
            }

            Err("Compilation Failed".to_string())
        },
        Ok(syntax) => {
            if !matches.is_present("quiet") {
                for warning in result.warnings {
                    eprintln!("{}", warning.fmt("Warning", &input, &contents));
                }
            }

            let mut output_buffer = String::new();
            syntax.serialize(&mut output_buffer).map_err(|e| format!("{}", e))?;

            match output {
                Some(output) => {
                    let mut file = fmt_io_err(File::create(output))?;
                    fmt_io_err(file.write_fmt(format_args!("{}", output_buffer)))?;
                },
                None => {
                    print!("{}", output_buffer);
                },
            }

            Ok(())
        },
    }
}
