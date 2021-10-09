use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

#[macro_use]
extern crate clap;
extern crate sbnf;

fn main() {
    std::process::exit(match try_main() {
        Ok(_) => 0,
        Err(e) => {
            eprintln!("{}", e);

            1
        }
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
        (@arg output: -o +takes_value "The file to write the compiled sublime-syntax to. \
         Defaults to $INPUT.sublime-syntax if left out. Use a single dash `-` \
         to write to stdout instead.")
        (@arg INPUT: +required "The SBNF file to compile")
        (@arg ARGS: ... "Arguments to pass to the main and prototype rules")
        ).get_matches();

    let input = matches.value_of("INPUT").unwrap();
    let output = matches.value_of("output");
    let args = matches.values_of("ARGS");

    let mut contents = String::new();
    {
        let mut file = fmt_io_err(File::open(&input))?;
        fmt_io_err(file.read_to_string(&mut contents))?;
    }

    let grammar =
        sbnf::sbnf::parse(&contents).map_err(|e| e.fmt(&input, &contents))?;

    // Use the base name of the input as a name hint
    let name_hint = Path::new(&input).file_stem().unwrap().to_str().unwrap();

    let options = sbnf::compiler::CompileOptions {
        name_hint: Some(name_hint),
        debug_contexts: matches.is_present("debug"),
        arguments: args.map(|a| a.collect::<Vec<_>>()).unwrap_or(vec![]),
        entry_points: vec!["main", "prototype"],
    };

    let result = sbnf::compiler::compile(&options, &grammar);

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
        }
        Ok(syntax) => {
            if !matches.is_present("quiet") {
                for warning in result.warnings {
                    eprintln!("{}", warning.fmt("Warning", &input, &contents));
                }
            }

            let mut output_buffer = String::new();
            syntax
                .serialize(&mut output_buffer)
                .map_err(|e| format!("{}", e))?;

            let output_path = match output {
                Some("-") => None,
                Some(path) => Some(PathBuf::from(path)),
                None => {
                    Some(Path::new(&input).with_extension("sublime-syntax"))
                }
            };

            match output_path {
                None => {
                    print!("{}", output_buffer);
                }
                Some(path) => {
                    let mut file = fmt_io_err(File::create(path))?;
                    fmt_io_err(
                        file.write_fmt(format_args!("{}", output_buffer)),
                    )?;
                }
            }

            Ok(())
        }
    }
}
