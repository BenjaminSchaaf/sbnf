use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use clap::{Parser};

#[derive(Parser)]
#[command(version, about = "SBNF Compiler")]
struct Args {
    /// Do not display warnings
    #[arg(short, long)]
    quiet: bool,

    /// Compile with debug scopes
    #[arg(short = 'g', long = "debug")]
    debug: bool,

    /// The file to write the compiled sublime-syntax to. Defaults to
    /// $INPUT.sublime-syntax if left out. Use a single dash `-` to write to
    /// stdout instead.
    #[arg(short, long)]
    output: Option<PathBuf>,

    // Add a search path for imports.
    #[arg(short = 'I')]
    import_search_paths: Vec<PathBuf>,

    /// The SBNF file to compile. Use a single dash `-` to read from stdin
    /// instead.
    input: PathBuf,

    /// Arguments to pass to the main and prototype rules
    args: Vec<String>,
}

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
    let mut args = Args::parse();

    let mut contents = String::new();
    if args.input == Path::new("-") {
        fmt_io_err(std::io::stdin().read_to_string(&mut contents))?;
    } else {
        // The parent directory of the input file is automatically added to the
        // import path
        if let Some(path) = args.input.parent() {
            args.import_search_paths.push(path.to_path_buf());
        }

        let mut file = fmt_io_err(File::open(&args.input))?;
        fmt_io_err(file.read_to_string(&mut contents))?;
    }

    // Use the base name of the input as a name hint
    let name_hint = Path::new(&args.input).file_stem().unwrap().to_str().unwrap();

    let import_search_paths = args.import_search_paths;

    let options = sbnf::compiler::CompileOptions {
        name_hint: Some(name_hint),
        debug_contexts: args.debug,
        arguments: args.args.iter().map(|s| s.as_ref()).collect(),
        entry_points: vec!["main", "prototype"],
        import_function: Some(Box::new(move |path| {
            let path = Path::new(path);

            for search_path in &import_search_paths {
                let mut buf = search_path.to_path_buf();
                buf.push(path);

                if buf.as_path().extension().is_none() {
                    buf.set_extension("sbnf");
                }

                let mut source = String::new();
                match File::open(buf.clone()).and_then(|mut f| f.read_to_string(&mut source)) {
                    Ok(_) => {
                        return Ok(source);
                    }
                    Err(error) => {
                        // Ignore files that don't exist
                        if error.kind() == std::io::ErrorKind::NotFound {
                            continue;
                        }

                        return Err(format!("Failed to read file '{}': {}", buf.display(), error));
                    }
                }
            }

            return Err(format!("File '{}' not found in any import path", path.display()));
        })),
    };

    let mut compiler = sbnf::compiler::Compiler::new();
    let source = compiler.add_source(None, contents);
    match compiler.compile(&options, source) {
        Err((errors, warnings)) => {
            for error in errors {
                eprintln!(
                    "{}",
                    error.with_compiler(&compiler, "Error")
                );
            }

            if !args.quiet {
                for warning in warnings {
                    eprintln!(
                        "{}",
                        warning.with_compiler(&compiler, "Warning")
                    );
                }
            }

            Err("Compilation Failed".to_string())
        },
        Ok((syntax, warnings)) => {
            if !args.quiet {
                for warning in warnings {
                    eprintln!(
                        "{}",
                        warning.with_compiler(&compiler, "Warning")
                    );
                }
            }

            let mut output_buffer = String::new();
            syntax
                .serialize(&mut output_buffer)
                .map_err(|e| format!("{}", e))?;

            let output_path = match args.output {
                Some(path) => {
                    if path == Path::new("-") {
                        None
                    } else {
                        Some(PathBuf::from(path))
                    }
                }
                None => {
                    Some(Path::new(&args.input).with_extension("sublime-syntax"))
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
