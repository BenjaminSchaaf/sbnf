use std::fs::File;
use std::path::Path;
use std::io::prelude::*;

pub mod sublime_syntax;
pub mod sbnf;
pub mod compiler;

fn main() -> std::io::Result<()> {
    let args: Vec<_> = std::env::args().collect();
    let mut output = None;
    if args.len() != 2 {
        if args.len() == 3 {
            output = Some(&args[2]);
        } else {
            panic!();
        }
    }

    let mut contents = String::new();
    {
        let mut file = File::open(&args[1])?;
        file.read_to_string(&mut contents)?;
    }

    let grammar = sbnf::parse(&contents);
    println!("GRAMMAR: {:?}", grammar);

    if let Ok(grammar) = grammar {
        let path = Path::new(&args[1]);

        let syntax = compiler::compile(Some(path.file_stem().unwrap().to_str().unwrap()), &grammar);

        match syntax {
            Err(e) => {
                println!("SYNTAX: {:?}", e);
            },
            Ok(syntax) => {
                let mut buf = String::new();
                syntax.serialize(&mut buf);
                println!("{}", buf);

                if let Some(output) = output {
                    let mut file = File::create(output)?;
                    file.write_fmt(format_args!("{}", buf));
                }
            }
        }
    }

    Ok(())
}
