use kotlin::*;
use std::fs;
use std::io::prelude::*;
use std::result::Result;

fn run() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let args_ref = args.iter().map(|s| s.as_ref()).collect::<Vec<&str>>();
    match args_ref.as_slice() {
        [_, "build", file_name] => {
            let mut file = fs::File::open(file_name)
                .map_err(|err| format!("Could not open {}: {}", file_name, err))?;
            let mut contents = String::new();
            file.read_to_string(&mut contents).map_err(|err| {
                format!(
                    "Could not read te contents of the file {}: {}",
                    file_name, err
                )
            })?;

            let mut lexer = Lexer::new(&contents);
            while let Some(token) = lexer.lex() {
                match token {
                    Ok(token) => {
                        token.print();
                        dbg!(token.kind);
                    }
                    Err(err) => {
                        eprintln!("Error: {}", err);
                    }
                }
            }
            Ok(())
        }
        [_, "build"] => {
            let mut contents = String::new();
            let stdin = std::io::stdin();
            let mut handle = stdin.lock();
            handle
                .read_to_string(&mut contents)
                .map_err(|err| format!("Could not read stdin: {}", err))?;

            let mut lexer = Lexer::new(&contents);
            while let Some(token) = lexer.lex() {
                match token {
                    Ok(token) => {
                        token.print();
                        dbg!(token.kind);
                    }
                    Err(err) => {
                        eprintln!("Error: {}", err);
                    }
                }
            }

            Ok(())
        }
        _ => {
            println!("Usage: {} build", args[0]);
            Ok(())
        }
    }
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {}", err);
        std::process::exit(1);
    }
}
