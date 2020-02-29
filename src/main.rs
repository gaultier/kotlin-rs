use kotlin::lex::*;
use kotlin::parse::*;
use std::io::prelude::*;
use std::result::Result;

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let args_ref = args.iter().map(|s| s.as_ref()).collect::<Vec<&str>>();
    match args_ref.as_slice() {
        [_, "lex"] => {
            let mut contents = String::new();
            let stdin = std::io::stdin();
            let mut handle = stdin.lock();
            handle
                .read_to_string(&mut contents)
                .map_err(|err| format!("Could not read stdin: {}", err))?;

            let mut lexer = Lexer::new(&contents);
            loop {
                let token = lexer.lex();
                match token {
                    Ok(token) if token.kind == TokenKind::Eof => {
                        return Ok(());
                    }
                    Ok(token) | Err(token) => {
                        token.print(&contents);
                        println!("{:?}", token);
                    }
                }
            }
        }
        [_, "parse"] => {
            let mut contents = String::new();
            let stdin = std::io::stdin();
            let mut handle = stdin.lock();
            handle
                .read_to_string(&mut contents)
                .map_err(|err| format!("Could not read stdin: {}", err))?;

            let mut parser = Parser::new(&contents);
            let ast = parser.parse();
            if let Ok(ast) = ast {
                println!("{:?}", ast);
                Ok(())
            } else {
                let err = ast.unwrap_err();
                err.print(&contents);
                Err("".to_string())
            }
        }
        _ => {
            println!("Usage: {} lex|parse", args[0]);
            Ok(())
        }
    }
}
