use kotlin::gen_js::gen_js;
use kotlin::lex::*;
use kotlin::parse::*;
use kotlin::type_check::*;
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
                    Ok(token) => {
                        println!("{}", token.to_owned(&contents));
                        println!("{:?}", token);
                    }
                    Err(error) => {
                        eprintln!("{}", error.to_owned(&contents));
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
                type_check(&ast, &contents)?;
                let stdout = std::io::stdout();
                let mut handle = stdout.lock();
                gen_js(&ast, &contents, &mut handle)?;
                Ok(())
            } else {
                let err = ast.unwrap_err();
                let err_s = format!("{}", err.to_owned(&contents));
                eprintln!("{}", err_s);
                Err(err_s)
            }
        }
        _ => {
            println!("Usage: {} lex|parse", args[0]);
            Ok(())
        }
    }
}
