use clap::{App, Arg};
use kotlin::compile::*;
use kotlin::error::Error;
use kotlin::lex::Lexer;
use kotlin::parse::Parser;
use kotlin::session::Session;
use std::io::prelude::*;

fn main() {
    pretty_env_logger::init();
    let matches = App::new("Kotlin-rs")
        .version("0.1")
        .about("Kotlin compiler")
        .arg(
            Arg::with_name("input_file")
                .value_name("INPUT_FILE")
                .short("f")
                .takes_value(true)
                .help("Input file"),
        )
        .arg(
            Arg::with_name("command")
                .help("What to do")
                .default_value("sexp")
                .possible_values(&[
                    "sexp",
                    "fmt",
                    "dump_ast",
                    "dump_tokens",
                    // Later: build, verify, etc
                ])
                .index(1),
        )
        .get_matches();
    let file_name = matches.value_of("input_file");

    let src = if let Some(file_name) = file_name {
        let file = std::fs::File::open(file_name);
        if let Err(err) = file {
            eprintln!("Could not open file {}: {}", file_name, err);
            std::process::exit(1);
        }
        let mut file = file.unwrap();

        let mut src = String::new();
        if let Err(err) = file.read_to_string(&mut src) {
            eprintln!("Could not read src of file {}: {}", file_name, err);
            std::process::exit(1);
        }

        src
    } else {
        let mut src = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        if let Err(err) = handle.read_to_string(&mut src) {
            eprintln!("Could not read stdin: {}", err);
            std::process::exit(1);
        }
        src
    };

    let cpy = src.clone();
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    let res = match matches.value_of("command").unwrap() {
        "sexp" => compile(src, &mut handle),
        "fmt" => fmt(src, &mut handle),
        "dump_ast" => dump_ast(src),
        "dump_tokens" => dump_tokens(src),
        _ => unreachable!(),
    };
    if let Err(err) = res {
        err.eprint(&cpy);
        std::process::exit(1);
    }
}

fn dump_ast(src: String) -> Result<(), Error> {
    let session = Session::new(&src, None);
    let mut lexer = Lexer::new(&session);
    let (tokens, session) = lexer.lex()?;
    let mut parser = Parser::new(&session, &tokens);
    let stmts = parser.parse()?;
    println!("{:#?}", stmts);
    Ok(())
}

fn dump_tokens(src: String) -> Result<(), Error> {
    let session = Session::new(&src, None);
    let mut lexer = Lexer::new(&session);
    let (tokens, _) = lexer.lex()?;
    println!("{:#?}", tokens);
    Ok(())
}
