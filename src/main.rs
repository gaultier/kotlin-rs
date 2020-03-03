use clap::{App, Arg};
use kotlin::gen_js::gen_js;
use kotlin::parse::*;
use kotlin::type_check::*;
use std::io::prelude::*;
use std::result::Result;

fn main() -> Result<(), String> {
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
        .get_matches();
    let file_name = matches.value_of("input_file");

    let contents = if let Some(file_name) = file_name {
        let mut file = std::fs::File::open(file_name)
            .map_err(|err| format!("Could not open file {}: {}", file_name, err))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|err| format!("Could not read contents of file {}: {}", file_name, err))?;
        contents
    } else {
        let mut contents = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        handle
            .read_to_string(&mut contents)
            .map_err(|err| format!("Could not read stdin: {}", err))?;
        contents
    };

    let mut parser = Parser::new(&contents);
    let ast = parser.parse();
    if let Ok(ast) = ast {
        println!("{:?}", ast);
        if let Err(err) = type_check(&ast, &contents) {
            eprintln!("{}", err.to_owned(&contents));
            return Err("".to_string());
        }
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
