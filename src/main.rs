use clap::{App, Arg};
use kotlin::compile::compile;
use std::io::prelude::*;

fn main() {
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
        let file = std::fs::File::open(file_name);
        if let Err(err) = file {
            eprintln!("Could not open file {}: {}", file_name, err);
            std::process::exit(1);
        }
        let mut file = file.unwrap();

        let mut contents = String::new();
        if let Err(err) = file.read_to_string(&mut contents) {
            eprintln!("Could not read contents of file {}: {}", file_name, err);
            std::process::exit(1);
        }

        contents
    } else {
        let mut contents = String::new();
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();
        if let Err(err) = handle.read_to_string(&mut contents) {
            eprintln!("Could not read stdin: {}", err);
            std::process::exit(1);
        }
        contents
    };

    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    if let Err(err) = compile(&contents, &mut handle) {
        err.to_owned(&contents).eprint();
        std::process::exit(1);
    }
}
