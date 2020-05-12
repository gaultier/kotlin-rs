use kotlin::compile::jvm;
use kotlin::error::*;
use kotlin::parse::Type;
use std::path::Path;

#[test]
fn check_both_branches_types_match() -> Result<(), String> {
    let src = "if (1<2) \n 'a'\n\n else 1 \n";

    match jvm(src, &Path::new("")) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Char, Type::Int),
            location:
                Location {
                    start_pos: 17,
                    start_line: 4,
                    start_column: 2,
                    end_pos: 21,
                    end_line: 4,
                    end_column: 6,
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn check_types_coalesce() -> Result<(), String> {
    let src = "if (1<2) 99 else 99f \n";

    match jvm(src, &Path::new("")) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Int, Type::Float),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}
