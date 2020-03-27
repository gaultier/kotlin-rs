use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_range() {
    let src = String::from("1..5");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(range 1 5)"
    );
}

#[test]
fn check_both_types_match() -> Result<(), String> {
    let src = String::from("1U..\n\n5UL");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::UInt, Type::ULong),
            location:
                Location {
                    start_pos: 2,
                    start_line: 1,
                    start_column: 3,
                    end_pos: 4,
                    end_line: 1,
                    end_column: 5,
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn reject_invalid_range_type() -> Result<(), String> {
    let src = String::from("null..null");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::InvalidRange(Type::Null),
            location:
                Location {
                    start_pos: 4,
                    start_line: 1,
                    start_column: 5,
                    end_pos: 6,
                    end_line: 1,
                    end_column: 7,
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}
