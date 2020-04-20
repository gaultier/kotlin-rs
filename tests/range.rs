use kotlin::compile::sexp;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_range() {
    let src = "1..5";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(range 1 5)"
    );
}

#[test]
fn check_both_types_match() -> Result<(), String> {
    let src = "1U..\n\n5UL";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::ULong, Type::UInt),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn reject_invalid_range_type() -> Result<(), String> {
    let src = "null..null";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::InvalidRange(Type::Null),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}
