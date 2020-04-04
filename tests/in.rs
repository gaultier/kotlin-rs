use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn in_expr() {
    let src = String::from("val a: Boolean = 1 in 0..10");

    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (in 1 (range 0 10)))"
    );
}

#[test]
fn in_expr_wrong_type_return() -> Result<(), String> {
    let src = String::from("val a: String = 1 in 0..2");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Boolean, Type::TString),
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn in_expr_wrong_type_inside() -> Result<(), String> {
    let src = String::from("1 in true..false");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::BooleanRange, Type::IntRange),
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}
