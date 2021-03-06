use kotlin::compile::sexp;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn in_expr() {
    let src = "val a: Boolean = 1 in 0..10";

    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (in 1 (range 0 10)))"
    );
}

#[test]
fn in_expr_wrong_type_return() -> Result<(), String> {
    let src = "val a: String = 1 in 0..2";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Boolean, Type::TString),
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn in_expr_wrong_type_inside() -> Result<(), String> {
    let src = "1 in true..false";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::BooleanRange, Type::IntRange),
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn in_is_complex_expr() {
    let src = "val a: Boolean = 1 is Int in false..true is Long is Boolean";

    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (is (is (in (is 1 Int) (range #f #t)) Long) Boolean))"
    );
}
