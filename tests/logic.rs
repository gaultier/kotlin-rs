use kotlin::compile::sexp;
use kotlin::error::*;

#[test]
fn int_equality() {
    let src = "0xab == 171;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(== 171 171)"
    );
}

#[test]
fn float_equality() {
    let src = ".3f == .2f + 10e-2f;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(== 0.3 (+ 0.2 0.1))"
    );
}

#[test]
fn double_equality() {
    let src = "100.0 == 10e1;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(== 100 100)"
    );
}

#[test]
fn long_equality() {
    let src = "0xabL == 171L;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(== 171 171)"
    );
}

#[test]
fn int_long_equality() -> Result<(), &'static str> {
    let src = "0xab == 171L;";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(..),
            location:
                Location {
                    start_line: 1,
                    start_column: 6,
                    end_line: 1,
                    end_column: 8,
                    ..
                },
        }) => Ok(()),
        _ => Err("Should be a type error"),
    }
}

#[test]
fn float_double_equality() -> Result<(), &'static str> {
    let src = "1f == 1.0;";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(..),
            location:
                Location {
                    start_line: 1,
                    start_column: 4,
                    end_line: 1,
                    end_column: 6,
                    ..
                },
        }) => Ok(()),
        _ => Err("Should be a type error"),
    }
}

#[test]
fn comparison_gte() {
    let src = "0xab >= 171 ;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(>= 171 171)"
    );
}

#[test]
fn comparison_gte_err() -> Result<(), String> {
    let src = "1f <= 3e1 < 33;";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(..),
            location:
                Location {
                    start_line: 1,
                    start_column: 4,
                    end_line: 1,
                    end_column: 6,
                    ..
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}
