use kotlin::compile::sexp;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn add_int_long() {
    let src = "1 + 2L;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 1 2)"
    );
}

#[test]
fn add_float_float() {
    let src = "1f + 2f;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 1 2)"
    );
}

#[test]
fn add_float_double() {
    let src = "1f + 2e2;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 1 200)"
    );
}

#[test]
fn add_double_float() {
    let src = "1e2 + 2f;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 100 2)"
    );
}

#[test]
fn add_double_double() {
    let src = "1.5 + 2.3;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 1.5 2.3)"
    );
}

#[test]
fn add_int_double() {
    let src = "1 + 2.3;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 1 2.3)"
    );
}

#[test]
fn add_double_int() {
    let src = "1.5 + 2;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 1.5 2)"
    );
}

#[test]
fn complex_math() {
    let src = "-1.5 / 2 + 5 * 3;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ (/ (- 1.5) 2) (* 5 3))"
    );
}

#[test]
fn add_string_string() {
    let src = r##""abc" + "def";"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" "def")"##
    );
}

#[test]
fn add_string_int() {
    let src = r##""abc" + 2;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" 2)"##
    );
}

#[test]
fn add_int_string() {
    let src = r##"2 + "abc";"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ 2 "abc")"##
    );
}

#[test]
fn add_string_long() {
    let src = r##""abc" + 2L;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" 2)"##
    );
}

#[test]
fn add_long_string() {
    let src = r##"2L + "abc";"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ 2 "abc")"##
    );
}

#[test]
fn add_string_float() {
    let src = r##""abc" + 2f;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" 2)"##
    );
}

#[test]
fn add_string_double() {
    let src = r##""abc" + 2.0;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" 2)"##
    );
}

#[test]
fn add_string_bool() {
    let src = r##""abc" + true;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" #t)"##
    );
}

#[test]
fn add_bool_string() -> Result<(), String> {
    let src = r##"true + "abc";"##;
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Boolean, Type::TString),
            location:
                Location {
                    start_line: 1,
                    start_column: 6,
                    end_line: 1,
                    end_column: 7,
                    ..
                },
        }) => Ok(()),
        _ => Err("Should fail type checking".to_string()),
    }
}

#[test]
fn add_string_null() {
    let src = r##""abc" + null;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ "abc" 'nil)"##
    );
}

#[test]
fn add_null_string() {
    let src = r##"null + "abc";"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(+ 'nil "abc")"##
    );
}

#[test]
fn add_null_null() {
    let src = r##"null + null;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(+ 'nil 'nil)"
    );
}

#[test]
fn plus_plus() {
    let src = r##"++1;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(add1 1)"
    );
}

#[test]
fn minus_minus() {
    let src = r##"--1;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(sub1 1)"
    );
}

#[test]
fn multi_prefix_operators() {
    let src = r##"- + -- ++ 1;"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(- (+ (sub1 (add1 1))))"
    );
}
#[test]
fn plus_plus_not_a_number() -> Result<(), String> {
    let src = r##"++true;"##;
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Boolean, _),
            location:
                Location {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 3,
                    ..
                },
        }) => Ok(()),
        _ => Err("Should fail type checking".to_string()),
    }
}

#[test]
fn minus_minus_not_a_number() -> Result<(), String> {
    let src = r##"--true;"##;
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Boolean, _),
            location:
                Location {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 3,
                    ..
                },
        }) => Ok(()),
        _ => Err("Should fail type checking".to_string()),
    }
}
