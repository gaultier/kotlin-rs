use kotlin::compile::compile;
use kotlin::error::*;

#[test]
fn simple_var() {
    let src = String::from("var a = 1;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"(def a 1)\n");
}

#[test]
fn var_with_math_expr() {
    let src = String::from("var a = 5*10\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a (* 5 10))\n"
    );
}

#[test]
fn var_with_no_expr() -> Result<(), String> {
    let src = String::from("var a = while (true) ;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::ExpectedPrimary,
            ..
        }) => Ok(()),
        other => Err(format!("Should be a parse error: {:?}", other)),
    }
}

#[test]
fn ref_var() {
    let src = String::from("var a = 1; var b = a * 2; if(b<4) {} else {}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a 1)(def b (* a 2))(if (< b 4) (do ) (do ))\n"
    )
}

#[test]
fn simple_val() {
    let src = String::from("val a = 1;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"(def a 1)\n");
}

#[test]
fn val_with_math_expr() {
    let src = String::from("val a = 5*10\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a (* 5 10))\n"
    );
}

#[test]
fn val_with_no_expr() -> Result<(), String> {
    let src = String::from("val a = while (true) ;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::ExpectedPrimary,
            ..
        }) => Ok(()),
        other => Err(format!("Should be a parse error: {:?}", other)),
    }
}

#[test]
fn ref_val() {
    let src = String::from("val a = 1; val b = a * 2; if(b<4) {} else {}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a 1)(def b (* a 2))(if (< b 4) (do ) (do ))\n"
    )
}

#[test]
fn simple_var_assign() {
    let src = String::from("var a = 1; a = 4;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a 1)(set! a 4)\n\n"
    );
}

#[test]
fn var_assign_with_math_expr() {
    let src = String::from("var a = 5*10; a = a * 2;\n\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a (* 5 10))(set! a (* a 2))\n\n"
    );
}
