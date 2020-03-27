use kotlin::compile::compile;
use kotlin::error::*;

#[test]
fn simple_var() {
    let src = String::from("var a = 1;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a 1)"
    );
}

#[test]
fn var_with_math_expr() {
    let src = String::from("var a = 5*10\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (* 5 10))"
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
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a 1)\n (define b (* a 2))\n (if (< b 4) (begin ) (begin )) )"
    )
}

#[test]
fn simple_val() {
    let src = String::from("val a = 1;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a 1)"
    );
}

#[test]
fn val_with_math_expr() {
    let src = String::from("val a = 5*10\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (* 5 10))"
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
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a 1)\n (define b (* a 2))\n (if (< b 4) (begin ) (begin )) )"
    )
}

#[test]
fn simple_var_assign() {
    let src = String::from("var a = 1; a = 4;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a 1)\n (set! a 4)\n )"
    );
}

#[test]
fn var_assign_with_math_expr() {
    let src = String::from("var a = 5*10; a = a * 2;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (* 5 10))\n(set! a (* a 2))"
    );
}

#[test]
fn val_reassign() -> Result<(), String> {
    let src = String::from("val a = 4; a = a*2;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::CannotReassignVal(identifier),
            ..
        }) if identifier == "a" => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn var_assign_other_ops() {
    let src = String::from("var a = 5*10; a -=\n 1; a%=2; a/=3; a*=4 ");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (* 5 10))\n(set! a (-= 1))\n(set! a (%= 2))\n(set! a (/= 3))\n(set! a (*= 4))"
    );
}
