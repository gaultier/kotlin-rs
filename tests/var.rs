use kotlin::compile::sexp;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_var() {
    let src = "var a = 1;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a 1)"
    );
}

#[test]
fn var_with_math_expr() {
    let src = "var a = 5*10\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (* 5 10))"
    );
}

#[test]
fn var_with_no_expr() -> Result<(), String> {
    let src = "var a = while (true) ;";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::ExpectedPrimary,
            ..
        }) => Ok(()),
        other => Err(format!("Should be a parse error: {:?}", other)),
    }
}

#[test]
fn ref_var() {
    let src = "var a = 1; var b = a * 2; if(b<4) {} else {}";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a 1)\n (define b (* a 2))\n (if (< b 4) (begin ) (begin )) )"
    )
}

#[test]
fn simple_val() {
    let src = "val a = 1;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a 1)"
    );
}

#[test]
fn val_with_math_expr() {
    let src = "val a = 5*10\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (* 5 10))"
    );
}

#[test]
fn val_with_no_expr() -> Result<(), String> {
    let src = "val a = while (true) ;";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::ExpectedPrimary,
            ..
        }) => Ok(()),
        other => Err(format!("Should be a parse error: {:?}", other)),
    }
}

#[test]
fn ref_val() {
    let src = "val a = 1; val b = a * 2; if(b<4) {} else {}";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a 1)\n (define b (* a 2))\n (if (< b 4) (begin ) (begin )) )"
    )
}

#[test]
fn simple_var_assign() {
    let src = "var a = 1; a = 4;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a 1)\n (set! a 4)\n )"
    );
}

#[test]
fn var_assign_with_math_expr() {
    let src = "var a = 5*10; a = a * 2;";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a (* 5 10))\n (set! a (* a 2))\n )"
    );
}

#[test]
fn val_reassign() -> Result<(), String> {
    let src = "val a = 4; a = a*2;";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::CannotReassignVal(identifier),
            ..
        }) if identifier == "a" => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn var_assign_other_ops() {
    let src = "var a = 5*10; a -=\n 1; a%=2; a/=3; a*=4 ";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a (* 5 10))\n (set! a (-= 1))\n (set! a (%= 2))\n (set! a (/= 3))\n (set! a (*= 4))\n )"
    );
}

#[test]
fn vars_with_type() {
    let src = "var a:Int = 5*10; val b: Char = 'b';";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a (* 5 10))\n (define b 'b')\n )"
    );
}

#[test]
fn val_wrong_explicit_type() -> Result<(), String> {
    let src = "val a: Int = 4L";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Long, Type::Int),
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn assign_with_paren() {
    let src = "var a = 5*10; (((a))) = 1";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define a (* 5 10))\n (set! a 1)\n )"
    );
}
