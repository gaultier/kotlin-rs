use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_call() {
    let src = String::from("fun a() = 10; a();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(defn a [] 10)\n(apply a '())\n"
    );
}

#[test]
fn assign_to_call_expr() {
    let src = String::from("var a = 1; fun foo() = 99; a = foo();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a 1)\n(defn foo [] 99)\n(set! a (apply foo '()))\n\n"
    );
}

#[test]
fn call_expr_wrong_type() -> Result<(), String> {
    let src = String::from("var a = \"hello\"; fun foo() = 99; a = foo();");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::TString, Type::Int),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn not_a_callable() -> Result<(), String> {
    let src = String::from("var a = \"hello\"; a();");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::NotACallable(Type::TString),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn fn_body_block() {
    let src = String::from("fun a() {1; 2; 'a'; true; 10;} var b = a();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(defn a [] (do 1 2 'a' true 10 ))\n(def b (apply a '()))\n\n"
    );
}
