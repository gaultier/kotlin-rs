use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::{JumpKind, Type};
use kotlin::resolver::Context;

#[test]
fn simple_call() {
    let src = String::from("fun a() = 10; a();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(begin (define (a ) 10)\n (apply a (list )) )\n"
    );
}

#[test]
fn assign_to_call_expr() {
    let src = String::from("var a = 1; fun foo() = 99; a = foo();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(begin (define a 1)\n (define (foo ) 99)\n (set! a (apply foo (list )))\n )\n"
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
        &"(begin (define (a ) (begin 1 2 'a' #t 10 ) )\n (define b (apply a (list )))\n )\n"
    );
}

#[test]
fn fn_body_block_no_return_type() -> Result<(), String> {
    let src = String::from("fun a() {1} var b= a(); b+=1;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Unit, Type::Int),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn fn_with_args() {
    let src = String::from("fun foo(a:Int, b:Long) = a * b; foo(1, 2L);");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());

    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define (foo a b ) (* a b))\n (apply foo (list 1 2 )) )"
    );
}

#[test]
fn fn_with_return() {
    let src = String::from("fun foo(a:Int, b:Long): Bool {return if (a < b) true else false }; val a: Bool = foo(1, 2L);");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());

    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define (foo a b ) (if (< a b) #t #f)\n (define a (apply foo (list 1 2 )) )))"
    );
}

#[test]
fn fn_with_wrong_arg_type() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long) = a * b; foo(1, true);");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Bool, Type::Long),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn fn_with_wrong_return_type() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long): String = a * b;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Long, Type::TString),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn fn_with_unknown_identifier_for_return_type() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long): Null = a * b;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::UnknownIdentifier(identifier),
            ..
        }) if identifier == "Null" => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn return_not_in_fn() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long): Int {} while(true) {return}");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind:
                ErrorKind::JumpInInvalidContext {
                    jump_kind: JumpKind::Return,
                    expected_context: Context::Function,
                    found_context: Context::Loop,
                },
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}
