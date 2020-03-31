use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::{JumpKind, Type};
use kotlin::resolver::*;

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
fn fn_with_empty_return() {
    let src = String::from(
        "fun foo(a:Int, b:Long){ if (a< b) return else ;}; val x : Unit = foo(1, 2L);",
    );
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());

    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define (foo a b ) (if (< a b) (return )  (begin ))  )\n (define x (apply foo (list 1 2 )))\n )"
    );
}

#[test]
fn fn_with_expr_return_body() {
    let src = String::from("fun foo(a:Int, b:Long): Boolean {return if (a < b) true else false }; val a: Boolean = foo(1, 2L);");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());

    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define (foo a b ) (return (if (< a b) #t  #f ))  )\n (define a (apply foo (list 1 2 )))\n )"
    );
}

#[test]
fn fn_with_expr_return() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long): Boolean = if (a < b) return true else return false; val a: Boolean = foo(1, 2L);");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Nothing, Type::Boolean),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn fn_with_expr_return_without_explicit_type() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long)= if (a < b) return true else return false; val a: Boolean = foo(1, 2L);");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Nothing, Type::Boolean),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn fn_with_wrong_arg_type() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long) = a * b; foo(1, true);");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Boolean, Type::Long),
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
fn fn_with_wrong_return_type_2() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long): String {return a* b;}");
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
fn fn_with_wrong_return_types() -> Result<(), String> {
    let src = String::from("fun foo(a:Int, b:Long): String {return \"a\"; return a* b;}");
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
    let src =
        String::from("fun foo(a:Int, b:Long): Unit { while(true) {break}} while (true) {return}");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind:
                ErrorKind::JumpInInvalidContext {
                    jump_kind: JumpKind::Return,
                    expected_context: LexicalContext(2), // Function
                    found_context: LexicalContext(1),    // Loop
                },
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn fn_with_function_definition_in_loop() {
    let src = String::from(
"var a =1; while (a < 10)  {fun show(x: Int) {println(x); return }; show(a++); if (a==5) break; else ;}"
    );
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());

    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(begin (define a 1)
 (while (< a 10) (begin (define (show x ) (begin (display x)
 (return ) ) )
 (apply show (list (postfix-add1 a) )) (if (== a 5) (break)  (begin )) ))
 )"##
    );
}

#[test]
fn nested_fn() {
    let src = String::from("fun a(): Long {fun b(): Int {return 42} return 99L * b();}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());

    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(define (a ) (begin (define (b ) (return 42)  )
 (return (* 99 (apply b (list )))) ) )"##
    );
}
