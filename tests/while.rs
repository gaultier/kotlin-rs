use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::{JumpKind, Type};
use kotlin::resolver::LexicalContext;

#[test]
fn while_with_body() {
    let src = String::from("while (1 < 10) { 'c' }");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(while (< 1 10) 'c' )"
    );
}

#[test]
fn while_with_empty_body() {
    let src = String::from("while (1 < 10) {}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(while (< 1 10) (begin ))"
    );
}

#[test]
fn while_without_body() {
    let src = String::from("while (1 < 10) ;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(while (< 1 10) (begin ))"
    );
}

#[test]
fn while_bad_cond_type() -> Result<(), String> {
    let src = String::from("while\n ('a') ;");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Char, Type::Boolean),
            location:
                Location {
                    start_pos: 7,
                    start_line: 2,
                    start_column: 2,
                    end_pos: 8,
                    end_line: 2,
                    end_column: 3,
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn while_bad_body_type() -> Result<(), String> {
    let src = String::from("while\n (true) {1+'c'}");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Int, Type::Char),
            location:
                Location {
                    start_pos: 16,
                    start_line: 2,
                    start_column: 11,
                    end_pos: 17,
                    end_line: 2,
                    end_column: 12,
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn while_jumps() {
    let src = String::from("while (1 < 10) {if (true) break else continue}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(while (< 1 10) (if #t (break) (continue)) )"
    );
}

#[test]
fn break_not_in_loop() -> Result<(), String> {
    let src = String::from("if (true) break else 1");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind:
                ErrorKind::JumpInInvalidContext {
                    jump_kind: JumpKind::Break,
                    expected_context: LexicalContext(1), // Loop
                    found_context: LexicalContext(0),    // Top level
                },
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn continue_not_in_loop() -> Result<(), String> {
    let src = String::from("if (true) 2 else continue");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind:
                ErrorKind::JumpInInvalidContext {
                    jump_kind: JumpKind::Continue,
                    expected_context: LexicalContext(1), // Loop
                    found_context: LexicalContext(0),    // Top level
                },
            ..
        }) => Ok(()),
        other => Err(format!("Should be an error: {:?}", other)),
    }
}

#[test]
fn do_while_use_var_in_cond_from_body() {
    let src = String::from("do {val y = 5;} while(y <5)");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(do-while (define y 5)\n  (if (< y 5)))"
    );
}

#[test]
fn do_while_empty_body() {
    let src = String::from("do while(true)");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(do-while (begin ) (if #t))"
    );
}
