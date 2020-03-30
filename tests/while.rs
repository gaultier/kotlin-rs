use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

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
            kind: ErrorKind::IncompatibleTypes(Type::Char, Type::Bool),
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
        "(while (< 1 10) (if #t (break)  (continue) ) )"
    );
}
