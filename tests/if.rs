use kotlin::compile::sexp;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_if_expr() {
    let src = "if (1<2) 'o' else 'x';";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) 'o' 'x')"
    );
}

#[test]
fn multi_if_expr() {
    let src = "if (1<2) 'o' else 'x'\nif (true) \n\n 42 \n else 99";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (if (< 1 2) 'o' 'x') (if #t 42 99) )"
    );
}

#[test]
fn nested_if_expr() {
    let src = "if (1<2) if (99 < 100) 'a' else 'b' else 'c'\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) (if (< 99 100) 'a' 'b') 'c')"
    );
}

#[test]
fn if_body_block() {
    let src = "if (1<2) {'a'; 1\n\n true; 'b'} else {1*3; 'c'}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) (begin 'a' 1 #t 'b' ) (begin (* 1 3) 'c' ))"
    );
}

#[test]
fn if_with_empty_else_block() {
    let src = "if (1<2) {'a'; 1\n\n true; 'b'} else {}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) (begin 'a' 1 #t 'b' ) (begin ))"
    );
}

#[test]
fn if_with_empty_if_body_block() {
    let src = "if (1<2) {} else {'a'; 1\n\n true; 'b'}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) (begin ) (begin 'a' 1 #t 'b' ))"
    );
}

#[test]
fn if_with_no_if_body_block() {
    let src = "if (1<2) \n else {'a'; 1\n\n true; 'b'}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) (begin ) (begin 'a' 1 #t 'b' ))"
    );
}

#[test]
fn if_with_no_else_block() {
    let src = "if (1==2) {'a'; 1\n\n true; 'b'} else ; \n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (== 1 2) (begin 'a' 1 #t 'b' ) (begin ))"
    );
}

#[test]
fn check_both_branches_types_match() -> Result<(), String> {
    let src = "if (1<2) \n 'a'\n\n else 1 \n";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Char, Type::Int),
            location:
                Location {
                    start_pos: 17,
                    start_line: 4,
                    start_column: 2,
                    end_pos: 21,
                    end_line: 4,
                    end_column: 6,
                },
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn check_both_branches_types_match_unit_when_empty_else() {
    let src = "if (1<2) 'a' else {} \n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) 'a' (begin ))"
    );
}

#[test]
fn check_both_branches_types_match_unit_when_empty_if() {
    let src = "if (1<2) ; else 42 \n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(if (< 1 2) (begin ) 42)"
    );
}

#[test]
fn check_types_coalesce() -> Result<(), String> {
    let src = "if (1<2) 99 else 99f \n";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Int, Type::Float),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn any() {
    let src = "val x: Any = if (1<2) 99 as Any else 42 \n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define x (if (< 1 2) (as 99 Any) 42))"
    );
}

#[test]
fn any_2() {
    let src = "val x: Any = 3; val y: Any = if (1<2) x else 42 \n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(begin (define x 3)\n (define y (if (< 1 2) x 42))\n )"
    );
}
