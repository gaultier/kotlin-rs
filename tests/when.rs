use kotlin::compile::sexp;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_when_expr() {
    let src = "when { true -> 1; false -> 0}";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(cond (#t 1) (#f 0) )"
    );
}

#[test]
fn empty_when_body() {
    let src = "when {}";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(cond )"
    );
}

#[test]
fn when_with_else() {
    let src = "when {true -> 1\n false -> 0\n\n else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(cond (#t 1) (#f 0)  'else 42)"
    );
}

#[test]
fn when_with_boolean_exprs() {
    let src = "when {true || false -> 1\n false -> 0\n\n else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(cond ((or #t #f) 1) (#f 0)  'else 42)"
    );
}

#[test]
fn when_with_subject() {
    let src = "when (5) {1 -> 2; 2->4; 3->6; 4->8; 5->10; else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(case 5 1 2 2 4 3 6 4 8 5 10  'else 42)"
    );
}

#[test]
fn when_with_subject_type_err() -> Result<(), String> {
    let src = "when (5) {1 -> 2; 2->4; 'a'->6; 4->8; 5->10; else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Char, Type::Int),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn when_with_val_subject() {
    let src = "when (val a = 5) {1 -> 2; 2->4; 3->6; 4->8; 5->10; else -> a * 2\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(case (define a 5)\n 1 2 2 4 3 6 4 8 5 10  'else (* a 2))"
    );
}

#[test]
fn when_with_val_subject_type_err() -> Result<(), String> {
    let src = "when (\nval \n\na \n\n =\n\n 5) {1 -> 2; 2->4; 'a'->6; 4->8; 5->10; else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Char, Type::Int),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn when_with_val_subject_with_type() {
    let src = "when (val a: Int = 5) {1 -> 2; 2->4; 3->6; 4->8; 5->10; else -> a * 2\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(case (define a 5)\n 1 2 2 4 3 6 4 8 5 10  'else (* a 2))"
    );
}

#[test]
fn when_with_val_subject_with_type_type_err() -> Result<(), String> {
    let src = "when (\nval \n\na: Char \n\n =\n\n 5) {1 -> 2; else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    match sexp(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Int, Type::Char),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn when_type() {
    let src = "val a : Int = when {true || false -> 1\n false -> 0\n\n else -> 42\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (cond ((or #t #f) 1) (#f 0)  'else 42))"
    );
}

#[test]
fn when_type_2() {
    let src = "val a : Long = when (1*5) {!in 0..10-> 1L\n 5 -> 0L\n\n else -> 42L\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (case (* 1 5) (not (in (range 0 10))) 1 5 0  'else 42))"
    );
}

#[test]
fn when_type_3() {
    let src = "val a : Long = when {else -> 42L\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (cond  'else 42))"
    );
}

#[test]
fn when_type_4() {
    let src = "val a : Long = when (1*5) {in 0..10-> 1L\n 5 -> 0L\n\n else -> 42L\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (case (* 1 5) (in (range 0 10)) 1 5 0  'else 42))"
    );
}

#[test]
fn is() {
    let src = "val a : Long = when (1*5) {in 0..10-> 1L\n is Int -> 0L\n\n else -> 42L\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (case (* 1 5) (in (range 0 10)) 1 (is Int) 0  'else 42))"
    );
}

#[test]
fn not_is() {
    let src = "val a : Long = when (1*5) {in 0..10-> 1L\n !is Int -> 0L\n\n else -> 42L\n}\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (case (* 1 5) (in (range 0 10)) 1 (not (is Int)) 0  'else 42))"
    );
}

#[test]
fn mutual_fn_in_when() {
    let src = r##"when {true -> {
                    fun odd(n: Int): Boolean = if (n == 1) true else even(n-1)
                    fun even(n: Int): Boolean = if (n == 0) true else odd(n-1)
                    odd(100) 
                    }
                 else -> false
        }"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(sexp(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        r##"(cond (#t (begin (define (odd n ) (if (== n 1) #t (apply even (list (- n 1) ))))
 (define (even n ) (if (== n 0) #t (apply odd (list (- n 1) ))))
 (apply odd (list 100 )) ))  'else #f)"##
    );
}
