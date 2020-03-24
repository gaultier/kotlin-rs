use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn simple_when_expr() {
    let src = String::from("when { true -> 1; false -> 0}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(cond (true 1) (false 0))\n"
    );
}

#[test]
fn empty_when_body() {
    let src = String::from("when {}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"(cond )\n");
}

#[test]
fn when_with_else() {
    let src = String::from("when {true -> 1\n false -> 0\n\n else -> 42\n}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(cond (true 1) (false 0) :else 42)\n"
    );
}

#[test]
fn when_with_boolean_exprs() {
    let src = String::from("when {true || false -> 1\n false -> 0\n\n else -> 42\n}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(cond ((or true false) 1) (false 0) :else 42)\n"
    );
}

#[test]
fn when_with_subject() {
    let src = String::from("when (5) {1 -> 2; 2->4; 3->6; 4->8; 5->10; else -> 42\n}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(case 5 1 2 2 4 3 6 4 8 5 10 :else 42)\n"
    );
}

#[test]
fn when_with_subject_type_err() -> Result<(), String> {
    let src = String::from("when (5) {1 -> 2; 2->4; 'a'->6; 4->8; 5->10; else -> 42\n}\n");
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Int, Type::Char),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}

#[test]
fn when_with_val_subject() {
    let src = String::from("when (val a = 5) {1 -> 2; 2->4; 3->6; 4->8; 5->10; else -> a * 2\n}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(case (def a 5) 1 2 2 4 3 6 4 8 5 10 :else (* a 2))\n"
    );
}

#[test]
fn when_with_val_subject_type_err() -> Result<(), String> {
    let src = String::from(
        "when (\nval \n\na \n\n =\n\n 5) {1 -> 2; 2->4; 'a'->6; 4->8; 5->10; else -> 42\n}\n",
    );
    let mut out: Vec<u8> = Vec::new();

    match compile(src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Int, Type::Char),
            ..
        }) => Ok(()),
        other => Err(format!("Should be a type error: {:?}", other)),
    }
}
