use kotlin::compile::compile;
// use kotlin::error::*;
// use kotlin::parse::Type;

#[test]
fn simple_when_expr() {
    let src = String::from("when { true -> 1; false -> 0}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(cond (#t 1) (#f 0))\n"
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
        &"(cond (#t 1) (#f 0) (else 42))\n"
    );
}
