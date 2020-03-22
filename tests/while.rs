use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

#[test]
fn while_with_body() {
    let src = String::from("while (1 < 10) { 'c' }");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(while (< 1 10) 'c')\n\n"
    );
}

#[test]
fn while_with_empty_body() {
    let src = String::from("while (1 < 10) {}");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(while (< 1 10) (do ))\n\n"
    );
}

#[test]
fn while_without_body() {
    let src = String::from("while (1 < 10) ;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(while (< 1 10) (do ))\n\n"
    );
}
