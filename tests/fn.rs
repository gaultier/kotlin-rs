use kotlin::compile::compile;
// use kotlin::error::*;
// use kotlin::parse::Type;

#[test]
fn simple_call() {
    // FIXME
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
    // FIXME
    let src = String::from("var a = 1; fun foo() = 99; a = foo();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a 1)\n(defn foo [] 99)\n(set! a (apply foo '()))\n\n"
    );
}
