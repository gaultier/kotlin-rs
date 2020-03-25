use kotlin::compile::compile;
// use kotlin::error::*;
// use kotlin::parse::Type;

#[test]
fn simple_call() {
    // FIXME
    let src = String::from("val a = 1; a();");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a 1)\n(apply a '())\n"
    );
}
