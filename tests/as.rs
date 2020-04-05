use kotlin::compile::compile;

#[test]
fn as_expr() {
    let src = "val a: Int = 4 as Int";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_mut().unwrap().trim(),
        "(define a (as 4 Int))"
    );
}
