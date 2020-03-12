use kotlin::compile::compile;

#[test]
fn int_equality() {
    let src = String::from("0xab == 171;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}
