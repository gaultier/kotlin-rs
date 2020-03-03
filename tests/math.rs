use kotlin::compile::compile;

#[test]
fn add_int_int() {
    let src = "1 + 2";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(out, b"1+2");
}
