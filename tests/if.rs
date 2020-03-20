use kotlin::compile::compile;

#[test]
fn simple_if_expr() {
    let src = String::from("if (1<2) 'o' else 'x';");
    let mut out: Vec<u8> = Vec::new();

    let res = compile(src, &mut out);
    dbg!(&res);
    assert!(res.is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171===171;\n");
}
