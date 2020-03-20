use kotlin::compile::compile;

#[test]
fn simple_if_expr() {
    let src = String::from("if (1<2) 'o' else 'x';");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) 'o' 'x')\n"
    );
}

#[test]
fn multi_if_expr() {
    let src = String::from("if (1<2) 'o' else 'x'\nif (true) \n\n 42 \n else 99");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) 'o' 'x')\n(if #t 42 99)"
    );
}
