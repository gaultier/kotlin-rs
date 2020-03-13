use kotlin::compile::compile;

#[test]
fn int_equality() {
    let src = String::from("0xab == 171;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn uint_equality() {
    let src = String::from("0xabU == 171U;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn uint_long_equality() {
    let src = String::from("0xabUL == 171UL;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn float_equality() {
    let src = String::from(".3f == .2f + 10e-2f;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"0.3==0.2+0.1;\n"
    );
}

#[test]
fn double_equality() {
    let src = String::from("100.0 == 10e1;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"100==100;\n");
}

#[test]
fn long_equality() {
    let src = String::from("0xabL == 171L;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn int_long_equality() {
    let src = String::from("0xab == 171L;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_err());
    // assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn int_uint_long_equality() {
    let src = String::from("0xab == 171UL;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_err());
    // assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn int_uint_equality() {
    let src = String::from("0xab == 171U;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_err());
    // assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}

#[test]
fn float_double_equality() {
    let src = String::from("1f == 1.0;");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_err());
    // assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"171==171;\n");
}
