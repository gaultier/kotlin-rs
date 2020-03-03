use kotlin::compile::compile;

#[test]
fn add_int_int() {
    let src = "1 + 2";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_int_long() {
    let src = "1 + 2L";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_long_long() {
    let src = "1L + 2L";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_uint_uint() {
    let src = "1U + 2U";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_uint_ulong() {
    let src = "1U + 2UL";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_ulong_uint() {
    let src = "1UL + 2U";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_ulong_ulong() {
    let src = "1UL + 2UL";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_float_float() {
    let src = "1f + 2f";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2");
}

#[test]
fn add_float_double() {
    let src = "1f + 2e50";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2e50");
}

#[test]
fn add_double_float() {
    let src = "1e50 + 2f";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1e50+2");
}

#[test]
fn add_double_double() {
    let src = "1.5 + 2.3";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1.5+2.3");
}
