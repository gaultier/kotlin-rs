use kotlin::compile::compile;
use kotlin::error::*;
use kotlin::parse::Type;

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
    let src = "1f + 2e2";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+200");
}

#[test]
fn add_double_float() {
    let src = "1e2 + 2f";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"100+2");
}

#[test]
fn add_double_double() {
    let src = "1.5 + 2.3";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1.5+2.3");
}

#[test]
fn add_int_double() {
    let src = "1 + 2.3";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1+2.3");
}

#[test]
fn add_double_int() {
    let src = "1.5 + 2";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"1.5+2");
}

#[test]
fn complex_math() {
    let src = "-1.5 / 2 + 5 * 3";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"-1.5/2+5*3");
}

#[test]
fn add_string_string() {
    let src = r##""abc" + "def""##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &r##""abc"+"def""##
    );
}

#[test]
fn add_string_int() {
    let src = r##""abc" + 2"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##""abc"+2"##);
}

#[test]
fn add_int_string() {
    let src = r##"2 + "abc""##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##"2+"abc""##);
}

#[test]
fn add_string_long() {
    let src = r##""abc" + 2L"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##""abc"+2"##);
}

#[test]
fn add_long_string() {
    let src = r##"2L + "abc""##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##"2+"abc""##);
}

#[test]
fn add_string_uint() {
    let src = r##""abc" + 2U"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##""abc"+2"##);
}

#[test]
fn add_uint_string() {
    let src = r##"2U + "abc""##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##"2+"abc""##);
}

#[test]
fn add_string_ulong() {
    let src = r##""abc" + 2UL"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##""abc"+2"##);
}

#[test]
fn add_ulong_string() {
    let src = r##"2UL + "abc""##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##"2+"abc""##);
}

#[test]
fn add_string_float() {
    let src = r##""abc" + 2f"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##""abc"+2"##);
}

#[test]
fn add_string_double() {
    let src = r##""abc" + 2.0"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &r##""abc"+2"##);
}

#[test]
fn add_string_bool() {
    let src = r##""abc" + true"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &r##""abc"+true"##
    );
}

#[test]
fn add_bool_string() -> Result<(), String> {
    let src = r##"true + "abc""##;
    let mut out: Vec<u8> = Vec::new();

    match compile(&src, &mut out) {
        Err(Error {
            kind: ErrorKind::IncompatibleTypes(Type::Bool, Type::TString),
            location:
                Location {
                    start_line: 1,
                    start_column: 6,
                    end_line: 1,
                    end_column: 7,
                    ..
                },
        }) => Ok(()),
        _ => Err("Should fail type checking".to_string()),
    }
}

#[test]
fn add_string_null() {
    let src = r##""abc" + null"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &r##""abc"+null"##
    );
}

#[test]
fn add_null_string() {
    let src = r##"null + "abc""##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &r##"null+"abc""##
    );
}

#[test]
fn add_null_null() {
    let src = r##"null + null"##;
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &r##"""+null+null"##
    );
}
