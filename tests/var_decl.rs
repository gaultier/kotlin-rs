use kotlin::compile::compile;
use kotlin::error::*;

#[test]
fn simple_var() {
    let src = "var a = 1;";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(std::str::from_utf8(&out).as_ref().unwrap(), &"(def a 1)\n");
}

#[test]
fn var_with_math_expr() {
    let src = "var a = 5*10\n";
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(&src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(def a (* 5 10))\n"
    );
}

#[test]
fn var_with_no_expr() -> Result<(), String> {
    let src = "var a = while (true) ;";
    let mut out: Vec<u8> = Vec::new();

    match compile(&src, &mut out) {
        Err(Error {
            kind: ErrorKind::ExpectedPrimary,
            ..
        }) => Ok(()),
        other => Err(format!("Should be a parse error: {:?}", other)),
    }
}
