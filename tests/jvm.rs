use kotlin::compile::{compile, default_path};
use std::path::Path;

#[test]
fn float_eq_true() {
    let src = "println(if (2.0f == 2.0f) 10 else -10)";
    let path = Path::new("FloatEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_eq_false() {
    let src = "println(if (2.0f == 3.0f) 10 else -10)";
    let path = Path::new("FloatEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_eq_true() {
    let src = "println(if (2 == 2) 10 else -10)";
    let path = Path::new("IntEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_eq_false() {
    let src = "println(if (2 != 2) 10 else -10)";
    let path = Path::new("IntEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_eq_true() {
    let src = "println(if (2L == 2L) 10 else -10)";
    let path = Path::new("LongEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_eq_false() {
    let src = "println(if (2L == 3L) 10 else -10)";
    let path = Path::new("LongEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_eq_true() {
    let src = "println(if (2.0 == 2.0) 10 else -10)";
    let path = Path::new("DoubleEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_eq_false() {
    let src = "println(if (2.0 == 3.0) 10 else -10)";
    let path = Path::new("DoubleEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_not_eq_true() {
    let src = "println(if (2 != 2) 10 else -10)";
    let path = Path::new("IntNotEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_not_eq_false() {
    let src = "println(if (2 != 3) 10 else -10)";
    let path = Path::new("IntNotEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_not_eq_true() {
    let src = "println(if (2.0f != 2.0f) 10 else -10)";
    let path = Path::new("FloatNotEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn float_not_eq_false() {
    let src = "println(if (2.0f != 3.0f) 10 else -10)";
    let path = Path::new("FloatNotEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_not_eq_true() {
    let src = "println(if (2L != 2L) 10 else -10)";
    let path = Path::new("LongNotEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_not_eq_false() {
    let src = "println(if (2L != 3L) 10 else -10)";
    let path = Path::new("LongNotEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}
