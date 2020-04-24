use kotlin::compile::{compile, default_path};
use std::path::Path;

#[test]
fn float_eq() {
    let src = "println(if (2.0f == 2.0f) 10 else -10)";
    let path = Path::new("FloatEq.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");

    let src = "println(if (2.0f == 3.0f) 10 else -10)";
    let output = compile(src, &default_path()).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_eq() {
    let src = "println(if (2 == 2) 10 else -10)";
    let path = Path::new("IntEq.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");

    let src = "println(if (2 == 3) 10 else -10)";
    let output = compile(src, &default_path()).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_eq() {
    let src = "println(if (2L == 2L) 10 else -10)";
    let path = Path::new("LongEq.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");

    let src = "println(if (2L == 3L) 10 else -10)";
    let output = compile(src, &default_path()).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_eq() {
    let src = "println(if (2.0 == 2.0) 10 else -10)";
    let path = Path::new("DoubleEq.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");

    let src = "println(if (2.0 == 3.0) 10 else -10)";
    let output = compile(src, &default_path()).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_not_eq() {
    let src = "println(if (2L != 2L) 10 else -10)";
    let path = Path::new("LongNotEq.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");

    let src = "println(if (2L != 3L) 10 else -10)";
    let output = compile(src, &default_path()).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}
