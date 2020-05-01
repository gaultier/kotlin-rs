use kotlin::compile::asm;
use std::path::Path;

#[test]
fn print_hello_world() {
    let src = "println(\"hello, world!\")";
    let path = Path::new("HelloWorldAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "hello, world!");
}

#[test]
fn print_int() {
    let src = "println(10)";
    let path = Path::new("PrintIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn print_neg_int() {
    let src = "println(-10)";
    let path = Path::new("PrintNegIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn add_int() {
    let src = "println(10+5)";
    let path = Path::new("AddIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "15");
}

#[test]
fn mult_int() {
    let src = "println(10*5)";
    let path = Path::new("MultIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "50");
}

#[test]
fn div_int() {
    let src = "println(11/4)";
    let path = Path::new("DivIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "2");
}

#[test]
fn rem_int() {
    let src = "println(11%4)";
    let path = Path::new("RemIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "3");
}
