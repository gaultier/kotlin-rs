use kotlin::compile::asm;
use std::path::Path;

#[test]
fn print_hello_world() {
    let src = "println(\"hello, world!\")";
    let path = Path::new("HelloWorld.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "hello, world!");
}

#[test]
fn print_int() {
    let src = "println(10)";
    let path = Path::new("PrintInt.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}
