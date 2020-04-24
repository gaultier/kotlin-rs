use kotlin::compile::{compile, default_path};

#[test]
fn float_eq() {
    let src = "println(if (2.0f == 2.0f) 10 else -10)";
    let output = compile(src, &default_path()).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}
