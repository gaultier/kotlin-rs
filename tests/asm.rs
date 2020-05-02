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
    let src = "println(10 + -5)";
    let path = Path::new("AddIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "5");
}

#[test]
fn mult_int() {
    let src = "println(10 * -5)";
    let path = Path::new("MultIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-50");
}

#[test]
fn sub_int() {
    let src = "println(10 - -5)";
    let path = Path::new("SubIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "15");
}

#[test]
fn div_int() {
    let src = "println(11 / -4)";
    let path = Path::new("DivIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-2");
}

#[test]
fn rem_int() {
    let src = "println(11 % 4)";
    let path = Path::new("RemIntAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "3");
}

#[test]
fn print_long() {
    let src = "println(10L)";
    let path = Path::new("PrintLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn print_neg_long() {
    let src = "println(-10L)";
    let path = Path::new("PrintNegLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn add_long() {
    let src = "println(10L + -5L)";
    let path = Path::new("AddLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "5");
}

#[test]
fn sub_long() {
    let src = "println(10L - -5L)";
    let path = Path::new("SubLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "15");
}

#[test]
fn mult_long() {
    let src = "println(10L * -5L)";
    let path = Path::new("MultLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-50");
}

#[test]
fn div_long() {
    let src = "println(11L / -4L)";
    let path = Path::new("DivLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-2");
}

#[test]
fn rem_long() {
    let src = "println(11L % 4L)";
    let path = Path::new("RemLongAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "3");
}

#[test]
fn int_eq_true() {
    let src = "println((2 + 3) == 5)";
    let path = Path::new("IntEqTrueAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "1");
}

#[test]
fn int_eq_false() {
    let src = "println((3 + 3) == 5)";
    let path = Path::new("IntEqFalseAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "0");
}

#[test]
fn long_eq_true() {
    let src = "println((2L + 3L) == 5L)";
    let path = Path::new("LongEqTrueAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "1");
}

#[test]
fn long_eq_false() {
    let src = "println((3L + 3L) == 5L)";
    let path = Path::new("LongEqFalseAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "0");
}

#[test]
fn int_ne_true() {
    let src = "println((2 + 3) != 6)";
    let path = Path::new("IntNeTrueAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "1");
}

#[test]
fn int_ne_false() {
    let src = "println((2 + 3) != 5)";
    let path = Path::new("IntNeFalseAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "0");
}

#[test]
fn long_ne_true() {
    let src = "println((2L + 3L) != 6L)";
    let path = Path::new("LongNeTrueAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "1");
}

#[test]
fn long_ne_false() {
    let src = "println((2L + 3L) != 5L)";
    let path = Path::new("LongNeFalseAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "0");
}

#[test]
fn int_if_true() {
    let src = "println(if(2 == 2) 10 else -10)";
    let path = Path::new("IntIfTrueAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_if_false() {
    let src = "println(if(2 == 3) 10 else -10)";
    let path = Path::new("IntIfFalseAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_if_true() {
    let src = "println(if(2L == 2L) 10L else -10L)";
    let path = Path::new("LongIfTrueAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_if_false() {
    let src = "println(if(2L == 3L) 10L else -10L)";
    let path = Path::new("LongIfFalseAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_var() {
    let src = "var a = 2; println(a)";
    let path = Path::new("IntVarAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "2");
}

#[test]
fn long_var() {
    let src = "var a = 2L; println(a)";
    let path = Path::new("LongVarAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "2");
}

#[test]
fn while_loop() {
    let src = "var i = 0; while(i != 10){println(i); i = i + 1; }";
    let path = Path::new("WhileLoopAsm.kts");
    let output = asm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(
        String::from_utf8_lossy(&output).trim(),
        "0\n1\n2\n3\n4\n5\n6\n7\n8\n9"
    );
}
