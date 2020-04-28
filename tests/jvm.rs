use kotlin::compile::jvm;
use std::path::Path;

#[test]
fn float_eq_true() {
    let src = "println(if (2.0f == 2.0f) 10 else -10)";
    let path = Path::new("FloatEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_eq_false() {
    let src = "println(if (2.0f == 3.0f) 10 else -10)";
    let path = Path::new("FloatEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_eq_true() {
    let src = "println(if (2 == 2) 10 else -10)";
    let path = Path::new("IntEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_eq_false() {
    let src = "println(if (2 != 2) 10 else -10)";
    let path = Path::new("IntEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_eq_true() {
    let src = "println(if (2L == 2L) 10 else -10)";
    let path = Path::new("LongEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_eq_false() {
    let src = "println(if (2L == 3L) 10 else -10)";
    let path = Path::new("LongEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_eq_true() {
    let src = "println(if (2.0 == 2.0) 10 else -10)";
    let path = Path::new("DoubleEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_eq_false() {
    let src = "println(if (2.0 == 3.0) 10 else -10)";
    let path = Path::new("DoubleEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_not_eq_true() {
    let src = "println(if (2 != 2) 10 else -10)";
    let path = Path::new("IntNotEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_not_eq_false() {
    let src = "println(if (2 != 3) 10 else -10)";
    let path = Path::new("IntNotEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_not_eq_true() {
    let src = "println(if (2.0f != 2.0f) 10 else -10)";
    let path = Path::new("FloatNotEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn float_not_eq_false() {
    let src = "println(if (2.0f != 3.0f) 10 else -10)";
    let path = Path::new("FloatNotEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_not_eq_true() {
    let src = "println(if (2L != 2L) 10 else -10)";
    let path = Path::new("LongNotEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_not_eq_false() {
    let src = "println(if (2L != 3L) 10 else -10)";
    let path = Path::new("LongNotEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_not_eq_true() {
    let src = "println(if (2.0 != 2.0) 10 else -10)";
    let path = Path::new("DoubleNotEqTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_not_eq_false() {
    let src = "println(if (2.0 != 3.0) 10 else -10)";
    let path = Path::new("DoubleNotEqFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_lt_true() {
    let src = "println(if (2 < 2) 10 else -10); println(if (2 < 1) 10 else -10)";
    let path = Path::new("IntLtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn int_lt_false() {
    let src = "println(if (2 < 3) 10 else -10)";
    let path = Path::new("IntLtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_lt_true() {
    let src = "println(if (2f < 2f) 10 else -10); println(if (2f < 1f) 10 else -10)";
    let path = Path::new("FloatLtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn float_lt_false() {
    let src = "println(if (2f < 3f) 10 else -10)";
    let path = Path::new("FloatLtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_lt_true() {
    let src = "println(if (2.0 < 2.0) 10 else -10); println(if (2.0 < 1.0) 10 else -10)";
    let path = Path::new("DoubleLtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn double_lt_false() {
    let src = "println(if (2.0 < 3.0) 10 else -10)";
    let path = Path::new("DoubleLtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_lt_true() {
    let src = "println(if (2L < 2L) 10 else -10)";
    let path = Path::new("LongLtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_lt_false() {
    let src = "println(if (2L < 3L) 10 else -10)";
    let path = Path::new("LongLtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_gt_true() {
    let src = "println(if (2f > 1f) 10 else -10)";
    let path = Path::new("FloatGtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_gt_false() {
    let src = "println(if (2f > 3f) 10 else -10); println(if (2f > 2f) 10 else -10)";
    let path = Path::new("FloatGtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn double_gt_true() {
    let src = "println(if (2.0 > 1.0) 10 else -10)";
    let path = Path::new("DoubleGtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_gt_false() {
    let src = "println(if (2.0 > 3.0) 10 else -10); println(if (2.0 > 2.0) 10 else -10)";
    let path = Path::new("DoubleGtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn long_gt_true() {
    let src = "println(if (2L > 1L) 10 else -10)";
    let path = Path::new("LongGtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_gt_false() {
    let src = "println(if (2L > 3L) 10 else -10); println(if (2 > 2) 10 else -10)";
    let path = Path::new("LongGtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn int_gt_true() {
    let src = "println(if (2 > 1) 10 else -10)";
    let path = Path::new("IntGtTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_gt_false() {
    let src = "println(if (2 > 3) 10 else -10); println(if (2 > 2) 10 else -10)";
    let path = Path::new("IntGtFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn float_le_true() {
    let src = "println(if (2f <= 2f) 10 else -10);println(if (2f <= 3f) 10 else -10)";
    let path = Path::new("FloatLeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn float_le_false() {
    let src = "println(if (2f <= 1f) 10 else -10)";
    let path = Path::new("FloatLeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_le_true() {
    let src = "println(if (2.0 <= 2.0) 10 else -10);println(if (2.0 <= 3.0) 10 else -10)";
    let path = Path::new("DoubleLeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn double_le_false() {
    let src = "println(if (2f <= 1f) 10 else -10)";
    let path = Path::new("DoubleLeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_le_true() {
    let src = "println(if (2L <= 2L) 10 else -10);println(if (2L <= 3L) 10 else -10)";
    let path = Path::new("LongLeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn long_le_false() {
    let src = "println(if (2L <= 1L) 10 else -10)";
    let path = Path::new("LongLeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_le_true() {
    let src = "println(if (2 <= 2) 10 else -10);println(if (2 <= 3) 10 else -10)";
    let path = Path::new("IntLeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn int_le_false() {
    let src = "println(if (2 <= 1) 10 else -10)";
    let path = Path::new("IntLeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_ge_true() {
    let src = "println(if (2.0 >= 2.0) 10 else -10);println(if (2.0 >= 1.0) 10 else -10)";
    let path = Path::new("DoubleGeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn double_ge_false() {
    let src = "println(if (2.0 >= 3.0) 10 else -10)";
    let path = Path::new("DoubleGeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn float_ge_true() {
    let src = "println(if (2f >= 2f) 10 else -10);println(if (2f >= 1f) 10 else -10)";
    let path = Path::new("FloatGeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn float_ge_false() {
    let src = "println(if (2f >= 3f) 10 else -10)";
    let path = Path::new("FloatGeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_ge_true() {
    let src = "println(if (2L >= 2L) 10 else -10);println(if (2L >= 1L) 10 else -10)";
    let path = Path::new("LongGeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn long_ge_false() {
    let src = "println(if (2f >= 3f) 10 else -10)";
    let path = Path::new("LongGeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_ge_true() {
    let src = "println(if (2 >= 2) 10 else -10);println(if (2 >= 1) 10 else -10)";
    let path = Path::new("IntGeTrue.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn int_ge_false() {
    let src = "println(if (2 >= 3) 10 else -10)";
    let path = Path::new("IntGeFalse.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn print_hello_world() {
    let src = "println(\"hello, world!\")";
    let path = Path::new("HelloWorld.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "hello, world!");
}

#[test]
fn print_int() {
    let src = "println(10)";
    let path = Path::new("PrintInt.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn print_char() {
    let src = "println('ᾯ')";
    let path = Path::new("PrintChar.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "ᾯ");
}

#[test]
fn print_float() {
    let src = "println(10f)";
    let path = Path::new("PrintFloat.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10.0");
}

#[test]
fn print_long() {
    let src = "println(10L)";
    let path = Path::new("PrintLong.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn print_double() {
    let src = "println(10.0)";
    let path = Path::new("PrintDouble.kts");
    let output = jvm(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10.0");
}
