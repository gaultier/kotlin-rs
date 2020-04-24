use kotlin::compile::compile;
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

#[test]
fn double_not_eq_true() {
    let src = "println(if (2.0 != 2.0) 10 else -10)";
    let path = Path::new("DoubleNotEqTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_not_eq_false() {
    let src = "println(if (2.0 != 3.0) 10 else -10)";
    let path = Path::new("DoubleNotEqFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_lt_true() {
    let src = "println(if (2 < 2) 10 else -10)";
    let path = Path::new("IntLtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn int_lt_false() {
    let src = "println(if (2 < 3) 10 else -10)";
    let path = Path::new("IntLtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_lt_true() {
    let src = "println(if (2f < 2f) 10 else -10)";
    let path = Path::new("FloatLtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn float_lt_false() {
    let src = "println(if (2f < 3f) 10 else -10)";
    let path = Path::new("FloatLtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_lt_true() {
    let src = "println(if (2.0 < 2.0) 10 else -10)";
    let path = Path::new("DoubleLtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn double_lt_false() {
    let src = "println(if (2.0 < 3.0) 10 else -10)";
    let path = Path::new("DoubleLtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_lt_true() {
    let src = "println(if (2L < 2L) 10 else -10)";
    let path = Path::new("LongLtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}

#[test]
fn long_lt_false() {
    let src = "println(if (2L < 3L) 10 else -10)";
    let path = Path::new("LongLtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_gt_true() {
    let src = "println(if (2f > 1f) 10 else -10)";
    let path = Path::new("FloatGtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn float_gt_false() {
    let src = "println(if (2f > 3f) 10 else -10); println(if (2f > 2f) 10 else -10)";
    let path = Path::new("FloatGtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn double_gt_true() {
    let src = "println(if (2.0 > 1.0) 10 else -10)";
    let path = Path::new("DoubleGtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn double_gt_false() {
    let src = "println(if (2.0 > 3.0) 10 else -10); println(if (2.0 > 2.0) 10 else -10)";
    let path = Path::new("DoubleGtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn long_gt_true() {
    let src = "println(if (2L > 1L) 10 else -10)";
    let path = Path::new("LongGtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn long_gt_false() {
    let src = "println(if (2L > 3L) 10 else -10); println(if (2 > 2) 10 else -10)";
    let path = Path::new("LongGtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn int_gt_true() {
    let src = "println(if (2 > 1) 10 else -10)";
    let path = Path::new("IntGtTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10");
}

#[test]
fn int_gt_false() {
    let src = "println(if (2 > 3) 10 else -10); println(if (2 > 2) 10 else -10)";
    let path = Path::new("IntGtFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10\n-10");
}

#[test]
fn float_le_true() {
    let src = "println(if (2f <= 2f) 10 else -10);println(if (2f <= 3f) 10 else -10)";
    let path = Path::new("FloatLeTrue.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "10\n10");
}

#[test]
fn float_le_false() {
    let src = "println(if (2f <= 1f) 10 else -10)";
    let path = Path::new("FloatLeFalse.kts");
    let output = compile(src, &path).unwrap().unwrap().stdout;
    assert_eq!(String::from_utf8_lossy(&output).trim(), "-10");
}
