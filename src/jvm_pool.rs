#[derive(Debug, Clone)]
pub(crate) enum Constant {
    Utf8(String),
    ClassInfo(u16),
    MethodRef(u16, u16),
    FieldRef(u16, u16),
    NameAndType(u16, u16),
    CString(u16),
    Int(i32),
    Float(f32),
    Long(i64),
    LongHigh(i32),
    LongLow(i32),
    Double(f64),
    DoubleHigh(u32),
    DoubleLow(u32),
}

#[derive(Debug)]
pub(crate) struct Pool {
    values: Vec<Constant>,
}

impl Pool {
    pub(crate) fn new() -> Pool {
        Pool { values: Vec::new() }
    }
}
