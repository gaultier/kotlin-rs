use crate::error::*;
use log::debug;

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

    pub(crate) fn push(&mut self, constant: &Constant) -> Result<u16, Error> {
        // Since `self.values` is one-indexed, the max index is `std::u16::MAX+1` but since we
        // can only index it with u16 the real max index is `std::u16::MAX`

        debug!("adding constant: constant={:?}", &constant);
        match constant {
            Constant::Long(n) if (self.values.len() + 1) < std::u16::MAX as usize => {
                self.values.push(Constant::LongHigh((*n >> 32) as i32));
                self.values
                    .push(Constant::LongLow((*n & 0xff_ff_ff_ff) as i32));
                Ok((self.values.len()) as u16)
            }
            Constant::Double(n) if (self.values.len() + 1) < std::u16::MAX as usize => {
                let bytes = n.to_be_bytes();
                self.values.push(Constant::DoubleHigh(
                    ((bytes[0] as u32) << 24)
                        + ((bytes[1] as u32) << 16)
                        + ((bytes[2] as u32) << 8)
                        + (bytes[3] as u32),
                ));
                self.values.push(Constant::DoubleLow(
                    ((bytes[4] as u32) << 24)
                        + ((bytes[5] as u32) << 16)
                        + ((bytes[6] as u32) << 8)
                        + (bytes[7] as u32),
                ));
                Ok((self.values.len()) as u16)
            }
            _ if self.values.len() < std::u16::MAX as usize => {
                self.values.push(constant.clone());
                Ok(self.values.len() as u16)
            }
            _ => Err(Error::new(
                ErrorKind::MaxConstantsReached(self.values.len() as u16),
                Location::new(),
            )),
        }
    }
}
