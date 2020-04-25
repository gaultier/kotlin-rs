use crate::error::*;
use log::debug;
use std::collections::BTreeMap;
use std::slice::Iter;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Eq, Ord)]
pub(crate) enum Constant {
    Utf8(String),
    ClassInfo(u16),
    MethodRef(u16, u16),
    FieldRef(u16, u16),
    NameAndType(u16, u16),
    CString(u16),
    Int(i32),
    Float([u8; 4]),
    Long(i64),
    LongHigh(i32),
    LongLow(i32),
    Double([u8; 8]),
    DoubleHigh(u32),
    DoubleLow(u32),
}

#[derive(Debug)]
pub(crate) struct Pool {
    values: Vec<Constant>,
    unique_value_to_index: BTreeMap<Constant, u16>,
}

impl Pool {
    pub(crate) fn new() -> Pool {
        Pool {
            values: Vec::new(),
            unique_value_to_index: BTreeMap::new(),
        }
    }

    pub(crate) fn len(&self) -> u16 {
        self.values.len() as u16
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub(crate) fn push(&mut self, constant: Constant) -> Result<u16, Error> {
        // Since `self.values` is one-indexed, the max index is `std::u16::MAX+1` but since we
        // can only index it with u16 the real max index is `std::u16::MAX`

        debug!("pool: adding constant: constant={:?}", &constant);

        if let Some(i) = self.unique_value_to_index.get(&constant) {
            debug!(
                "pool: constant already exists at index i={} constant={:?}",
                i, &constant
            );
            return Ok(*i);
        }

        let i = match constant {
            Constant::Long(n) if (self.values.len() + 1) < std::u16::MAX as usize => {
                self.values.push(Constant::LongHigh((n >> 32) as i32));
                self.values
                    .push(Constant::LongLow((n & 0xff_ff_ff_ff) as i32));
                Ok((self.values.len()) as u16)
            }
            Constant::Double(n) if (self.values.len() + 1) < std::u16::MAX as usize => {
                self.values.push(Constant::DoubleHigh(
                    ((n[0] as u32) << 24)
                        + ((n[1] as u32) << 16)
                        + ((n[2] as u32) << 8)
                        + (n[3] as u32),
                ));
                self.values.push(Constant::DoubleLow(
                    ((n[4] as u32) << 24)
                        + ((n[5] as u32) << 16)
                        + ((n[6] as u32) << 8)
                        + (n[7] as u32),
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
        }?;

        self.unique_value_to_index.insert(constant, i);

        Ok(i)
    }

    pub(crate) fn iter(&self) -> Iter<Constant> {
        self.values.iter()
    }
}

impl IntoIterator for Pool {
    type Item = Constant;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}
