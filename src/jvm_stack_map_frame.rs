use crate::parse::Type;
use log::debug;

#[derive(Debug, Copy, Clone)]
pub(crate) enum VerificationTypeInfo {
    Int,
    Object(u16),
}

#[derive(Debug, Clone)]
pub(crate) enum StackMapFrame {
    Same {
        offset: u8,
    },
    SameLocalsOneStackItem {
        offset: u8,
        stack: VerificationTypeInfo,
    },
    Full {
        offset: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    }, // More to come
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum JumpTarget {
    If {
        if_location: u16,
        if_target: u16,
        else_location: u16,
        else_target: u16,
    },
    Goto {
        location: u16,
        target: u16,
    },
}
impl StackMapFrame {
    pub(crate) fn offset(&mut self, offset_to_set: u16) {
        match self {
            StackMapFrame::Same { offset, .. } => *offset = offset_to_set as u8,
            StackMapFrame::SameLocalsOneStackItem { offset, .. } => *offset = offset_to_set as u8,
            StackMapFrame::Full { offset, .. } => *offset = offset_to_set,
        }
    }
}

impl Type {
    pub(crate) fn to_verification_info(&self) -> VerificationTypeInfo {
        match self {
            Type::Boolean | Type::Int | Type::Char => VerificationTypeInfo::Int,
            Type::Long => todo!(),
            Type::Float => todo!(),
            Type::Double => todo!(),
            Type::TString => todo!(),
            Type::Object {
                jvm_constant_pool_index,
                ..
            } => {
                debug!(
                    "to_verification_info: kind=object jvm_constant_pool_index={:?}",
                    jvm_constant_pool_index
                );

                VerificationTypeInfo::Object(jvm_constant_pool_index.unwrap())
            }
            _ => {
                dbg!(self);
                unreachable!()
            }
        }
    }
}
