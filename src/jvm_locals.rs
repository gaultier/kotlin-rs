use crate::jvm_stack_map_frame::VerificationTypeInfo;
use crate::parse::{NodeId, Type};

type Local = (NodeId, Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Locals {
    values: Vec<Local>,
    count_max: u16,
}

impl Locals {
    pub(crate) fn new() -> Locals {
        Locals {
            values: Vec::new(),
            count_max: 0,
        }
    }

    pub(crate) fn find_by_id(&self, id: NodeId) -> Option<(u16, Local)> {
        self.values.iter().enumerate().find_map(|(i, l)| {
            if l.0 == id {
                Some((i as u16, l.clone()))
            } else {
                None
            }
        })
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub(crate) fn at(&self, i: u16) -> &Local {
        assert!(self.values.len() > i as usize);
        &self.values[i as usize]
    }

    pub(crate) fn push(&mut self, l: Local) -> u16 {
        assert!(self.values.len() <= std::u16::MAX as usize);

        if self.values.len() > std::u8::MAX as usize {
            unimplemented!()
        }

        let i = if l.1 == Type::Long || l.1 == Type::Double {
            self.values.push(l.clone());
            self.values.push(l);
            self.values.len() - 2
        } else {
            self.values.push(l);
            self.values.len() - 1
        };

        self.count_max = std::cmp::max(self.values.len() as u16, self.count_max);
        i as u16
    }

    pub(crate) fn count_max(&self) -> u16 {
        self.count_max
    }

    pub(crate) fn to_verification_info(&self) -> Vec<VerificationTypeInfo> {
        self.values
            .iter()
            .map(|(_, t)| t.to_verification_info())
            .collect::<Vec<_>>()
    }
}
