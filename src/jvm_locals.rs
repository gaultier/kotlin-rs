#[cfg(feature = "jvm_stack_map_frames")]
use crate::jvm_stack_map_frame::VerificationTypeInfo;
use crate::parse::{Id, Type};
use log::debug;
#[cfg(feature = "jvm_stack_map_frames")]
use std::slice::Iter;

type Local = (Id, Type);

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

    pub(crate) fn find_by_id(&self, id: Id) -> Option<(u16, Local)> {
        self.values.iter().enumerate().find_map(|(i, l)| {
            if l.0 == id {
                Some((i as u16, l.clone()))
            } else {
                None
            }
        })
    }

    pub(crate) fn len(&self) -> u16 {
        self.values.len() as u16
    }

    pub(crate) fn at(&self, i: u16) -> &Local {
        assert!(self.values.len() > i as usize);
        &self.values[i as usize]
    }

    pub(crate) fn upsert(&mut self, index: u16, l: Local) -> u16 {
        debug!("upsert: index={} l={:?}", index, l);
        if self.values.len() <= index as usize {
            assert!(index as usize - 1 <= self.values.len());
            self.push(l)
        } else {
            self.values[index as usize].1 = l.1;
            index
        }
    }

    pub(crate) fn push(&mut self, l: Local) -> u16 {
        debug!("push: l={:?}", l);
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

    #[cfg(feature = "jvm_stack_map_frames")]
    pub(crate) fn count_max(&self) -> u16 {
        self.count_max
    }

    pub(crate) fn last(&self) -> Option<&Local> {
        self.values.last()
    }

    #[cfg(feature = "jvm_stack_map_frames")]
    pub(crate) fn to_verification_info(&self) -> Vec<VerificationTypeInfo> {
        self.values
            .iter()
            .map(|(_, t)| t.to_verification_info())
            .collect::<Vec<_>>()
    }

    #[cfg(feature = "jvm_stack_map_frames")]
    pub(crate) fn iter(&self) -> Iter<Local> {
        self.values.iter()
    }
}

impl IntoIterator for Locals {
    type Item = Local;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}
