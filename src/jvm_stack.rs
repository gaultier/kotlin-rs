use crate::error::*;
use crate::jvm_stack_map_frame::VerificationTypeInfo;
use crate::parse::Type;

#[derive(Debug, Clone)]
pub(crate) struct Stack {
    values: Vec<Type>,
    count_max: u16,
}

impl Stack {
    pub(crate) fn new() -> Stack {
        Stack {
            values: Vec::new(),
            count_max: 0,
        }
    }
    pub(crate) fn pop(&mut self) -> Result<Type, Error> {
        Ok(self.values.pop().unwrap())
    }

    pub(crate) fn pop2(&mut self) -> Result<[Type; 2], Error> {
        let a = self.pop()?;
        let b = self.pop()?;
        Ok([a, b])
    }

    pub(crate) fn push(&mut self, t: Type) -> Result<(), Error> {
        if self.values.len() == std::u8::MAX as usize {
            return Err(Error::new(ErrorKind::JvmStackOverflow, Location::new()));
        }

        self.values.push(t);

        self.count_max = std::cmp::max(self.values.len() as u16, self.count_max);
        Ok(())
    }

    pub(crate) fn count_max(&self) -> u16 {
        self.count_max
    }

    pub(crate) fn to_verification_info(&self) -> Vec<VerificationTypeInfo> {
        self.values
            .iter()
            .map(|t| t.to_verification_info())
            .collect::<Vec<_>>()
    }
}
