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
    pub(crate) fn pop(&mut self) -> Type {
        self.values.pop().unwrap()
    }

    pub(crate) fn pop2(&mut self) -> [Type; 2] {
        let a = self.pop();
        let b = self.pop();
        [a, b]
    }

    pub(crate) fn push(&mut self, t: Type) {
        assert!(self.values.len() <= std::u16::MAX as usize);

        if self.values.len() > std::u8::MAX as usize {
            unimplemented!()
        }

        self.values.push(t);

        self.count_max = std::cmp::max(self.values.len() as u16, self.count_max);
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
