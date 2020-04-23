use crate::jvm_locals::Locals;
use crate::jvm_stack::Stack;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct State {
    pub(crate) locals: Locals,
    pub(crate) stack: Stack,
}

impl State {
    pub(crate) fn new() -> State {
        State {
            locals: Locals::new(),
            stack: Stack::new(),
        }
    }
}
