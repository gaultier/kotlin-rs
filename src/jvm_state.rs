use crate::jvm_locals::Locals;
use crate::jvm_stack::Stack;

#[derive(Debug, PartialEq, Eq)]
struct State {
    locals: Locals,
    stack: Stack,
}

impl State {
    fn new() -> State {
        State {
            locals: Locals::new(),
            stack: Stack::new(),
        }
    }
}
