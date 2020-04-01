use crate::parse::*;
// use crate::resolver::Resolution;
// use log::debug;

pub(crate) struct MirTransformer {
    _current_id: usize,
}

impl MirTransformer {
    pub(crate) fn new(_current_id: usize) -> MirTransformer {
        MirTransformer { _current_id }
    }

    fn _next_id(&mut self) -> NodeId {
        self._current_id = self
            ._current_id
            .checked_add(1)
            .expect("Out of ids, input too big");
        self._current_id
    }

    pub(crate) fn statements(&mut self, block: AstNodeStmt) -> AstNodeStmt {
        // self.statement(block)
        block
    }
}
