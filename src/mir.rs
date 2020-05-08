use crate::parse::*;
use crate::session::Span;
// use log::debug;

pub(crate) struct MirTransformer {
    current_id: usize,
}

impl MirTransformer {
    pub(crate) fn new(current_id: usize) -> MirTransformer {
        MirTransformer { current_id }
    }

    fn next_id(&mut self) -> NodeId {
        self.current_id = self
            .current_id
            .checked_add(1)
            .expect("Out of ids, input too big");
        self.current_id
    }

    fn fn_def(&mut self, fn_body: AstNodeStmt) -> AstNodeStmt {
        match fn_body {
            // Transform expression form into standard block form for simplicity later
            AstNodeStmt::Expr(expr) => AstNodeStmt::Block {
                body: vec![AstNodeStmt::Expr(AstNodeExpr::Jump {
                    kind: JumpKind::Return,
                    span: Span::new(0, 0),
                    expr: Some(Box::new(expr)),
                    id: self.next_id(),
                })],
                id: self.next_id(),
            },
            // Add final return in fn body if none to avoid falling through
            AstNodeStmt::Block { mut body, id } => {
                match body.last() {
                    Some(AstNodeStmt::Expr(AstNodeExpr::Jump {
                        kind: JumpKind::Return,
                        ..
                    })) => (), // No-op
                    _ => {
                        body.push(AstNodeStmt::Expr(AstNodeExpr::Jump {
                            span: Span::new(0, 0),
                            kind: JumpKind::Return,
                            expr: None,
                            id: self.next_id(),
                        }));
                    }
                }
                AstNodeStmt::Block { body, id }
            }
            _ => unreachable!(),
        }
    }

    fn statement(&mut self, statement: AstNodeStmt) -> AstNodeStmt {
        match statement {
            AstNodeStmt::Block { body, id } => {
                let body = body
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                AstNodeStmt::Block { body, id }
            }
            AstNodeStmt::FnDefinition {
                fn_name,
                body,
                id,
                args,
                flags,
                return_t_span,
            } => AstNodeStmt::FnDefinition {
                fn_name,
                body: Box::new(self.fn_def(*body)),
                args,
                id,
                flags,
                return_t_span,
            },
            _ => statement,
        }
    }

    pub(crate) fn statements(&mut self, block: AstNodeStmt) -> AstNodeStmt {
        self.statement(block)
    }
}
