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

    fn next_id(&mut self) -> Id {
        self.current_id = self
            .current_id
            .checked_add(1)
            .expect("Out of ids, input too big");
        self.current_id
    }

    fn fn_def(&mut self, fn_body: AstStmt) -> AstStmt {
        match fn_body {
            // Transform expression form into standard block form for simplicity later
            AstStmt::Expr(expr) => AstStmt::Block {
                body: vec![AstStmt::Expr(AstExpr::Jump {
                    kind: JumpKind::Return,
                    span: Span::new(0, 0),
                    expr: Some(Box::new(expr)),
                    id: self.next_id(),
                })],
                id: self.next_id(),
            },
            // Add final return in fn body if none to avoid falling through
            AstStmt::Block { mut body, id } => {
                match body.last() {
                    Some(AstStmt::Expr(AstExpr::Jump {
                        kind: JumpKind::Return,
                        ..
                    })) => (), // No-op
                    _ => {
                        body.push(AstStmt::Expr(AstExpr::Jump {
                            span: Span::new(0, 0),
                            kind: JumpKind::Return,
                            expr: None,
                            id: self.next_id(),
                        }));
                    }
                }
                AstStmt::Block { body, id }
            }
            _ => unreachable!(),
        }
    }

    fn statement(&mut self, statement: AstStmt) -> AstStmt {
        match statement {
            AstStmt::Block { body, id } => {
                let body = body
                    .into_iter()
                    .map(|stmt| self.statement(stmt))
                    .collect::<Vec<_>>();
                AstStmt::Block { body, id }
            }
            AstStmt::FnDefinition {
                fn_name,
                body,
                id,
                args,
                flags,
                return_t_span,
            } => AstStmt::FnDefinition {
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

    pub(crate) fn statements(&mut self, block: AstStmt) -> AstStmt {
        self.statement(block)
    }
}
