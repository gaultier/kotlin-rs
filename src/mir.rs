use crate::lex::{Span, Token, TokenKind};
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

    fn _do_while(&mut self, cond: AstNodeExpr, body: AstNodeStmt) -> AstNodeStmt {
        let (mut body, id) = match body {
            AstNodeStmt::Block { body, id } => (body, id),
            _ => unreachable!(),
        };
        body.push(AstNodeStmt::Expr(AstNodeExpr::IfExpr {
            cond: Box::new(cond),
            cond_span: Span::new(0, 0),
            if_body: Box::new(AstNodeStmt::Block {
                id: self._next_id(),
                body: vec![],
            }),
            else_body: Box::new(AstNodeStmt::Block {
                id: self._next_id(),
                body: vec![], // TODO: break;
            }),
            else_body_span: Span::new(0, 0),
            id: self._next_id(),
        }));

        AstNodeStmt::While {
            cond: AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Boolean(true),
                    span: Span::new(0, 0),
                },
                self._next_id(),
            ),
            cond_start_tok: Token {
                kind: TokenKind::LeftParen,
                span: Span::new(0, 0),
            },
            body: Box::new(AstNodeStmt::Block { id, body }),
        }
    }

    fn _statement(&mut self, statement: AstNodeStmt) -> AstNodeStmt {
        match statement {
            AstNodeStmt::DoWhile { cond, body, .. } => self._do_while(cond, *body),
            AstNodeStmt::Block { body, id } => AstNodeStmt::Block {
                id,
                body: body
                    .into_iter()
                    .map(|stmt| self._statement(stmt))
                    .collect::<Vec<_>>(),
            },
            _ => statement,
        }
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
