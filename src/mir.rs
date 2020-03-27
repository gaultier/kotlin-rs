use crate::lex::{Span, Token, TokenKind};
use crate::parse::*;
// use crate::resolver::Resolution;
// use log::debug;

pub(crate) struct MirTransformer {
    current_id: usize,
}

impl MirTransformer {
    pub(crate) fn new(current_id: usize) -> MirTransformer {
        MirTransformer { current_id }
    }

    fn do_while(&mut self, cond: AstNodeExpr, body: Block) -> AstNodeStmt {
        // FIXME

        let mut new_body = body.body;
        new_body.push(AstNodeStmt::Expr(AstNodeExpr::IfExpr {
            cond: Box::new(cond),
            cond_span: Span::new(0, 0),
            if_body: Block {
                id: self.next_id(),
                body: vec![],
            },
            else_body: Block {
                id: self.next_id(),
                body: vec![], // TODO: break;
            },
            else_body_span: Span::new(0, 0),
            id: self.next_id(),
        }));

        AstNodeStmt::While {
            cond: AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Bool(true),
                    span: Span::new(0, 0),
                },
                self.next_id(),
            ),
            cond_start_tok: Token {
                kind: TokenKind::LeftParen,
                span: Span::new(0, 0),
            },
            body: Block {
                id: body.id,
                body: new_body,
            },
        }
    }

    fn statement(&mut self, statement: AstNodeStmt) -> AstNodeStmt {
        match statement {
            AstNodeStmt::DoWhile { cond, body, .. } => self.do_while(cond, body),
            _ => statement,
        }
    }

    fn next_id(&mut self) -> NodeId {
        self.current_id = self
            .current_id
            .checked_add(1)
            .expect("Out of ids, input too big");
        self.current_id
    }

    pub(crate) fn statements(&mut self, block: Block) -> Block {
        let id = block.id;
        let body = block
            .body
            .into_iter()
            .map(|stmt| self.statement(stmt))
            .collect::<Vec<_>>();

        Block { id, body }
    }
}
