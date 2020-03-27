use crate::lex::{Span, Token, TokenKind};
use crate::parse::*;
// use crate::resolver::Resolution;
// use log::debug;

pub(crate) struct MirTransformer {}

impl MirTransformer {
    pub(crate) fn new() -> MirTransformer {
        MirTransformer {}
    }

    fn do_while(&self, cond: AstNodeExpr, body: Block) -> AstNodeStmt {
        // FIXME

        let mut new_body = body.body;
        new_body.push(AstNodeStmt::Expr(AstNodeExpr::IfExpr {
            cond: Box::new(cond),
            cond_start_tok: Token {
                kind: TokenKind::LeftParen,
                span: Span::new(0, 0),
            },
            if_body: Block {
                id: 0,
                body: vec![],
            },
            else_body: Block {
                id: 0,
                body: vec![], // TODO: break;
            },
            else_body_span: Span::new(0, 0),
            id: 0,
        }));

        AstNodeStmt::While {
            cond: AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Bool(true),
                    span: Span::new(0, 0),
                },
                99,
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

    fn statement(&self, statement: AstNodeStmt) -> AstNodeStmt {
        match statement {
            // AstNodeStmt::DoWhile { cond, body, .. } => self.do_while(cond, body),
            _ => statement,
        }
    }

    pub(crate) fn statements(&self, block: Block) -> Block {
        let id = block.id;
        let body = block
            .body
            .into_iter()
            .map(|stmt| self.statement(stmt))
            .collect::<Vec<_>>();

        Block { id, body }
    }
}
