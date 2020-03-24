use crate::error::*;
use crate::lex::{Lexer, Token, TokenKind};
use crate::parse::*;

pub(crate) struct Resolver<'a> {
    lexer: &'a Lexer,
    resolution: Resolution,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct Resolution {}

impl Resolver<'_> {
    pub(crate) fn new(lexer: &Lexer) -> Resolver {
        Resolver {
            lexer,
            resolution: Resolution {},
        }
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<Resolution, Error> {
        match statement {
            AstNodeStmt::Expr(AstNode {
                kind: AstNodeExpr::WhenExpr { .. },
                ..
            })
            | AstNodeStmt::Expr(AstNode {
                kind: AstNodeExpr::IfExpr { .. },
                ..
            }) => unimplemented!(),
            AstNodeStmt::While { .. } | AstNodeStmt::DoWhile { .. } => unimplemented!(),
            AstNodeStmt::VarDeclaration { .. } => unimplemented!(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn statements(&mut self, statements: &BlockSlice) -> Result<Resolution, Error> {
        for stmt in statements {
            self.statement(stmt)?;
        }
        Ok(self.resolution)
    }
}
