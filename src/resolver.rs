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

    fn expr(&mut self, expr: &AstNode) -> Result<Resolution, Error> {
        match expr.kind {
            AstNodeExpr::IfExpr { .. } | AstNodeExpr::WhenExpr { .. } => unreachable!(),
            _ => unreachable!(),
        }
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<Resolution, Error> {
        match statement {
            AstNodeStmt::Expr(expr) => self.expr(expr),
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
