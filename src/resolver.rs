use crate::error::*;
use crate::lex::Lexer;
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

    fn expr(&mut self, expr: &AstNode) -> Result<(), Error> {
        match &expr.kind {
            AstNodeExpr::Literal(_) => (),
            AstNodeExpr::Unary(_, expr) => {
                self.expr(&*expr)?;
            }
            AstNodeExpr::IfExpr { .. } | AstNodeExpr::WhenExpr { .. } => unimplemented!(),
            _ => unreachable!(),
        };
        Ok(())
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<(), Error> {
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
