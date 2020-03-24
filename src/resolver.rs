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

    fn when_entry(&mut self, entry: &WhenEntry) -> Result<(), Error> {
        self.expr(&entry.cond)?;
        self.statements(&entry.body)?;
        Ok(())
    }

    fn expr(&mut self, expr: &AstNode) -> Result<(), Error> {
        match &expr.kind {
            AstNodeExpr::Literal(_) => (),
            AstNodeExpr::Grouping(expr) | AstNodeExpr::Unary(_, expr) => {
                self.expr(&*expr)?;
            }
            AstNodeExpr::Binary(left, _, right) => {
                self.expr(&*left)?;
                self.expr(&*right)?;
            }
            AstNodeExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => {
                self.expr(cond)?;
                self.statements(if_body)?;
                self.statements(else_body)?;
            }
            AstNodeExpr::WhenExpr {
                subject,
                entries,
                else_entry,
            } => {
                if let Some(subject) = subject {
                    self.expr(subject)?;
                }

                for entry in entries {
                    self.when_entry(entry)?;
                }

                if let Some([else_entry]) = else_entry.as_ref().map(|v| v.as_slice()) {
                    self.statement(else_entry)?;
                }
            }
        };
        Ok(())
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<(), Error> {
        match statement {
            AstNodeStmt::Expr(expr) => self.expr(expr),
            AstNodeStmt::While { .. } | AstNodeStmt::DoWhile { .. } => unimplemented!(),
            AstNodeStmt::VarDeclaration { .. } => unimplemented!(),
        }
    }

    pub(crate) fn statements(&mut self, statements: &BlockSlice) -> Result<Resolution, Error> {
        for stmt in statements {
            self.statement(stmt)?;
        }
        Ok(self.resolution)
    }
}
