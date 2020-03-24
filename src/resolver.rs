use crate::error::*;
use crate::lex::Lexer;
use crate::parse::*;
use log::debug;
use std::collections::BTreeMap;

pub(crate) struct Resolver<'a> {
    lexer: &'a Lexer,
    resolution: Resolution,
    scopes: Scopes<'a>,
}

#[derive(Debug, Copy, Clone)]
enum VarStatus {
    Declared,
    Defined,
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    var_statuses: BTreeMap<&'a str, VarStatus>,
    block_id: NodeId,
}

type Scopes<'a> = Vec<Scope<'a>>;

#[derive(Debug, Copy, Clone)]
pub(crate) struct VarUsageRef {
    scope_depth: usize,
    node_id_ref: NodeId,
}

pub(crate) type Resolution = BTreeMap<NodeId, VarUsageRef>;

impl<'a> Resolver<'a> {
    pub(crate) fn new(lexer: &Lexer) -> Resolver {
        let global_scope = Scope {
            var_statuses: BTreeMap::new(),
            block_id: 0,
        };
        Resolver {
            lexer,
            resolution: Resolution::new(),
            scopes: vec![global_scope],
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

                if let Some([else_entry]) = else_entry.as_ref().map(|b| b.body.as_slice()) {
                    self.statement(else_entry)?;
                }
            }
        };
        Ok(())
    }

    fn var_decl(&mut self, identifier: &'a str) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        scope.var_statuses.insert(identifier, VarStatus::Declared);
        debug!(
            "var declaration: identifier=`{}` scope={}",
            identifier, scope.block_id
        );
        Ok(())
    }

    fn var_def(&mut self, identifier: &'a str) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        scope.var_statuses.insert(identifier, VarStatus::Defined);
        debug!(
            "var definition: identifier=`{}` scope={}",
            identifier, scope.block_id
        );
        Ok(())
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<(), Error> {
        match statement {
            AstNodeStmt::Expr(expr) => {
                self.expr(expr)?;
            }
            AstNodeStmt::While { cond, body, .. } | AstNodeStmt::DoWhile { cond, body, .. } => {
                self.expr(cond)?;
                self.statements(body)?;
            }
            AstNodeStmt::VarDefinition { identifier, value } => {
                self.expr(value)?;
                let identifier = &self.lexer.src[identifier.span.start..identifier.span.end];
                self.var_decl(identifier)?;
                self.var_def(identifier)?;
            }
        };
        Ok(())
    }

    pub(crate) fn statements(&mut self, statements: &Block) -> Result<Resolution, Error> {
        for stmt in &statements.body {
            self.statement(&stmt)?;
        }
        Ok(self.resolution.clone())
    }
}
