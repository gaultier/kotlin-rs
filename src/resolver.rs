use crate::error::*;
use crate::lex::{Lexer, Span};
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

impl<'a> Scope<'a> {
    fn new(block_id: NodeId) -> Scope<'a> {
        Scope {
            block_id,
            var_statuses: BTreeMap::new(),
        }
    }
}

type Scopes<'a> = Vec<Scope<'a>>;

#[derive(Debug, Copy, Clone)]
pub(crate) struct VarUsageRef {
    pub(crate) scope_depth: usize,
    pub(crate) node_id_ref: NodeId,
}

pub(crate) type Resolution = BTreeMap<NodeId, VarUsageRef>;

impl<'a> Resolver<'a> {
    pub(crate) fn new(lexer: &Lexer) -> Resolver {
        Resolver {
            lexer,
            resolution: Resolution::new(),
            scopes: Vec::new(),
        }
    }

    fn when_entry(&mut self, entry: &WhenEntry) -> Result<(), Error> {
        self.expr(&entry.cond)?;
        self.statements(&entry.body)?;
        Ok(())
    }

    fn var_ref(&mut self, span: &Span, id: NodeId) -> Result<(), Error> {
        let identifier = &self.lexer.src[span.start..span.end];
        if let Some(depth) = self
            .scopes
            .iter()
            .rev()
            .position(|scope| scope.var_statuses.contains_key(identifier))
        {
            let scope = self.scopes.iter().rev().nth(depth).unwrap();
            self.resolution.insert(
                id,
                VarUsageRef {
                    scope_depth: depth,
                    node_id_ref: scope.block_id,
                },
            );
            debug!(
                "var_ref: identifier={} id={} depth={} scope_id={}",
                identifier, id, depth, scope.block_id
            );
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::UnknownIdentifier(identifier.to_string()),
                self.lexer.span_location(span),
            ))
        }
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
            AstNodeExpr::VarRef(span) => {
                self.var_ref(span, expr.id)?;
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

    fn enter_scope(&mut self, block_id: NodeId) {
        debug!("enter scope: id={}", block_id);
        self.scopes.push(Scope::new(block_id));
    }

    fn exit_scope(&mut self) {
        debug!("exit scope: id={}", self.scopes.last().unwrap().block_id);
        self.scopes.pop();
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

    pub(crate) fn statements(&mut self, block: &Block) -> Result<Resolution, Error> {
        self.enter_scope(block.id);
        for stmt in &block.body {
            self.statement(&stmt)?;
        }
        self.exit_scope();
        Ok(self.resolution.clone())
    }
}
