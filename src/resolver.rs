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

#[derive(Debug, Copy, Clone)]
struct Var {
    id: NodeId,
    status: VarStatus,
    flags: u16,
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    var_statuses: BTreeMap<&'a str, Var>,
    fn_statuses: BTreeMap<&'a str, Var>,
    block_id: NodeId,
}

impl<'a> Scope<'a> {
    fn new(block_id: NodeId) -> Scope<'a> {
        Scope {
            block_id,
            var_statuses: BTreeMap::new(),
            fn_statuses: BTreeMap::new(),
        }
    }
}

type Scopes<'a> = Vec<Scope<'a>>;

#[derive(Debug, Copy, Clone)]
pub(crate) struct VarUsageRef {
    pub(crate) scope_depth: usize,
    pub(crate) node_ref_id: NodeId,
    pub(crate) node_ref_flags: u16,
    pub(crate) block_id_ref: NodeId,
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

    fn find_var(&self, identifier: &str) -> Option<(&Scope, &Var, usize)> {
        let depth = self
            .scopes
            .iter()
            .rev()
            .position(|scope| scope.var_statuses.contains_key(identifier))?;
        let scope = self.scopes.iter().rev().nth(depth)?;
        let var = scope.var_statuses.get(identifier)?;
        Some((scope, var, depth))
    }

    fn find_fn(&self, identifier: &str) -> Option<(&Scope, &Var, usize)> {
        let depth = self
            .scopes
            .iter()
            .rev()
            .position(|scope| scope.fn_statuses.contains_key(identifier))?;
        let scope = self.scopes.iter().rev().nth(depth)?;
        let var = scope.fn_statuses.get(identifier)?;
        Some((scope, var, depth))
    }

    fn var_ref(&mut self, span: &Span, id: NodeId) -> Result<(), Error> {
        let identifier = &self.lexer.src[span.start..span.end];
        let (scope, var, depth) = self
            .find_var(identifier)
            .or_else(|| self.find_fn(identifier))
            .ok_or_else(|| {
                Error::new(
                    ErrorKind::UnknownIdentifier(identifier.to_string()),
                    self.lexer.span_location(span),
                )
            })?;

        let var_usage_ref = VarUsageRef {
            scope_depth: depth,
            node_ref_id: var.id,
            node_ref_flags: var.flags,
            block_id_ref: scope.block_id,
        };

        debug!(
            "var_ref: identifier={} id={} depth={} scope_id={} node_ref_id={}",
            identifier, id, depth, scope.block_id, var.id
        );
        self.resolution.insert(id, var_usage_ref);
        Ok(())
    }

    fn expr(&mut self, expr: &AstNodeExpr) -> Result<(), Error> {
        match &expr {
            AstNodeExpr::Literal(_, _) => (),
            AstNodeExpr::Grouping(expr, _) | AstNodeExpr::Unary { expr, .. } => {
                self.expr(&*expr)?;
            }
            AstNodeExpr::Binary { left, right, .. } => {
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
                ..
            } => {
                if let Some(subject) = subject {
                    self.statement(subject)?;
                }

                for entry in entries {
                    self.when_entry(entry)?;
                }

                if let Some([else_entry]) = else_entry.as_ref().map(|b| b.body.as_slice()) {
                    self.statement(else_entry)?;
                }
            }
            AstNodeExpr::VarRef(span, id) => {
                self.var_ref(span, *id)?;
            }
            AstNodeExpr::FnCall { fn_name, args, .. } => {
                self.fn_call(fn_name, args)?;
            }
        };
        Ok(())
    }

    fn fn_call(&mut self, fn_name: &AstNodeExpr, args: &[AstNodeExpr]) -> Result<(), Error> {
        self.expr(fn_name)?;

        for arg in args {
            self.expr(arg)?;
        }
        Ok(())
    }

    fn var_decl(&mut self, identifier: &'a str, flags: u16, id: NodeId) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        scope.var_statuses.insert(
            identifier,
            Var {
                id,
                status: VarStatus::Declared,
                flags,
            },
        );
        debug!(
            "var declaration: identifier=`{}` scope_id={} id={}",
            identifier, scope.block_id, id
        );
        Ok(())
    }

    fn fn_name_def(&mut self, fn_name: &AstNodeExpr, flags: u16, id: NodeId) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        let identifier = match fn_name {
            AstNodeExpr::VarRef(span, _) => &self.lexer.src[span.start..span.end],
            _ => unimplemented!(),
        };

        scope.fn_statuses.insert(
            identifier,
            Var {
                id,
                status: VarStatus::Defined,
                flags,
            },
        );
        debug!(
            "fn declaration: identifier=`{}` scope_id={} id={}",
            identifier, scope.block_id, id
        );
        Ok(())
    }

    fn var_def(&mut self, identifier: &'a str, flags: u16, id: NodeId) -> Result<(), Error> {
        let scope = self.scopes.last_mut().unwrap();
        scope.var_statuses.insert(
            identifier,
            Var {
                id,
                status: VarStatus::Defined,
                flags,
            },
        );
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

    fn assign(&mut self, target: &AstNodeExpr, value: &AstNodeExpr) -> Result<(), Error> {
        self.expr(target)?;
        self.expr(value)?;

        let (identifier, span) = match target {
            AstNodeExpr::VarRef(span, _) => (&self.lexer.src[span.start..span.end], span),
            _ => unreachable!(),
        };

        // FIXME
        let (_, var, _) = self.find_var(identifier).unwrap();
        let flags = var.flags;
        if flags & FLAG_VAL as u16 == 1 {
            return Err(Error::new(
                ErrorKind::CannotReassignVal(identifier.to_string()),
                self.lexer.span_location(span),
            ));
        }

        debug!(
            "assign: identifier={} target={:?} value={:?} flags={}",
            identifier, target, value, flags
        );
        Ok(())
    }

    fn fn_def(
        &mut self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        flags: u16,
        id: NodeId,
    ) -> Result<(), Error> {
        self.fn_name_def(fn_name, flags, id)?;

        self.enter_scope(id);

        for arg in args {
            self.expr(arg)?;
        }

        self.statement(body)?;

        self.exit_scope();
        Ok(())
    }

    fn fn_block(&mut self, block: &Block) -> Result<(), Error> {
        for stmt in &block.body {
            self.statement(stmt)?;
        }
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
            AstNodeStmt::VarDefinition {
                identifier,
                value,
                id,
                flags,
            } => {
                self.expr(value)?;
                let identifier = &self.lexer.src[identifier.span.start..identifier.span.end];
                self.var_decl(identifier, *flags as u16, *id)?;
                self.var_def(identifier, *flags as u16, *id)?;
            }
            AstNodeStmt::Assign { target, value, .. } => {
                self.assign(target, value)?;
            }
            AstNodeStmt::FnDefinition {
                fn_name,
                args,
                body,
                flags,
                id,
            } => {
                self.fn_def(fn_name, args, body, *flags, *id)?;
            }
            AstNodeStmt::Block(block) => self.fn_block(block)?,
            AstNodeStmt::Println(expr) => {
                self.expr(expr)?;
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
