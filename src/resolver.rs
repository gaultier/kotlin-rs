use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::{Session, Span};
use log::debug;
use std::collections::BTreeMap;
use std::fmt;

const LEXICAL_CONTEXT_TOP_LEVEL: u8 = 0;
const LEXICAL_CONTEXT_LOOP: u8 = 0b001;
const LEXICAL_CONTEXT_FUNCTION: u8 = 0b010;
const LEXICAL_CONTEXT_LOOP_IN_FUNCTION: u8 = 0b011;
const LEXICAL_CONTEXT_CLASS: u8 = 0b100;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LexicalContext(pub u8);

impl LexicalContext {
    fn is_in_loop(self) -> bool {
        self.0 & LEXICAL_CONTEXT_LOOP != 0
    }

    fn is_in_function(self) -> bool {
        self.0 & LEXICAL_CONTEXT_FUNCTION != 0
    }

    fn enter_loop(&mut self) {
        self.0 |= LEXICAL_CONTEXT_LOOP;
    }

    fn enter_function(&mut self) {
        self.0 |= LEXICAL_CONTEXT_FUNCTION;
    }

    fn enter_class(&mut self) {
        self.0 |= LEXICAL_CONTEXT_CLASS;
    }
}

impl fmt::Display for LexicalContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            LEXICAL_CONTEXT_TOP_LEVEL => write!(f, "top level"),
            LEXICAL_CONTEXT_FUNCTION => write!(f, "function"),
            LEXICAL_CONTEXT_LOOP | LEXICAL_CONTEXT_LOOP_IN_FUNCTION => write!(f, "loop"),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct FnDef<'a> {
    identifier: &'a str,
    id: NodeId,
    block_id: NodeId,
    flags: u16,
}

pub(crate) struct Resolver<'a> {
    session: &'a Session<'a>,
    resolution: Resolution,
    scopes: Scopes<'a>,
    context: LexicalContext,
    fn_definitions: BTreeMap<(NodeId, &'a str), FnDef<'a>>, // Key=(block_id, identifier)
    class_definitions: BTreeMap<(NodeId, &'a str), FnDef<'a>>, // Key=(block_id, identifier)
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
    pub(crate) fn new(session: &'a Session) -> Resolver<'a> {
        Resolver {
            session,
            resolution: Resolution::new(),
            scopes: Vec::new(),
            context: LexicalContext(LEXICAL_CONTEXT_TOP_LEVEL),
            fn_definitions: BTreeMap::new(),
            class_definitions: BTreeMap::new(),
        }
    }

    fn when_entry0(&mut self, entry: &WhenEntry) -> Result<(), Error> {
        self.expr0(&entry.cond)?;
        self.statement0(&entry.body)?;
        Ok(())
    }

    fn when_entry(&mut self, entry: &WhenEntry) -> Result<(), Error> {
        self.expr(&entry.cond)?;
        self.statement(&entry.body)?;
        Ok(())
    }

    fn find_var(&self, identifier: &str) -> Option<(NodeId, Var, usize)> {
        let depth = self
            .scopes
            .iter()
            .rev()
            .position(|scope| scope.var_statuses.contains_key(identifier))?;
        let scope = self.scopes.iter().rev().nth(depth)?;
        let var = scope.var_statuses.get(identifier)?;
        Some((scope.block_id, *var, depth))
    }

    fn find_fn(&self, identifier: &str) -> Option<(NodeId, Var, usize)> {
        let depth = self.scopes.iter().rev().position(|scope| {
            self.fn_definitions
                .get(&(scope.block_id, identifier))
                .is_some()
        })?;
        let fn_def = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| self.fn_definitions.get(&(scope.block_id, identifier)))?;

        let var = Var {
            id: fn_def.id,
            flags: fn_def.flags,
            status: VarStatus::Declared,
        };
        Some((fn_def.block_id, var, depth))
    }

    fn var_ref(&mut self, span: &Span, id: NodeId) -> Result<(), Error> {
        let identifier = &self.session.src[span.start..span.end];
        let (block_id, var, depth) = self
            .find_var(identifier)
            .or_else(|| self.find_fn(identifier))
            .ok_or_else(|| {
                Error::new(
                    ErrorKind::UnknownIdentifier(identifier.to_string()),
                    self.session.span_location(span),
                )
            })?;

        let var_usage_ref = VarUsageRef {
            scope_depth: depth,
            node_ref_id: var.id,
            node_ref_flags: var.flags,
            block_id_ref: block_id,
        };

        debug!(
            "var_ref: identifier={} id={} depth={} scope_id={} node_ref_id={}",
            identifier, id, depth, block_id, var.id
        );
        self.resolution.insert(id, var_usage_ref);
        Ok(())
    }

    fn expr0(&mut self, expr: &AstNodeExpr) -> Result<(), Error> {
        match &expr {
            AstNodeExpr::Literal(_, _) => (),
            AstNodeExpr::Grouping(expr, _) | AstNodeExpr::Unary { expr, .. } => {
                self.expr0(&*expr)?;
            }
            AstNodeExpr::Binary {
                left,
                op:
                    Token {
                        kind: TokenKind::KeywordAs(_),
                        ..
                    },
                ..
            } => {
                self.expr0(&*left)?;
            }
            AstNodeExpr::Binary { left, right, .. } => {
                self.expr0(&*left)?;
                self.expr0(&*right)?;
            }
            AstNodeExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => {
                self.expr0(cond)?;
                self.statements0(if_body)?;
                self.statements0(else_body)?;
            }
            AstNodeExpr::WhenExpr {
                subject,
                entries,
                else_entry,
                ..
            } => {
                if let Some(subject) = subject {
                    self.statement0(subject)?;
                }

                for entry in entries {
                    self.when_entry0(entry)?;
                }

                if let Some(else_entry) = else_entry {
                    self.statement0(else_entry)?;
                }
            }
            AstNodeExpr::VarRef(_, _) => {}
            AstNodeExpr::FnCall { .. } => {}
            AstNodeExpr::Jump {
                kind: JumpKind::Break,
                ..
            }
            | AstNodeExpr::Jump {
                kind: JumpKind::Continue,
                ..
            } => {}
            AstNodeExpr::Jump {
                kind: JumpKind::Return,
                ..
            } => {}
            AstNodeExpr::Jump {
                kind: JumpKind::Throw,
                ..
            } => unimplemented!("Throw expressions"),
            AstNodeExpr::RangeTest { .. } => {}
            AstNodeExpr::TypeTest { .. } => {}
            AstNodeExpr::Println(_, _) => {}
        };
        Ok(())
    }

    fn expr(&mut self, expr: &AstNodeExpr) -> Result<(), Error> {
        match &expr {
            AstNodeExpr::Literal(_, _) => (),
            AstNodeExpr::Grouping(expr, _) | AstNodeExpr::Unary { expr, .. } => {
                self.expr(&*expr)?;
            }
            AstNodeExpr::Binary {
                left,
                op:
                    Token {
                        kind: TokenKind::KeywordIs,
                        ..
                    },
                ..
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    Token {
                        kind: TokenKind::KeywordAs(_),
                        ..
                    },
                ..
            } => {
                self.expr(&*left)?;
                // No need to resolve the rifght part which is the type
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

                if let Some(else_entry) = else_entry {
                    self.statement(else_entry)?;
                }
            }
            AstNodeExpr::VarRef(span, id) => {
                self.var_ref(span, *id)?;
            }
            AstNodeExpr::FnCall { fn_name, args, .. } => {
                self.fn_call(fn_name, args)?;
            }
            AstNodeExpr::Jump {
                kind: k @ JumpKind::Break,
                span,
                ..
            }
            | AstNodeExpr::Jump {
                kind: k @ JumpKind::Continue,
                span,
                ..
            } => {
                if !self.context.is_in_loop() {
                    return Err(Error::new(
                        ErrorKind::JumpInInvalidContext {
                            jump_kind: *k,
                            expected_context: LexicalContext(LEXICAL_CONTEXT_LOOP),
                            found_context: self.context,
                        },
                        self.session.span_location(span),
                    ));
                }
            }
            AstNodeExpr::Jump {
                kind: k @ JumpKind::Return,
                span,
                expr,
                ..
            } => {
                if !self.context.is_in_function() {
                    return Err(Error::new(
                        ErrorKind::JumpInInvalidContext {
                            jump_kind: *k,
                            expected_context: LexicalContext(LEXICAL_CONTEXT_FUNCTION),
                            found_context: self.context,
                        },
                        self.session.span_location(span),
                    ));
                }

                if let Some(expr) = expr {
                    self.expr(expr)?;
                }
            }
            AstNodeExpr::Jump {
                kind: JumpKind::Throw,
                ..
            } => unimplemented!("Throw expressions"),
            AstNodeExpr::RangeTest { range, .. } => {
                self.expr(range)?;
            }
            AstNodeExpr::TypeTest { .. } => {}
            AstNodeExpr::Println(expr, _) => {
                self.expr(expr)?;
            }
        };
        Ok(())
    }

    fn var_ref_fn(&mut self, fn_name: &AstNodeExpr) -> Result<(), Error> {
        match fn_name {
            AstNodeExpr::VarRef(span, id) => {
                let identifier = &self.session.src[span.start..span.end];
                let (block_id, var, depth) = self.find_fn(identifier).ok_or_else(|| {
                    if self.find_var(identifier).is_some() {
                        Error::new(
                            ErrorKind::NotACallable(Type::TString),
                            self.session.span_location(span),
                        )
                    } else {
                        Error::new(
                            ErrorKind::UnknownIdentifier(identifier.to_string()),
                            self.session.span_location(span),
                        )
                    }
                })?;

                let var_usage_ref = VarUsageRef {
                    scope_depth: depth,
                    node_ref_id: var.id,
                    node_ref_flags: var.flags,
                    block_id_ref: block_id,
                };

                debug!(
                    "var_ref_fn: identifier={} id={} depth={} scope_id={} node_ref_id={}",
                    identifier, id, depth, block_id, var.id
                );
                self.resolution.insert(*id, var_usage_ref);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn fn_call(&mut self, fn_name: &AstNodeExpr, args: &[AstNodeExpr]) -> Result<(), Error> {
        self.var_ref_fn(fn_name)?;

        for arg in args {
            self.expr(arg)?;
        }
        debug!("fn_call: fn_name={:?} args={:?}", fn_name, args);
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

    fn fn_name_decl(&mut self, fn_name: &AstNodeExpr, flags: u16, id: NodeId) -> Result<(), Error> {
        let block_id = self.scopes.last().unwrap().block_id;
        let identifier = match fn_name {
            AstNodeExpr::VarRef(span, _) => &self.session.src[span.start..span.end],
            _ => unimplemented!("Complex function names (e.g extension methods)"),
        };

        self.fn_definitions.insert(
            (block_id, identifier),
            FnDef {
                id,
                flags,
                block_id,
                identifier,
            },
        );
        debug!(
            "fn declaration: identifier=`{}` scope_id={} id={}",
            identifier, block_id, id
        );
        Ok(())
    }

    fn class_decl(&mut self, name: &'a str, flags: u16, id: NodeId) -> Result<(), Error> {
        let block_id = self.scopes.last().unwrap().block_id;

        self.fn_definitions.insert(
            (block_id, name),
            FnDef {
                id,
                flags,
                block_id,
                identifier: name,
            },
        );
        debug!(
            "class declaration: name=`{}` scope_id={} id={}",
            name, block_id, id
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
            AstNodeExpr::VarRef(span, _) => (&self.session.src[span.start..span.end], span),
            _ => unreachable!(),
        };

        let (_, var, _) = self.find_var(identifier).unwrap();
        let flags = var.flags;
        if flags & FLAG_VAL as u16 == FLAG_VAL {
            return Err(Error::new(
                ErrorKind::CannotReassignVal(identifier.to_string()),
                self.session.span_location(span),
            ));
        }

        debug!(
            "assign: identifier={} target={:?} value={:?} flags={}",
            identifier, target, value, flags
        );
        Ok(())
    }

    fn class_def0(
        &mut self,
        name: &'a str,
        body: &AstNodeStmt,
        flags: u16,
        id: NodeId,
    ) -> Result<(), Error> {
        self.class_decl(name, flags, id)?;
        self.enter_scope(id);

        let ctx = self.context;
        self.context.enter_class();
        self.statement0(body)?;
        self.context = ctx;

        self.exit_scope();
        Ok(())
    }

    fn fn_def0(
        &mut self,
        fn_name: &AstNodeExpr,
        flags: u16,
        body: &AstNodeStmt,
        id: NodeId,
    ) -> Result<(), Error> {
        self.fn_name_decl(fn_name, flags, id)?;
        self.enter_scope(id);

        let ctx = self.context;
        self.context.enter_function();
        self.statement0(body)?;
        self.context = ctx;

        self.exit_scope();
        Ok(())
    }

    fn fn_def(
        &mut self,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        id: NodeId,
    ) -> Result<(), Error> {
        self.enter_scope(id);

        for arg in args {
            match arg {
                AstNodeExpr::VarRef(span, id) => {
                    let identifier = &self.session.src[span.start..span.end];
                    self.var_decl(identifier, 0, *id)?;
                    self.var_def(identifier, 0, *id)?;
                }
                _ => unreachable!(),
            }
        }

        let ctx = self.context;
        self.context.enter_function();
        self.statement(body)?;
        self.context = ctx;

        self.exit_scope();
        Ok(())
    }

    fn fn_block0(&mut self, body: &[AstNodeStmt]) -> Result<(), Error> {
        for stmt in body {
            self.statement0(stmt)?;
        }
        Ok(())
    }

    fn fn_block(&mut self, body: &[AstNodeStmt]) -> Result<(), Error> {
        for stmt in body {
            self.statement(stmt)?;
        }
        Ok(())
    }

    fn statement0(&mut self, statement: &AstNodeStmt) -> Result<(), Error> {
        match statement {
            AstNodeStmt::Expr(expr) => {
                self.expr0(expr)?;
            }
            AstNodeStmt::DoWhile { body, .. } | AstNodeStmt::While { body, .. } => {
                let ctx = self.context;
                self.context.enter_loop();
                self.statements0(body)?;
                self.context = ctx;
            }
            AstNodeStmt::VarDefinition { .. } => {}
            AstNodeStmt::Assign { .. } => {}
            AstNodeStmt::FnDefinition {
                fn_name,
                flags,
                body,
                id,
                ..
            } => {
                self.fn_def0(fn_name, *flags, body, *id)?;
            }
            AstNodeStmt::Block { body, .. } => self.fn_block0(body)?,
            AstNodeStmt::Class {
                name_span,
                body,
                flags,
                id,
            } => {
                let name = &self.session.src[name_span.start..name_span.end];
                self.class_def0(name, body, *flags, *id)?;
            }
        };
        Ok(())
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<(), Error> {
        match statement {
            AstNodeStmt::Expr(expr) => {
                self.expr(expr)?;
            }
            AstNodeStmt::DoWhile { cond, body, .. } => {
                let ctx = self.context;
                self.context.enter_loop();

                match &**body {
                    AstNodeStmt::Block { id, body } => {
                        self.enter_scope(*id);
                        for stmt in body {
                            self.statement(&stmt)?;
                        }
                        // The condition is in the same scope as the loop body for the resolution
                        self.expr(cond)?;
                        self.exit_scope();
                    }
                    stmt => self.statement(&stmt)?,
                }
                self.context = ctx;
            }
            AstNodeStmt::While { cond, body, .. } => {
                let ctx = self.context;
                self.expr(cond)?;
                self.context.enter_loop();
                self.statements(body)?;
                self.context = ctx;
            }
            AstNodeStmt::VarDefinition {
                identifier,
                value,
                id,
                flags,
            } => {
                self.expr(value)?;
                let identifier = &self.session.src[identifier.span.start..identifier.span.end];
                self.var_decl(identifier, *flags as u16, *id)?;
                self.var_def(identifier, *flags as u16, *id)?;
            }
            AstNodeStmt::Assign { target, value, .. } => {
                self.assign(target, value)?;
            }
            AstNodeStmt::FnDefinition { args, body, id, .. } => {
                self.fn_def(args, body, *id)?;
            }
            AstNodeStmt::Block { body, .. } => self.fn_block(body)?,
            AstNodeStmt::Class { .. } => {
                // TODO
            }
        };
        Ok(())
    }

    fn statements0(&mut self, block: &AstNodeStmt) -> Result<(), Error> {
        match block {
            AstNodeStmt::Block { id, body } => {
                self.enter_scope(*id);
                for stmt in body {
                    self.statement0(&stmt)?;
                }
                self.exit_scope();
            }
            AstNodeStmt::Expr(expr) => {
                self.expr0(expr)?;
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn statements(&mut self, block: &AstNodeStmt) -> Result<(), Error> {
        match block {
            AstNodeStmt::Block { id, body } => {
                self.enter_scope(*id);
                for stmt in body {
                    self.statement(&stmt)?;
                }
                self.exit_scope();
            }
            AstNodeStmt::Expr(expr) => {
                self.expr(expr)?;
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    pub(crate) fn resolve(&mut self, block: &AstNodeStmt) -> Result<Resolution, Error> {
        self.statements0(block)?;
        debug!("fn_definitions={:?}", &self.fn_definitions);
        self.statements(block)?;

        Ok(self.resolution.clone())
    }
}
