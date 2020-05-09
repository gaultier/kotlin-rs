use crate::error::*;
use crate::lex::Token;
use crate::parse::*;
use crate::session::{Session, Span};
use log::debug;

pub struct Formatter<'a> {
    session: &'a Session<'a>,
    ident: isize,
    types: &'a Types,
}

impl<'a> Formatter<'a> {
    pub fn new(session: &'a Session, types: &'a Types) -> Formatter<'a> {
        Formatter {
            session,
            ident: -1,
            types,
        }
    }

    fn assign<W: std::io::Write>(
        &mut self,
        target: &AstExpr,
        value: &AstExpr,
        span: &Span,
        w: &mut W,
    ) -> Result<(), Error> {
        self.ident(w);
        self.expr(target, w)?;
        write!(w, " {} ", &self.session.src[span.start..span.end]).unwrap();
        self.expr(value, w)?;

        Ok(())
    }

    fn statement<W: std::io::Write>(&mut self, stmt: &AstStmt, w: &mut W) -> Result<(), Error> {
        match stmt {
            AstStmt::Expr(expr) => {
                self.expr(expr, w)?;
            }
            AstStmt::While { .. } => {
                self.while_stmt(stmt, w)?;
            }
            AstStmt::DoWhile { .. } => {
                self.do_while_stmt(stmt, w)?;
            }
            AstStmt::VarDefinition { .. } => {
                self.var_def(stmt, w)?;
            }
            AstStmt::Assign {
                target,
                value,
                span,
                ..
            } => {
                self.assign(target, value, span, w)?;
            }
            AstStmt::FnDefinition {
                fn_name,
                args,
                body,
                id,
                ..
            } => {
                self.fn_def(fn_name, args, body, *id, w)?;
            }
            AstStmt::Block { body, .. } => {
                self.block(body, w)?;
            }
            AstStmt::Class {
                name_span, body, ..
            } => {
                let name = self.session.src[name_span.start..name_span.end].into();
                self.class(name, body, w)?;
            }
        };
        Ok(())
    }

    fn class<W: std::io::Write>(
        &mut self,
        name: &str,
        body: &AstStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.ident(w);
        write!(w, "class {} {{\n", name)?;

        self.statements(body, w)?;
        self.ident(w);
        write!(w, "}}\n")?;
        Ok(())
    }

    pub fn statements<W: std::io::Write>(
        &mut self,
        block: &AstStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.statement(&block, w)?;
        Ok(())
    }

    fn var_def<W: std::io::Write>(&mut self, ast: &AstStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstStmt::VarDefinition {
                identifier,
                value,
                flags,
                ..
            } => {
                write!(
                    w,
                    "{} {} = ",
                    if *flags & FLAG_VAR != 0 { "var" } else { "val" },
                    &self.session.src[identifier.span.start..identifier.span.end]
                )
                .unwrap();
                self.expr(value, w)?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn do_while_stmt<W: std::io::Write>(
        &mut self,
        ast: &AstStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        match ast {
            AstStmt::DoWhile { cond, body, .. } => {
                writeln!(w, "do {{").unwrap();
                self.statement(body, w)?;
                writeln!(w).unwrap();
                self.ident(w);
                write!(w, "}} while (").unwrap();
                self.expr(cond, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn while_stmt<W: std::io::Write>(&mut self, ast: &AstStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstStmt::While { cond, body, .. } => {
                write!(w, "while (").unwrap();
                self.expr(cond, w)?;
                writeln!(w, ") {{").unwrap();
                self.statement(body, w)?;
                self.ident(w);
                write!(w, "}}").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn literal<W: std::io::Write>(&mut self, token: &Token, w: &mut W) -> Result<(), Error> {
        write!(w, "{}", &self.session.src[token.span.start..token.span.end]).unwrap();
        Ok(())
    }

    fn unary<W: std::io::Write>(&mut self, ast: &AstExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstExpr::Unary {
                token, expr, kind, ..
            } => {
                if *kind == UnaryKind::Postfix {
                    self.expr(expr, w)?;
                    write!(w, "{}", &self.session.src[token.span.start..token.span.end]).unwrap();
                } else {
                    write!(w, "{}", &self.session.src[token.span.start..token.span.end]).unwrap();
                    self.expr(expr, w)?;
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn binary<W: std::io::Write>(&mut self, ast: &AstExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstExpr::Binary {
                left, op, right, ..
            } => {
                self.expr(left, w)?;
                write!(w, " {} ", &self.session.src[op.span.start..op.span.end]).unwrap();
                self.expr(right, w)?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn block<W: std::io::Write>(&mut self, block: &[AstStmt], w: &mut W) -> Result<(), Error> {
        self.ident += 1;

        if let Some((stmt, stmts)) = block.split_last() {
            for stmt in stmts {
                self.ident(w);
                self.statement(&stmt, w)?;
                writeln!(w).unwrap();
            }
            self.ident(w);
            self.statement(&stmt, w)?;
        }

        self.ident -= 1;
        Ok(())
    }

    fn ident<W: std::io::Write>(&mut self, w: &mut W) {
        if self.ident > 0 {
            write!(w, "{}", str::repeat("    ", self.ident as usize)).unwrap();
        }
    }

    fn fn_call<W: std::io::Write>(
        &mut self,
        fn_name: &AstExpr,
        args: &[AstExpr],
        w: &mut W,
    ) -> Result<(), Error> {
        self.expr(fn_name, w)?;
        write!(w, "(").unwrap();
        if let Some((last, args)) = args.split_last() {
            for arg in args {
                self.expr(arg, w)?;
                write!(w, ", ").unwrap();
            }

            self.expr(last, w)?;
        }
        write!(w, ")").unwrap();

        Ok(())
    }

    fn fn_def<W: std::io::Write>(
        &mut self,
        fn_name: &AstExpr,
        args: &[AstExpr],
        body: &AstStmt,
        id: Id,
        w: &mut W,
    ) -> Result<(), Error> {
        write!(w, "fun ").unwrap();
        self.expr(fn_name, w)?;
        write!(w, "(").unwrap();

        if let Some((arg, args)) = args.split_last() {
            for arg in args {
                self.expr(arg, w)?;
                write!(w, ": {}, ", self.types.get(&arg.id()).unwrap()).unwrap();
            }

            self.expr(arg, w)?;
            write!(w, ": {}", self.types.get(&arg.id()).unwrap()).unwrap();
        }
        let return_t = self.types.get(&id).unwrap().fn_return_t().unwrap();
        debug!("fn_def: name={:?} return_t={}", fn_name, return_t);
        write!(w, "): {} ", return_t).unwrap();

        self.fn_body(body, &return_t, w)?;
        Ok(())
    }

    fn when_expr<W: std::io::Write>(&mut self, ast: &AstExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstExpr::WhenExpr {
                entries,
                else_entry,
                subject,
                ..
            } => {
                write!(w, "when ").unwrap();
                if let Some(subject) = subject {
                    write!(w, "(").unwrap();
                    self.statement(&subject, w)?;
                    write!(w, ")").unwrap();
                }
                writeln!(w, " {{").unwrap();

                self.ident += 1;

                for entry in entries {
                    self.ident(w);
                    self.expr(&entry.cond, w)?;
                    write!(w, " -> ").unwrap();
                    self.statement(&entry.body, w)?;
                    writeln!(w).unwrap();
                }

                if let Some(else_entry) = else_entry {
                    self.ident(w);
                    write!(w, "else -> ").unwrap();
                    self.statement(&else_entry, w)?;
                    writeln!(w).unwrap();
                }

                self.ident -= 1;
                self.ident(w);
                write!(w, "}}").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn fn_body<W: std::io::Write>(
        &mut self,
        ast: &AstStmt,
        return_t: &Type,
        w: &mut W,
    ) -> Result<(), Error> {
        // One empty line after the function declaration
        match ast {
            AstStmt::Block { body, .. } => match body.as_slice() {
                [AstStmt::Expr(AstExpr::Jump {
                    kind: JumpKind::Return,
                    expr: Some(expr),
                    ..
                })] => {
                    write!(w, "= ").unwrap();
                    self.expr(expr, w)?;
                    writeln!(w).unwrap();
                }
                _ => {
                    writeln!(w, "{{").unwrap();
                    self.block(body, w)?;
                    writeln!(w, "\n}}").unwrap();
                }
            },
            AstStmt::Expr(e) if self.types.get(&e.id()).unwrap() == return_t => {
                write!(w, "= ").unwrap();
                self.expr(e, w)?;
                writeln!(w).unwrap();
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn control_structure_body<W: std::io::Write>(
        &mut self,
        ast: &AstStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        match ast {
            AstStmt::Block { body, .. } => match body.as_slice() {
                [AstStmt::Expr(e)] => self.expr(e, w),
                _ => {
                    writeln!(w, "{{").unwrap();
                    self.block(body, w)?;
                    write!(w, "}}").unwrap();
                    Ok(())
                }
            },
            AstStmt::Expr(e) => self.expr(e, w),
            _ => unreachable!(),
        }
    }

    fn if_expr<W: std::io::Write>(&mut self, ast: &AstExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => {
                write!(w, "if (").unwrap();
                self.expr(cond, w)?;
                write!(w, ") ").unwrap();
                self.control_structure_body(&*if_body, w)?;
                write!(w, " else ").unwrap();
                self.control_structure_body(&*else_body, w)?;
                write!(w, "").unwrap();

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn var_ref<W: std::io::Write>(&mut self, span: &Span, w: &mut W) -> Result<(), Error> {
        let identifier = &self.session.src[span.start..span.end];
        write!(w, "{}", identifier).unwrap();
        Ok(())
    }

    fn jump_expr<W: std::io::Write>(
        &mut self,
        kind: JumpKind,
        expr: &Option<Box<AstExpr>>,
        w: &mut W,
    ) -> Result<(), Error> {
        match kind {
            JumpKind::Break | JumpKind::Continue => write!(w, "{}", kind).unwrap(),
            JumpKind::Return => {
                write!(w, "return ").unwrap();
                if let Some(expr) = expr {
                    self.expr(expr, w)?;
                }
                writeln!(w).unwrap();
            }
            JumpKind::Throw => unimplemented!("Throw expressions"),
        }
        Ok(())
    }

    fn range_test<W: std::io::Write>(
        &mut self,
        range: &AstExpr,
        cond: bool,
        w: &mut W,
    ) -> Result<(), Error> {
        if !cond {
            write!(w, "!").unwrap();
        }
        write!(w, "in ").unwrap();
        self.expr(range, w)?;

        Ok(())
    }

    fn type_test<W: std::io::Write>(
        &mut self,
        identifier: &AstExpr,
        cond: bool,
        w: &mut W,
    ) -> Result<(), Error> {
        if !cond {
            write!(w, "!").unwrap();
        }
        write!(w, "is ").unwrap();
        self.expr(identifier, w)?;

        Ok(())
    }

    fn expr<W: std::io::Write>(&mut self, ast: &AstExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstExpr::WhenExpr { .. } => self.when_expr(ast, w),
            AstExpr::Literal(token, ..) => self.literal(token, w),
            AstExpr::Unary { .. } => self.unary(ast, w),
            AstExpr::Binary { .. } => self.binary(ast, w),
            AstExpr::Grouping(expr, _) => {
                write!(w, "(").unwrap();
                self.expr(expr, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            AstExpr::IfExpr { .. } => self.if_expr(ast, w),
            AstExpr::VarRef(span, _) => self.var_ref(span, w),
            AstExpr::FnCall { fn_name, args, .. } => self.fn_call(fn_name, args, w),
            AstExpr::Jump { kind, expr, .. } => self.jump_expr(*kind, expr, w),
            AstExpr::RangeTest { range, cond, .. } => self.range_test(range, *cond, w),
            AstExpr::TypeTest {
                identifier, cond, ..
            } => self.type_test(identifier, *cond, w),
            AstExpr::Println(expr, _) => {
                write!(w, "println(").unwrap();
                self.expr(expr, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
        }
    }
}
