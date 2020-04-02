use crate::error::*;
use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::parse::*;

pub struct Formatter<'a> {
    lexer: &'a Lexer,
}

impl<'a> Formatter<'a> {
    pub fn new(lexer: &'a Lexer) -> Formatter<'a> {
        Formatter { lexer }
    }

    fn assign<W: std::io::Write>(
        &self,
        target: &AstNodeExpr,
        value: &AstNodeExpr,
        span: &Span,
        w: &mut W,
    ) -> Result<(), Error> {
        self.expr(target, w)?;
        write!(w, " {} ", &self.lexer.src[span.start..span.end]).unwrap();
        self.expr(value, w)?;

        Ok(())
    }

    fn statement<W: std::io::Write>(&self, stmt: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match stmt {
            AstNodeStmt::Expr(expr) => {
                self.expr(expr, w)?;
            }
            AstNodeStmt::While { .. } => {
                self.while_stmt(stmt, w)?;
            }
            AstNodeStmt::DoWhile { .. } => {
                self.do_while_stmt(stmt, w)?;
            }
            AstNodeStmt::VarDefinition { .. } => {
                self.var_def(stmt, w)?;
            }
            AstNodeStmt::Assign {
                target,
                value,
                span,
                ..
            } => {
                self.assign(target, value, span, w)?;
            }
            AstNodeStmt::FnDefinition {
                fn_name,
                args,
                body,
                ..
            } => {
                self.fn_def(fn_name, args, body, w)?;
            }
            AstNodeStmt::Block { body, .. } => {
                self.block(body, w)?;
            }
            AstNodeStmt::Println(expr) => {
                write!(w, "println(").unwrap();
                self.expr(expr, w)?;
                writeln!(w, ")").unwrap();
            }
        };
        Ok(())
    }

    pub fn statements<W: std::io::Write>(
        &self,
        block: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.statement(&block, w)?;
        writeln!(w).unwrap();
        Ok(())
    }

    fn var_def<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::VarDefinition {
                identifier,
                value,
                flags,
                ..
            } => {
                write!(
                    w,
                    "{} {} = ",
                    if *flags & FLAG_VAR != 0 { "var" } else { "val" },
                    &self.lexer.src[identifier.span.start..identifier.span.end]
                )
                .unwrap();
                self.expr(value, w)?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn do_while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::DoWhile { cond, body, .. } => {
                writeln!(w, "do ").unwrap();
                self.statement(body, w)?;
                write!(w, "\nwhile (").unwrap();
                self.expr(cond, w)?;
                writeln!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::While { cond, body, .. } => {
                write!(w, "while (").unwrap();
                self.expr(cond, w)?;
                writeln!(w, ")").unwrap();
                self.statement(body, w)?;
                writeln!(w, "").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn literal<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        write!(w, "{}", &self.lexer.src[ast.span().start..ast.span().end]).unwrap();
        Ok(())
    }

    fn unary<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::Unary {
                token, expr, kind, ..
            } => {
                if *kind == UnaryKind::Postfix {
                    self.expr(expr, w)?;
                    write!(w, "{}", &self.lexer.src[token.span.start..token.span.end]).unwrap();
                } else {
                    write!(w, "{}", &self.lexer.src[token.span.start..token.span.end]).unwrap();
                    self.expr(expr, w)?;
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn binary<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::Binary {
                left, op, right, ..
            } => {
                self.expr(left, w)?;
                write!(w, " {} ", &self.lexer.src[op.span.start..op.span.end]).unwrap();
                self.expr(right, w)?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn block<W: std::io::Write>(&self, block: &[AstNodeStmt], w: &mut W) -> Result<(), Error> {
        if block.len() != 1 {
            write!(w, "{{").unwrap();
        }

        for stmt in block.iter() {
            self.statement(&stmt, w)?;
            write!(w, " ").unwrap();
        }

        if block.len() != 1 {
            writeln!(w, "}}").unwrap();
        }
        Ok(())
    }

    fn fn_call<W: std::io::Write>(
        &self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
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
        &self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        write!(w, "fun ").unwrap();
        self.expr(fn_name, w)?;
        write!(w, "(").unwrap();

        if let Some((last, args)) = args.split_last() {
            for arg in args {
                self.expr(arg, w)?;
                write!(w, ", ").unwrap();
            }

            self.expr(last, w)?;
        }
        write!(w, ") = ").unwrap();

        self.statement(body, w)?;
        writeln!(w, "").unwrap();
        Ok(())
    }

    fn when_expr<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::WhenExpr {
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
                write!(w, " {{").unwrap();

                for entry in entries {
                    self.expr(&entry.cond, w)?;
                    write!(w, " -> ").unwrap();
                    self.statement(&entry.body, w)?;
                    writeln!(w, "").unwrap();
                }

                if let Some(else_entry) = else_entry {
                    write!(w, "\nelse -> ").unwrap();
                    self.statement(&else_entry, w)?;
                }

                write!(w, "\n}}").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn if_expr<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => {
                write!(w, "if (").unwrap();
                self.expr(cond, w)?;
                write!(w, ") ").unwrap();
                self.statement(&*if_body, w)?;
                write!(w, " else ").unwrap();
                self.statement(&*else_body, w)?;
                write!(w, "").unwrap();

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn var_ref<W: std::io::Write>(&self, span: &Span, w: &mut W) -> Result<(), Error> {
        let identifier = &self.lexer.src[span.start..span.end];
        write!(w, "{}", identifier).unwrap();
        Ok(())
    }

    fn jump_expr<W: std::io::Write>(
        &self,
        kind: JumpKind,
        expr: &Option<Box<AstNodeExpr>>,
        w: &mut W,
    ) -> Result<(), Error> {
        match kind {
            JumpKind::Break | JumpKind::Continue => write!(w, "{}", kind).unwrap(),
            JumpKind::Return => {
                write!(w, "return ").unwrap();
                if let Some(expr) = expr {
                    self.expr(expr, w)?;
                }
                writeln!(w, "").unwrap();
            }
            JumpKind::Throw => unimplemented!(),
        }
        Ok(())
    }

    fn range_test<W: std::io::Write>(
        &self,
        range: &AstNodeExpr,
        cond: bool,
        w: &mut W,
    ) -> Result<(), Error> {
        if !cond {
            write!(w, "(not ").unwrap();
        }
        write!(w, "(in ").unwrap();
        self.expr(range, w)?;
        write!(w, ")").unwrap();

        if !cond {
            write!(w, ")").unwrap();
        }
        Ok(())
    }

    fn type_test<W: std::io::Write>(
        &self,
        identifier: &AstNodeExpr,
        cond: bool,
        w: &mut W,
    ) -> Result<(), Error> {
        if !cond {
            write!(w, "(not ").unwrap();
        }
        write!(w, "(is ").unwrap();
        self.expr(identifier, w)?;
        write!(w, ")").unwrap();

        if !cond {
            write!(w, ")").unwrap();
        }
        Ok(())
    }

    pub fn expr<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::WhenExpr { .. } => self.when_expr(ast, w),
            AstNodeExpr::Literal(..) => self.literal(ast, w),
            AstNodeExpr::Unary { .. } => self.unary(ast, w),
            AstNodeExpr::Binary { .. } => self.binary(ast, w),
            AstNodeExpr::Grouping(expr, _) => {
                write!(w, "(").unwrap();
                self.expr(expr, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            AstNodeExpr::IfExpr { .. } => self.if_expr(ast, w),
            AstNodeExpr::VarRef(span, _) => self.var_ref(span, w),
            AstNodeExpr::FnCall { fn_name, args, .. } => self.fn_call(fn_name, args, w),
            AstNodeExpr::Jump { kind, expr, .. } => self.jump_expr(*kind, expr, w),
            AstNodeExpr::RangeTest { range, cond, .. } => self.range_test(range, *cond, w),
            AstNodeExpr::TypeTest {
                identifier, cond, ..
            } => self.type_test(identifier, *cond, w),
        }
    }
}
