use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::{Session, Span};

pub(crate) struct SexpEmitter<'a> {
    session: &'a Session<'a>,
    _types: &'a Types,
}

fn unary_op(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::Bang => "not",
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::PlusPlus => "add1",
        TokenKind::MinusMinus => "sub1",
        _ => unreachable!(),
    }
}

fn binary_op(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Star => "*",
        TokenKind::Slash => "/",
        TokenKind::Percent => "%",
        TokenKind::DotDot => "range",
        TokenKind::EqualEqual => "==",
        TokenKind::EqualEqualEqual => "===",
        TokenKind::PipePipe => "or",
        TokenKind::AmpersandAmpersand => "and",
        TokenKind::Lesser => "<",
        TokenKind::LesserEqual => "<=",
        TokenKind::Greater => ">",
        TokenKind::GreaterEqual => ">=",
        TokenKind::Elvis => "?:",
        TokenKind::KeywordIn => "in",
        TokenKind::KeywordIs => "is",
        TokenKind::KeywordAs(safe) => {
            if *safe {
                "as?"
            } else {
                "as"
            }
        }
        TokenKind::BangEqual | TokenKind::BangEqualEqual => "not=",
        _ => {
            dbg!(kind);
            unreachable!()
        }
    }
}

fn assign_op(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::PlusEqual => "+=",
        TokenKind::MinusEqual => "-=",
        TokenKind::StarEqual => "*=",
        TokenKind::SlashEqual => "/=",
        TokenKind::PercentEqual => "%=",
        _ => {
            dbg!(kind);
            unreachable!()
        }
    }
}

impl<'a> SexpEmitter<'a> {
    pub(crate) fn new(session: &'a Session, _types: &'a Types) -> SexpEmitter<'a> {
        SexpEmitter { session, _types }
    }

    fn assign<W: std::io::Write>(
        &self,
        target: &AstNodeExpr,
        value: &AstNodeExpr,
        op: &TokenKind,
        w: &mut W,
    ) -> Result<(), Error> {
        if *op == TokenKind::Equal {
            write!(w, "(set! ")?;
            self.expr(target, w)?;
            write!(w, " ")?;
            self.expr(value, w)?;
            writeln!(w, ")")?;
        } else {
            write!(w, "(set! ")?;
            self.expr(target, w)?;
            write!(w, " ({} ", assign_op(op))?;
            self.expr(value, w)?;
            writeln!(w, "))")?;
        }

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
                target, value, op, ..
            } => {
                self.assign(target, value, op, w)?;
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
            AstNodeStmt::Class { .. } => todo!(),
        };
        Ok(())
    }

    pub fn statements<W: std::io::Write>(
        &self,
        block: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.statement(&block, w)?;
        writeln!(w)?;
        Ok(())
    }

    fn var_def<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::VarDefinition {
                identifier, value, ..
            } => {
                write!(
                    w,
                    "(define {} ",
                    &self.session.src[identifier.span.start..identifier.span.end]
                )?;
                self.expr(value, w)?;
                writeln!(w, ")")?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn do_while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::DoWhile { cond, body, .. } => {
                write!(w, "(do-while ")?;
                self.statement(body, w)?;
                write!(w, " (if ")?;
                self.expr(cond, w)?;
                writeln!(w, "))")?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::While { cond, body, .. } => {
                write!(w, "(while ")?;
                self.expr(cond, w)?;
                write!(w, " ")?;
                self.statement(body, w)?;
                writeln!(w, ")")?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn literal<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Int(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::UInt(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Long(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::ULong(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Float(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Double(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Boolean(b),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", if *b { "#t" } else { "#f" })?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Char(c),
                    ..
                },
                _,
            ) => {
                write!(w, "'{}'", c)?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::TString,
                    span,
                },
                _,
            ) => {
                write!(w, "{}", &self.session.src[span.start..span.end])?;
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Null,
                    ..
                },
                _,
            ) => {
                write!(w, "'nil")?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn unary<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::Unary {
                token, expr, kind, ..
            } => {
                write!(
                    w,
                    "({}{} ",
                    if *kind == UnaryKind::Postfix {
                        "postfix-"
                    } else {
                        ""
                    },
                    unary_op(&token.kind)
                )?;
                self.expr(expr, w)?;
                write!(w, ")")?;
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
                write!(w, "({} ", binary_op(&op.kind))?;
                self.expr(left, w)?;
                write!(w, " ")?;
                self.expr(right, w)?;
                write!(w, ")")?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn block<W: std::io::Write>(&self, block: &[AstNodeStmt], w: &mut W) -> Result<(), Error> {
        if block.len() != 1 {
            write!(w, "(begin ")?;
        }

        for stmt in block.iter() {
            self.statement(&stmt, w)?;
            write!(w, " ")?;
        }

        if block.len() != 1 {
            write!(w, ")")?;
        }
        Ok(())
    }

    fn fn_call<W: std::io::Write>(
        &self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        w: &mut W,
    ) -> Result<(), Error> {
        write!(w, "(apply ")?;
        self.expr(fn_name, w)?;
        write!(w, " (list ")?;
        for arg in args {
            self.expr(arg, w)?;
            write!(w, " ")?;
        }
        write!(w, "))")?;

        Ok(())
    }

    fn fn_def<W: std::io::Write>(
        &self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        write!(w, "(define (")?;
        self.expr(fn_name, w)?;
        write!(w, " ")?;

        for arg in args {
            self.expr(arg, w)?;
            write!(w, " ")?;
        }
        write!(w, ") ")?;

        self.statement(body, w)?;
        writeln!(w, ")")?;
        Ok(())
    }

    fn when_expr<W: std::io::Write>(&self, ast: &AstNodeExpr, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeExpr::WhenExpr {
                entries,
                else_entry,
                subject: Some(subject),
                ..
            } => {
                write!(w, "(case ")?;
                self.statement(&subject, w)?;
                write!(w, " ")?;

                for entry in entries {
                    self.expr(&entry.cond, w)?;
                    write!(w, " ")?;
                    self.statement(&entry.body, w)?;
                    write!(w, " ")?;
                }

                if let Some(else_entry) = else_entry {
                    write!(w, " 'else ")?;
                    self.statement(&else_entry, w)?;
                }

                write!(w, ")")?;
                Ok(())
            }
            AstNodeExpr::WhenExpr {
                entries,
                else_entry,
                subject: None,
                ..
            } => {
                write!(w, "(cond ")?;

                for entry in entries {
                    write!(w, "(")?;
                    self.expr(&entry.cond, w)?;
                    write!(w, " ")?;
                    self.statement(&entry.body, w)?;
                    write!(w, ") ")?;
                }

                if let Some(else_entry) = else_entry {
                    write!(w, " 'else ")?;
                    self.statement(&else_entry, w)?;
                }

                write!(w, ")")?;
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
                write!(w, "(if ")?;
                self.expr(cond, w)?;
                write!(w, " ")?;
                self.statement(&*if_body, w)?;
                write!(w, " ")?;
                self.statement(&*else_body, w)?;
                write!(w, ")")?;

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn var_ref<W: std::io::Write>(&self, span: &Span, w: &mut W) -> Result<(), Error> {
        let identifier = &self.session.src[span.start..span.end];
        write!(w, "{}", identifier)?;
        Ok(())
    }

    fn jump_expr<W: std::io::Write>(
        &self,
        kind: JumpKind,
        expr: &Option<Box<AstNodeExpr>>,
        w: &mut W,
    ) -> Result<(), Error> {
        match kind {
            JumpKind::Break | JumpKind::Continue => write!(w, "({})", kind).unwrap(),
            JumpKind::Return => {
                write!(w, "(return ")?;
                if let Some(expr) = expr {
                    self.expr(expr, w)?;
                }
                write!(w, ")")?;
            }
            JumpKind::Throw => unimplemented!("Throw expressions"),
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
            write!(w, "(not ")?;
        }
        write!(w, "(in ")?;
        self.expr(range, w)?;
        write!(w, ")")?;

        if !cond {
            write!(w, ")")?;
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
            write!(w, "(not ")?;
        }
        write!(w, "(is ")?;
        self.expr(identifier, w)?;
        write!(w, ")")?;

        if !cond {
            write!(w, ")")?;
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
                write!(w, "(")?;
                self.expr(expr, w)?;
                write!(w, ")")?;
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
            AstNodeExpr::Println(expr, _) => {
                write!(w, "(display ")?;
                self.expr(expr, w)?;
                writeln!(w, ")")?;
                Ok(())
            }
        }
    }
}
