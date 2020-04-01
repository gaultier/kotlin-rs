use crate::error::*;
use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::parse::*;

pub(crate) struct SexpEmitter<'a> {
    lexer: &'a Lexer,
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
    pub(crate) fn new(lexer: &'a Lexer, _types: &'a Types) -> SexpEmitter<'a> {
        SexpEmitter { lexer, _types }
    }

    fn assign<W: std::io::Write>(
        &self,
        target: &AstNodeExpr,
        value: &AstNodeExpr,
        op: &TokenKind,
        w: &mut W,
    ) -> Result<(), Error> {
        if *op == TokenKind::Equal {
            write!(w, "(set! ").unwrap();
            self.expr(target, w)?;
            write!(w, " ").unwrap();
            self.expr(value, w)?;
            writeln!(w, ")").unwrap();
        } else {
            write!(w, "(set! ").unwrap();
            self.expr(target, w)?;
            write!(w, " ({} ", assign_op(op)).unwrap();
            self.expr(value, w)?;
            writeln!(w, "))").unwrap();
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
            AstNodeStmt::Println(expr) => {
                write!(w, "(display ").unwrap();
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
                identifier, value, ..
            } => {
                write!(
                    w,
                    "(define {} ",
                    &self.lexer.src[identifier.span.start..identifier.span.end]
                )
                .unwrap();
                self.expr(value, w)?;
                writeln!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn do_while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::DoWhile { cond, body, .. } => {
                write!(w, "(loop [] ").unwrap();
                self.statement(body, w)?;
                write!(w, " (if ").unwrap();
                self.expr(cond, w)?;
                writeln!(w, " (recur)))").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::While { cond, body, .. } => {
                write!(w, "(while ").unwrap();
                self.expr(cond, w)?;
                write!(w, " ").unwrap();
                self.statement(body, w)?;
                writeln!(w, ")").unwrap();
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
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::UInt(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Long(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::ULong(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Float(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Double(n),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Boolean(b),
                    ..
                },
                _,
            ) => {
                write!(w, "{}", if *b { "#t" } else { "#f" }).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Char(c),
                    ..
                },
                _,
            ) => {
                write!(w, "'{}'", c).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::TString,
                    span,
                },
                _,
            ) => {
                write!(w, "{}", &self.lexer.src[span.start..span.end]).unwrap();
                Ok(())
            }
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Null,
                    ..
                },
                _,
            ) => {
                write!(w, "'nil").unwrap();
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
                )
                .unwrap();
                self.expr(expr, w)?;
                write!(w, ")").unwrap();
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
                write!(w, "({} ", binary_op(&op.kind)).unwrap();
                self.expr(left, w)?;
                write!(w, " ").unwrap();
                self.expr(right, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn block<W: std::io::Write>(&self, block: &[AstNodeStmt], w: &mut W) -> Result<(), Error> {
        if block.len() != 1 {
            write!(w, "(begin ").unwrap();
        }

        for stmt in block.iter() {
            self.statement(&stmt, w)?;
            write!(w, " ").unwrap();
        }

        if block.len() != 1 {
            write!(w, ")").unwrap();
        }
        Ok(())
    }

    fn fn_call<W: std::io::Write>(
        &self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        w: &mut W,
    ) -> Result<(), Error> {
        write!(w, "(apply ").unwrap();
        self.expr(fn_name, w)?;
        write!(w, " (list ").unwrap();
        for arg in args {
            self.expr(arg, w)?;
            write!(w, " ").unwrap();
        }
        write!(w, "))").unwrap();

        Ok(())
    }

    fn fn_def<W: std::io::Write>(
        &self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        write!(w, "(define (").unwrap();
        self.expr(fn_name, w)?;
        write!(w, " ").unwrap();

        for arg in args {
            self.expr(arg, w)?;
            write!(w, " ").unwrap();
        }
        write!(w, ") ").unwrap();

        self.statement(body, w)?;
        writeln!(w, ")").unwrap();
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
                write!(w, "(case ").unwrap();
                self.statement(&subject, w)?;
                write!(w, " ").unwrap();

                for entry in entries {
                    self.expr(&entry.cond, w)?;
                    write!(w, " ").unwrap();
                    self.statement(&entry.body, w)?;
                    write!(w, " ").unwrap();
                }

                if let Some(else_entry) = else_entry {
                    write!(w, " :else ").unwrap();
                    self.statement(&else_entry, w)?;
                }

                write!(w, ")").unwrap();
                Ok(())
            }
            AstNodeExpr::WhenExpr {
                entries,
                else_entry,
                subject: None,
                ..
            } => {
                write!(w, "(cond ").unwrap();

                for entry in entries {
                    write!(w, "(").unwrap();
                    self.expr(&entry.cond, w)?;
                    write!(w, " ").unwrap();
                    self.statement(&entry.body, w)?;
                    write!(w, ") ").unwrap();
                }

                if let Some(else_entry) = else_entry {
                    write!(w, " :else ").unwrap();
                    self.statement(&else_entry, w)?;
                }

                write!(w, ")").unwrap();
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
                write!(w, "(if ").unwrap();
                self.expr(cond, w)?;
                write!(w, " ").unwrap();
                self.statement(&*if_body, w)?;
                write!(w, " ").unwrap();
                self.statement(&*else_body, w)?;
                write!(w, ")").unwrap();

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
        kind: &JumpKind,
        expr: &Option<Box<AstNodeExpr>>,
        w: &mut W,
    ) -> Result<(), Error> {
        match kind {
            JumpKind::Break | JumpKind::Continue => write!(w, "({})", kind).unwrap(),
            JumpKind::Return => {
                write!(w, "(return ").unwrap();
                if let Some(expr) = expr {
                    self.expr(expr, w)?;
                }
                write!(w, ")").unwrap();
            }
            JumpKind::Throw => unimplemented!(),
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
            AstNodeExpr::Jump { kind, expr, .. } => self.jump_expr(kind, expr, w),
        }
    }
}
