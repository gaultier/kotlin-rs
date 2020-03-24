use crate::error::*;
use crate::lex::{Lexer, Token, TokenKind};
use crate::parse::*;

pub(crate) struct SexpEmitter<'a> {
    lexer: &'a Lexer,
}

fn unary_op(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::Bang => "not",
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
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
        TokenKind::EqualEqual | TokenKind::EqualEqualEqual => "=",
        TokenKind::PipePipe => "or",
        TokenKind::AmpersandAmpersand => "and",
        TokenKind::Lesser => "<",
        TokenKind::LesserEqual => "<=",
        TokenKind::Greater => ">",
        TokenKind::GreaterEqual => ">=",
        TokenKind::BangEqual | TokenKind::BangEqualEqual => "not=",
        _ => {
            dbg!(kind);
            unreachable!()
        }
    }
}

impl SexpEmitter<'_> {
    pub(crate) fn new(lexer: &Lexer) -> SexpEmitter {
        SexpEmitter { lexer }
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
        };
        Ok(())
    }

    pub fn statements<W: std::io::Write>(&self, block: &Block, w: &mut W) -> Result<(), Error> {
        for stmt in &block.body {
            self.statement(&stmt, w)?;
        }
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
                    "(def {} ",
                    &self.lexer.src[identifier.span.start..identifier.span.end]
                )
                .unwrap();
                self.expr(value, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn do_while_stmt<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::DoWhile { cond, body, .. } => {
                write!(w, "(loop [] ").unwrap();
                self.block(body, w)?;
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
                self.block(body, w)?;
                writeln!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn literal<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Int(n),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::UInt(n),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Long(n),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::ULong(n),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Float(n),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Double(n),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", n).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Bool(b),
                        ..
                    }),
                ..
            } => {
                write!(w, "{}", if *b { "true" } else { "false" }).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Char(c),
                        ..
                    }),
                ..
            } => {
                write!(w, "'{}'", c).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::TString,
                        span,
                    }),
                ..
            } => {
                write!(w, "{}", &self.lexer.src[span.start..span.end]).unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Null,
                        ..
                    }),
                ..
            } => {
                write!(w, "nil").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn unary<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind: AstNodeExpr::Unary(tok, right),
                ..
            } => {
                write!(w, "({} ", unary_op(&tok.kind)).unwrap();
                self.expr(right, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn binary<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind: AstNodeExpr::Binary(left, tok, right),
                ..
            } => {
                write!(w, "({} ", binary_op(&tok.kind)).unwrap();
                self.expr(left, w)?;
                write!(w, " ").unwrap();
                self.expr(right, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn block<W: std::io::Write>(&self, block: &Block, w: &mut W) -> Result<(), Error> {
        if block.body.len() != 1 {
            write!(w, "(do ").unwrap();
        }

        // Akin to ast.map(...).join(" ")
        if let Some((last, rest)) = block.body.split_last() {
            for stmt in rest {
                self.statement(stmt, w)?;
                write!(w, " ").unwrap();
            }
            self.statement(last, w)?;
        }

        if block.body.len() != 1 {
            write!(w, ")").unwrap();
        }
        Ok(())
    }

    fn when_expr<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind:
                    AstNodeExpr::WhenExpr {
                        entries,
                        else_entry,
                        subject: Some(subject),
                    },
                ..
            } => {
                write!(w, "(case ").unwrap();
                self.expr(&subject, w)?;
                write!(w, " ").unwrap();

                if let Some((last, entries)) = entries.split_last() {
                    for entry in entries {
                        write!(w, "((").unwrap();
                        self.expr(&entry.cond, w)?;
                        write!(w, ") ").unwrap();
                        self.block(&entry.body, w)?;
                        write!(w, ") ").unwrap();
                    }
                    write!(w, "((").unwrap();
                    self.expr(&last.cond, w)?;
                    write!(w, ") ").unwrap();
                    self.block(&last.body, w)?;
                    write!(w, ")").unwrap();
                }

                if let Some(else_entry) = else_entry {
                    write!(w, " :else ").unwrap();
                    self.block(&else_entry, w)?;
                }

                write!(w, ")").unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::WhenExpr {
                        entries,
                        else_entry,
                        subject: None,
                    },
                ..
            } => {
                write!(w, "(cond ").unwrap();

                if let Some((last, entries)) = entries.split_last() {
                    for entry in entries {
                        write!(w, "(").unwrap();
                        self.expr(&entry.cond, w)?;
                        write!(w, " ").unwrap();
                        self.block(&entry.body, w)?;
                        write!(w, ") ").unwrap();
                    }
                    write!(w, "(").unwrap();
                    self.expr(&last.cond, w)?;
                    write!(w, " ").unwrap();
                    self.block(&last.body, w)?;
                    write!(w, ")").unwrap();
                }

                if let Some(else_entry) = else_entry {
                    write!(w, " :else ").unwrap();
                    self.block(&else_entry, w)?;
                }

                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn if_expr<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind:
                    AstNodeExpr::IfExpr {
                        cond,
                        if_body,
                        else_body,
                        ..
                    },
                ..
            } => {
                write!(w, "(if ").unwrap();
                self.expr(cond, w)?;
                write!(w, " ").unwrap();
                self.block(if_body, w)?;
                write!(w, " ").unwrap();
                self.block(else_body, w)?;
                write!(w, ")").unwrap();

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    pub fn expr<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind: AstNodeExpr::WhenExpr { .. },
                ..
            } => self.when_expr(ast, w),
            AstNode {
                kind: AstNodeExpr::Literal(..),
                ..
            } => self.literal(ast, w),
            AstNode {
                kind: AstNodeExpr::Unary(..),
                ..
            } => self.unary(ast, w),
            AstNode {
                kind: AstNodeExpr::Binary(..),
                ..
            } => self.binary(ast, w),
            AstNode {
                kind: AstNodeExpr::Grouping(expr),
                ..
            } => {
                write!(w, "(").unwrap();
                self.expr(expr, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            AstNode {
                kind: AstNodeExpr::IfExpr { .. },
                ..
            } => self.if_expr(ast, w),
            AstNode {
                kind: AstNodeExpr::VarRef(_),
                ..
            } => unimplemented!(),
        }
    }
}
