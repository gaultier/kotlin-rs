use crate::error::*;
use crate::lex::{Lexer, Token, TokenKind};
use crate::parse::*;

pub(crate) struct SexpEmitter<'a> {
    lexer: &'a Lexer,
}

impl SexpEmitter<'_> {
    pub(crate) fn new(lexer: &Lexer) -> SexpEmitter {
        SexpEmitter { lexer }
    }

    pub fn stmts<W: std::io::Write>(
        &self,
        statements: &[AstNodeStmt],
        w: &mut W,
    ) -> Result<(), Error> {
        for stmt in statements {
            let ast = match &stmt {
                AstNodeStmt::Expr(expr) => expr,
            };
            self.expr(&ast, w)?;
            writeln!(w).unwrap();
        }
        Ok(())
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
                write!(w, "{}", if *b { "#t" } else { "#f" }).unwrap();
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
                write!(w, "'nil").unwrap();
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
                write!(w, "({} ", &self.lexer.src[tok.span.start..tok.span.end]).unwrap();
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
                kind:
                    AstNodeExpr::Binary(
                        _,
                        Token {
                            kind: TokenKind::BangEqualEqual,
                            ..
                        },
                        _,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        _,
                        Token {
                            kind: TokenKind::EqualEqualEqual,
                            ..
                        },
                        _,
                    ),
                ..
            } => unimplemented!(),
            AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        Token {
                            kind: TokenKind::BangEqual,
                            ..
                        },
                        right,
                    ),
                ..
            } => {
                write!(w, "(not (= ").unwrap();
                self.expr(left, w)?;
                write!(w, " ").unwrap();
                self.expr(right, w)?;
                write!(w, "))").unwrap();
                Ok(())
            }
            AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        Token {
                            kind: TokenKind::EqualEqual,
                            ..
                        },
                        right,
                    ),
                ..
            } => {
                write!(w, "(= ").unwrap();
                self.expr(left, w)?;
                write!(w, " ").unwrap();
                self.expr(right, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            AstNode {
                kind: AstNodeExpr::Binary(left, tok, right),
                ..
            } => {
                write!(w, "({} ", &self.lexer.src[tok.span.start..tok.span.end]).unwrap();
                self.expr(left, w)?;
                write!(w, " ").unwrap();
                self.expr(right, w)?;
                write!(w, ")").unwrap();
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn statement<W: std::io::Write>(&self, ast: &AstNodeStmt, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNodeStmt::Expr(expr) => self.expr(expr, w),
        }
    }

    fn block<W: std::io::Write>(&self, ast: &Statements, w: &mut W) -> Result<(), Error> {
        if ast.len() != 1 {
            write!(w, "(begin ").unwrap();
        }

        // Akin to ast.map(...).join(" ")
        if let Some((last, rest)) = ast.split_last() {
            for stmt in rest {
                self.statement(stmt, w)?;
                write!(w, " ").unwrap();
            }
            self.statement(last, w)?;
        }

        if ast.len() != 1 {
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
                        ..
                    },
                ..
            } => {
                write!(w, "(cond ").unwrap();
                for entry in entries {
                    write!(w, "(").unwrap();
                    self.expr(&entry.cond, w)?;
                    write!(w, " ").unwrap();
                    self.block(&entry.body, w)?;
                    write!(w, ") ").unwrap();
                }

                if let Some(else_entry) = else_entry {
                    write!(w, "(else ").unwrap();
                    self.block(&else_entry, w)?;
                    write!(w, ")").unwrap();
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
        }
    }
}
