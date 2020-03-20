use crate::error::*;
use crate::lex::{Lexer, Token, TokenKind};
use crate::parse::*;

pub(crate) struct JsEmitter<'a> {
    lexer: &'a Lexer,
}

impl JsEmitter<'_> {
    pub(crate) fn new(lexer: &Lexer) -> JsEmitter {
        JsEmitter { lexer }
    }

    pub fn stmts<W: std::io::Write>(
        &self,
        statements: &[AstNodeStmt],
        w: &mut W,
    ) -> Result<(), Error> {
        for stmt in statements {
            let ast = match &stmt {
                AstNodeStmt::Expr(expr, _) => expr,
            };
            self.expr(&ast, w)?;
            writeln!(w, ";").unwrap();
        }
        Ok(())
    }

    fn literal<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Int(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n).map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    self.lexer.span_location(&span),
                )
            }),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::UInt(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Long(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::ULong(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Float(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Double(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Bool(n),
                        span,
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::TString,
                        span,
                    }),
                ..
            } => write!(w, "{}", &self.lexer.src[span.start..span.end])
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Null,
                        span,
                    }),
                ..
            } => write!(w, "null")
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })
                .map(|_| ()),
            _ => unreachable!(),
        }
    }

    fn unary<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind: AstNodeExpr::Unary(tok, right),
                ..
            } => {
                write!(w, "{}", &self.lexer.src[tok.span.start..tok.span.end]).map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&tok.span),
                    )
                })?;
                self.expr(right, w)
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
                            span,
                        },
                        right,
                    ),
                ..
            } => {
                self.expr(left, w)?;
                write!(w, "!==").map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })?;
                self.expr(right, w)
            }
            AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        Token {
                            kind: TokenKind::EqualEqual,
                            span,
                        },
                        right,
                    ),
                ..
            } => {
                self.expr(left, w)?;
                write!(w, "===").map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&span),
                    )
                })?;
                self.expr(right, w)
            }
            AstNode {
                kind: AstNodeExpr::Binary(left, tok, right),
                ..
            } => {
                if let (Some(Type::Null), Some(Type::Null)) = (left.type_info, right.type_info) {
                    if ast.type_info == Some(Type::TString) {
                        write!(w, "\"\"+").map_err(|err| {
                            Error::new(
                                ErrorKind::EmitError(err.to_string()),
                                self.lexer.span_location(&tok.span),
                            )
                        })?;
                    }
                }

                self.expr(left, w)?;
                write!(w, "{}", &self.lexer.src[tok.span.start..tok.span.end]).map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        self.lexer.span_location(&tok.span),
                    )
                })?;
                self.expr(right, w)
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
                write!(w, "((").unwrap();
                self.expr(cond, w)?;
                write!(w, ")?(").unwrap();
                self.expr(if_body, w)?;
                write!(w, "):(").unwrap();
                self.expr(else_body, w)?;
                write!(w, "))").unwrap();

                Ok(())
            }
            _ => unreachable!(),
        }
    }

    pub fn expr<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
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
