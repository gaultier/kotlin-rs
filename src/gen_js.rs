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
        statements: &Statements,
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

    pub fn expr<W: std::io::Write>(&self, ast: &AstNode, w: &mut W) -> Result<(), Error> {
        match ast {
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Int(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n).map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    Location {
                        start_pos: 0,
                        start_line: 0,
                        start_column: 0,
                        end_pos: 0,
                        end_line: 0,
                        end_column: 0,
                    },
                )
            }),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::UInt(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Long(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::ULong(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Float(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Double(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Bool(n),
                        ..
                    }),
                ..
            } => write!(w, "{}", n)
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
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
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Null,
                        ..
                    }),
                ..
            } => write!(w, "null")
                .map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })
                .map(|_| ()),
            AstNode {
                kind: AstNodeExpr::Unary(tok, right),
                ..
            } => {
                write!(w, "{}", &self.lexer.src[tok.span.start..tok.span.end]).map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })?;
                self.expr(right, w)
            }
            AstNode {
                kind: AstNodeExpr::Binary(left, tok, right),
                ..
            } => {
                match (left.type_info, right.type_info) {
                    (Some(Type::Null), Some(Type::Null)) => {
                        if ast.type_info == Some(Type::TString) {
                            write!(w, "\"\"+").map_err(|err| {
                                Error::new(
                                    ErrorKind::EmitError(err.to_string()),
                                    Location {
                                        start_pos: 0,
                                        start_line: 0,
                                        start_column: 0,
                                        end_pos: 0,
                                        end_line: 0,
                                        end_column: 0,
                                    },
                                )
                            })?;
                        }
                    }
                    _ => {}
                }
                self.expr(left, w)?;
                write!(w, "{}", &self.lexer.src[tok.span.start..tok.span.end]).map_err(|err| {
                    Error::new(
                        ErrorKind::EmitError(err.to_string()),
                        Location {
                            start_pos: 0,
                            start_line: 0,
                            start_column: 0,
                            end_pos: 0,
                            end_line: 0,
                            end_column: 0,
                        },
                    )
                })?;
                self.expr(right, w)
            }
            _ => unimplemented!(),
        }
    }
}
