use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;

pub fn gen_js_stmts<W: std::io::Write>(
    statements: &Statements,
    src: &str,
    w: &mut W,
) -> Result<(), Error> {
    for stmt in statements {
        let ast = match &stmt {
            AstNodeStmt::Expr(expr, _) => expr,
        };
        gen_js_expr(&ast, src, w)?;
        writeln!(w, ";").unwrap();
    }
    Ok(())
}

pub fn gen_js_expr<W: std::io::Write>(ast: &AstNode, src: &str, w: &mut W) -> Result<(), Error> {
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
        } => write!(w, "{}", &src[span.start..span.end])
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
            write!(w, "{}", &src[tok.span.start..tok.span.end]).map_err(|err| {
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
            gen_js_expr(right, src, w)
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
            gen_js_expr(left, src, w)?;
            write!(w, "{}", &src[tok.span.start..tok.span.end]).map_err(|err| {
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
            gen_js_expr(right, src, w)
        }
        _ => unimplemented!(),
    }
}
