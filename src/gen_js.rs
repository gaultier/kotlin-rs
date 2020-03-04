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
    }
    Ok(())
}

pub fn gen_js_expr<W: std::io::Write>(ast: &AstNode, src: &str, w: &mut W) -> Result<(), Error> {
    match ast {
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Int(n),
                    location,
                }),
            ..
        } => write!(w, "{}", n).map_err(|err| {
            Error::new(
                ErrorKind::EmitError(err.to_string()),
                location.start_pos,
                location.start_line,
                location.start_column,
                location.end_pos,
                location.end_line,
                location.end_column,
            )
        }),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::UInt(n),
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", n)
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Long(n),
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", n)
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::ULong(n),
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", n)
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Float(n),
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", n)
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Double(n),
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", n)
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Bool(n),
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", n)
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::TString,
                    location,
                    ..
                }),
            ..
        } => write!(w, "{}", &src[location.start_pos..location.end_pos])
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Null,
                    location,
                    ..
                }),
            ..
        } => write!(w, "null")
            .map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    location.start_pos,
                    location.start_line,
                    location.start_column,
                    location.end_pos,
                    location.end_line,
                    location.end_column,
                )
            })
            .map(|_| ()),
        AstNode {
            kind: AstNodeExpr::Unary(tok, right),
            ..
        } => {
            write!(w, "{}", &src[tok.location.start_pos..tok.location.end_pos]).map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    tok.location.start_pos,
                    tok.location.start_line,
                    tok.location.start_column,
                    tok.location.end_pos,
                    tok.location.end_line,
                    tok.location.end_column,
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
                                tok.location.start_pos,
                                tok.location.start_line,
                                tok.location.start_column,
                                tok.location.end_pos,
                                tok.location.end_line,
                                tok.location.end_column,
                            )
                        })?;
                    }
                }
                _ => {}
            }
            gen_js_expr(left, src, w)?;
            write!(w, "{}", tok.to_owned(src)).map_err(|err| {
                Error::new(
                    ErrorKind::EmitError(err.to_string()),
                    tok.location.start_pos,
                    tok.location.start_line,
                    tok.location.start_column,
                    tok.location.end_pos,
                    tok.location.end_line,
                    tok.location.end_column,
                )
            })?;
            gen_js_expr(right, src, w)
        }
        _ => unimplemented!(),
    }
}
