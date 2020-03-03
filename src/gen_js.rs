use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;

pub fn gen_js<W: std::io::Write>(ast: &AstNodeExpr, src: &str, w: &mut W) -> Result<(), Error> {
    match ast {
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Int(n),
            location,
        }) => write!(w, "{}", n).map_err(|err| {
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::UInt(n),
            location,
            ..
        }) => write!(w, "{}", n)
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Long(n),
            location,
            ..
        }) => write!(w, "{}", n)
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::ULong(n),
            location,
            ..
        }) => write!(w, "{}", n)
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Float(n),
            location,
            ..
        }) => write!(w, "{}", n)
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Double(n),
            location,
            ..
        }) => write!(w, "{}", n)
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Bool(n),
            location,
            ..
        }) => write!(w, "{}", n)
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::TString,
            location,
            ..
        }) => write!(w, "{}", &src[location.start_pos..location.end_pos])
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
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Null,
            location,
            ..
        }) => write!(w, "null")
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
        AstNodeExpr::Unary(tok, right) => {
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
            gen_js(right, src, w)
        }
        AstNodeExpr::Binary(left, tok, right) => {
            gen_js(left, src, w)?;
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
            gen_js(right, src, w)
        }
        _ => unimplemented!(),
    }
}
