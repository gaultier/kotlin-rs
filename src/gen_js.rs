use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;
use std::io;

pub fn gen_js<W: io::Write>(ast: &AstNodeExpr, src: &str, w: &mut W) -> Result<(), Error> {
    match ast {
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Int(n),
            location,
        }) => w
            .write(format!("{}", n).as_bytes())
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
            kind: TokenKind::UInt(n),
            location,
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
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
        }) => w
            .write(format!("{}", n).as_bytes())
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
        }) => w
            .write(format!("{}", n).as_bytes())
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
        }) => w
            .write(format!("{}", n).as_bytes())
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
        }) => w
            .write(&src[location.start_pos..location.end_pos].as_bytes())
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
            w.write("-".as_bytes()).map_err(|err| {
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
