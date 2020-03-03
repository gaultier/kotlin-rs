use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;
use std::io;

pub fn gen_js<W: io::Write>(ast: &AstNodeExpr, src: &str, w: &mut W) -> Result<usize, String> {
    match ast {
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Int(n),
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::UInt(n),
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Long(n),
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::ULong(n),
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Bool(n),
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::TString,
            location,
            ..
        }) => w
            .write(&src[location.start_pos..location.end_pos].as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Unary(tok, right) => {
            write!(w, "{}", tok.to_owned(src)).map_err(|err| err.to_string())?;
            w.write("-".as_bytes()).map_err(|err| err.to_string())?;
            gen_js(right, src, w)
        }
        AstNodeExpr::Binary(left, tok, right) => {
            gen_js(left, src, w)?;
            write!(w, "{}", tok.to_owned(src)).map_err(|err| err.to_string())?;
            gen_js(right, src, w)
        }
        _ => unimplemented!(),
    }
}
