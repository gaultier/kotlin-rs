use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;
use std::io;

pub fn gen_js<W: io::Write>(ast: &AstNodeExpr, w: &mut W) -> Result<usize, String> {
    match ast {
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Int(n),
            ..
        }) => w
            .write(format!("{}", n).as_bytes())
            .map_err(|err| err.to_string()),
        AstNodeExpr::Unary(
            Token {
                kind: TokenKind::Minus,
                ..
            },
            right,
        ) => {
            w.write("-".as_bytes()).map_err(|err| err.to_string())?;
            gen_js(right, w)
        }
        AstNodeExpr::Binary(
            left,
            Token {
                kind: TokenKind::Plus,
                ..
            },
            right,
        ) => {
            gen_js(left, w)?;
            w.write("+".as_bytes()).map_err(|err| err.to_string())?;
            gen_js(right, w)
        }
        _ => unimplemented!(),
    }
}
