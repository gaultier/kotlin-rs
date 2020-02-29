use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;
use std::fmt;

impl fmt::Display for AstNodeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstNodeExpr::Literal(Token {
                kind: TokenKind::Int(n),
                ..
            }) => write!(f, "{}", n),
            AstNodeExpr::Unary(
                Token {
                    kind: TokenKind::Minus,
                    ..
                },
                right,
            ) => write!(f, "(-{})", right),
            _ => unimplemented!(),
        }
    }
}
