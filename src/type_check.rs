use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Long,
    Void,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Long => write!(f, "Long"),
            Type::Void => write!(f, "Void"),
        }
    }
}

impl Type {
    fn coalesce(&self, other: &Type) -> Result<Type, String> {
        match (self, other) {
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Bool, _) | (_, Type::Bool) => {
                Err(format!("Incompatible types: {} and {}", self, other))
            }
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Long, Type::Long) | (Type::Long, Type::Int) | (Type::Int, Type::Long) => {
                Ok(Type::Long)
            }
            (_, Type::Void) | (Type::Void, _) => Err("Incompatible types".to_string()),
        }
    }
}

pub fn type_check(ast: &AstNodeExpr, src: &str) -> Result<Type, String> {
    match ast {
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Int(_),
            ..
        }) => Ok(Type::Int),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Bool(_),
            ..
        }) => Ok(Type::Bool),
        AstNodeExpr::Unary(_, right) => type_check(right, src),
        AstNodeExpr::Binary(left, _, right) => {
            type_check(left, src)?.coalesce(&type_check(right, src)?)
        }
        _ => unimplemented!(),
    }
}
