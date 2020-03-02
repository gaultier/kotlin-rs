use crate::error::*;
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
    fn coalesce(&self, other: &Type, location: &Location) -> Result<Type, Error> {
        match (self, other) {
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Bool, _) | (_, Type::Bool) => Err(Error::new(
                ErrorKind::IncompatibleTypes(self.clone(), other.clone()),
                location.start_pos,
                location.start_line,
                location.start_column,
                location.end_pos,
                location.end_line,
                location.end_column,
            )),
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Long, Type::Long) | (Type::Long, Type::Int) | (Type::Int, Type::Long) => {
                Ok(Type::Long)
            }
            (_, Type::Void) | (Type::Void, _) => Err(Error::new(
                ErrorKind::IncompatibleTypes(self.clone(), other.clone()),
                location.start_pos,
                location.start_line,
                location.start_column,
                location.end_pos,
                location.end_line,
                location.end_column,
            )),
        }
    }
}

pub fn type_check(ast: &AstNodeExpr, src: &str) -> Result<Type, Error> {
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
        AstNodeExpr::Binary(left, tok, right) => {
            type_check(left, src)?.coalesce(&type_check(right, src)?, &tok.location)
        }
        _ => unimplemented!(),
    }
}
