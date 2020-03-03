use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::AstNodeExpr;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    UInt,
    Long,
    ULong,
    Float,
    Double,
    Object,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::UInt => write!(f, "UInt"),
            Type::Long => write!(f, "Long"),
            Type::ULong => write!(f, "ULong"),
            Type::Float => write!(f, "Float"),
            Type::Double => write!(f, "Double"),
            Type::Object => write!(f, "Object"),
        }
    }
}

impl Type {
    fn coalesce(left: Type, right: Type, location: &Location) -> Result<Type, Error> {
        match (left, right) {
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::UInt, Type::UInt) => Ok(Type::UInt),
            (Type::Long, Type::Long) | (Type::Long, Type::Int) | (Type::Int, Type::Long) => {
                Ok(Type::Long)
            }
            (Type::UInt, Type::ULong) | (Type::ULong, Type::UInt) | (Type::ULong, Type::ULong) => {
                Ok(Type::ULong)
            }
            (Type::Float, Type::Float)
            | (Type::Int, Type::Float)
            | (Type::Float, Type::Int)
            | (Type::Float, Type::Long)
            | (Type::Long, Type::Float) => Ok(Type::Float),
            (Type::Double, Type::Double)
            | (Type::Int, Type::Double)
            | (Type::Double, Type::Int)
            | (Type::Double, Type::Long)
            | (Type::Long, Type::Double)
            | (Type::Double, Type::Float)
            | (Type::Float, Type::Double) => Ok(Type::Double),
            _ => Err(Error::new(
                ErrorKind::IncompatibleTypes(left, right),
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
            kind: TokenKind::UInt(_),
            ..
        }) => Ok(Type::UInt),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Long(_),
            ..
        }) => Ok(Type::Long),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::ULong(_),
            ..
        }) => Ok(Type::ULong),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Float(_),
            ..
        }) => Ok(Type::Float),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Double(_),
            ..
        }) => Ok(Type::Double),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Null,
            ..
        }) => Ok(Type::Object),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Bool(_),
            ..
        }) => Ok(Type::Bool),
        AstNodeExpr::Unary(_, right) => type_check(right, src),
        AstNodeExpr::Binary(left, tok, right) => Type::coalesce(
            type_check(left, src)?,
            type_check(right, src)?,
            &tok.location,
        ),
        _ => unimplemented!(),
    }
}
