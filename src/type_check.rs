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
    Null,
    TString,
    Char,
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
            Type::Null => write!(f, "Null"),
            Type::Char => write!(f, "Char"),
            Type::TString => write!(f, "String"),
        }
    }
}

impl Type {
    fn coalesce(left: Type, right: Type, token: &Token) -> Result<Type, Error> {
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
            (Type::TString, Type::Null)
            | (Type::Null, Type::TString)
            | (Type::Null, Type::Null)
            | (Type::TString, Type::TString)
            | (Type::TString, Type::Int)
            | (Type::Int, Type::TString)
            | (Type::TString, Type::UInt)
            | (Type::UInt, Type::TString)
            | (Type::TString, Type::Long)
            | (Type::Long, Type::TString)
            | (Type::TString, Type::ULong)
            | (Type::ULong, Type::TString)
            | (Type::TString, Type::Char)
            | (Type::Char, Type::TString)
            // Asymetrical
            | (Type::TString, Type::Bool)
                if token.kind == TokenKind::Plus =>
            {
                Ok(Type::TString)
            }
            // Asymetrical
            (Type::Char, Type::Int) if token.kind == TokenKind::Plus => Ok(Type::Char),
            // Asymetrical
            (Type::Char, Type::Int) if token.kind == TokenKind::Minus => Ok(Type::Char),
            _ => Err(Error::new(
                ErrorKind::IncompatibleTypes(left, right),
                token.location.start_pos,
                token.location.start_line,
                token.location.start_column,
                token.location.end_pos,
                token.location.end_line,
                token.location.end_column,
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
        }) => Ok(Type::Null),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::Bool(_),
            ..
        }) => Ok(Type::Bool),
        AstNodeExpr::Literal(Token {
            kind: TokenKind::TString,
            ..
        }) => Ok(Type::TString),
        AstNodeExpr::Unary(_, right) => type_check(right, src),
        AstNodeExpr::Binary(left, tok, right) => {
            Type::coalesce(type_check(left, src)?, type_check(right, src)?, &tok)
        }
        _ => unimplemented!(),
    }
}
