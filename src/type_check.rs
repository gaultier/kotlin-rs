use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;

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
            | (Type::TString, Type::Float)
            | (Type::Float, Type::TString)
            | (Type::TString, Type::Double)
            | (Type::Double, Type::TString)
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

pub fn type_check(ast: &mut AstNode, src: &str) -> Result<Type, Error> {
    match ast {
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Int(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::Int);
            Ok(Type::Int)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::UInt(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::UInt);
            Ok(Type::UInt)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Long(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::Long);
            Ok(Type::Long)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::ULong(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::ULong);
            Ok(Type::ULong)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Float(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::Float);
            Ok(Type::Float)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Double(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::Double);
            Ok(Type::Double)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Null,
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::Null);
            Ok(Type::Null)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::Bool(_),
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::Bool);
            Ok(Type::Bool)
        }
        AstNode {
            kind:
                AstNodeExpr::Literal(Token {
                    kind: TokenKind::TString,
                    ..
                }),
            ..
        } => {
            ast.type_info = Some(Type::TString);
            Ok(Type::TString)
        }
        AstNode {
            kind: AstNodeExpr::Unary(_, right),
            ..
        } => {
            let t = type_check(right, src)?;
            ast.type_info = Some(t);
            Ok(t)
        }
        AstNode {
            kind: AstNodeExpr::Binary(left, tok, right),
            ..
        } => {
            let t = Type::coalesce(type_check(left, src)?, type_check(right, src)?, &tok)?;
            ast.type_info = Some(t);
            Ok(t)
        }
        _ => unimplemented!(),
    }
}
