use crate::error::*;
use crate::lex::{Lexer, Token, TokenKind};
use crate::parse::*;

pub(crate) struct TypeChecker<'a> {
    lexer: &'a Lexer,
}

impl TypeChecker<'_> {
    pub fn new(lexer: &Lexer) -> TypeChecker {
        TypeChecker { lexer }
    }

    pub fn type_check_stmts(&self, statements: &mut Statements) -> Result<(), Error> {
        for mut stmt in statements {
            let mut ast = match &mut stmt {
                AstNodeStmt::Expr(expr, _) => expr,
            };
            self.type_check_expr(&mut ast).map(|_| ())?;
        }
        Ok(())
    }

    pub fn type_check_expr(&self, ast: &mut AstNode) -> Result<Type, Error> {
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
                let t = self.type_check_expr(right)?;
                ast.type_info = Some(t);
                Ok(t)
            }
            AstNode {
                kind: AstNodeExpr::Binary(left, tok, right),
                ..
            } => {
                let t = self.coalesce_types(
                    self.type_check_expr(left)?,
                    self.type_check_expr(right)?,
                    &tok,
                )?;
                ast.type_info = Some(t);
                Ok(t)
            }
            _ => unimplemented!(),
        }
    }

    fn coalesce_types(&self, left: Type, right: Type, token: &Token) -> Result<Type, Error> {
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
                self.lexer.span_location(&token.span))), // FIXME
        }
    }
}
