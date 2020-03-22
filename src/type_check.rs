use crate::error::*;
use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::parse::*;

pub(crate) struct TypeChecker<'a> {
    lexer: &'a Lexer,
}

impl TypeChecker<'_> {
    pub fn new(lexer: &Lexer) -> TypeChecker {
        TypeChecker { lexer }
    }

    fn eq(&self, left: Type, right: Type, span: &Span) -> Result<(), Error> {
        if left != right {
            Err(Error::new(
                ErrorKind::IncompatibleTypes(left, right),
                self.lexer.span_location(&span),
            ))
        } else {
            Ok(())
        }
    }

    fn while_stmt(
        &self,
        cond: &mut AstNode,
        body: &mut [AstNodeStmt],
        cond_start_tok: &Token,
    ) -> Result<Type, Error> {
        self.eq(
            self.type_check_expr(cond)?,
            Type::Bool,
            &cond_start_tok.span,
        )?;
        self.statements(body)?;
        Ok(Type::Unit)
    }

    fn statement(&self, statement: &mut AstNodeStmt) -> Result<Type, Error> {
        match statement {
            AstNodeStmt::Expr(expr) => self.type_check_expr(expr),
            AstNodeStmt::While {
                cond,
                cond_start_tok,
                body,
            } => self.while_stmt(cond, body, &cond_start_tok),
        }
    }

    pub fn statements(&self, statements: &mut [AstNodeStmt]) -> Result<Type, Error> {
        for stmt in statements.iter_mut() {
            self.statement(stmt)?;
        }

        Ok(statements
            .last()
            .map(|stmt| match stmt {
                AstNodeStmt::Expr(expr) => expr.type_info.unwrap_or(Type::Unit),
                _ => Type::Unit,
            })
            .unwrap_or(Type::Unit))
    }

    fn type_check_unary(&self, ast: &mut AstNode) -> Result<Type, Error> {
        match ast {
            AstNode {
                kind:
                    AstNodeExpr::Unary(
                        Token {
                            kind: TokenKind::Minus,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Unary(
                        Token {
                            kind: TokenKind::Plus,
                            ..
                        },
                        right,
                    ),
                ..
            } => {
                let t = self.type_check_expr(right)?;
                ast.type_info = Some(t);
                Ok(t)
            }
            AstNode {
                kind:
                    AstNodeExpr::Unary(
                        tok
                        @
                        Token {
                            kind: TokenKind::Bang,
                            ..
                        },
                        right,
                    ),
                ..
            } => {
                let t = self.type_check_expr(right)?;
                ast.type_info = Some(self.coalesce_types(Type::Bool, t, tok)?);
                Ok(Type::Bool)
            }
            _ => unreachable!(),
        }
    }

    fn type_check_literal(&self, ast: &mut AstNode) -> Result<Type, Error> {
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
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Char(_),
                        ..
                    }),
                ..
            } => {
                ast.type_info = Some(Type::Char);
                Ok(Type::Char)
            }
            _ => unreachable!(),
        }
    }

    fn type_check_binary(&self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = ast.type_info {
            return Ok(t);
        }

        match ast {
            AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::BangEqual,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::EqualEqual,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::BangEqualEqual,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::EqualEqualEqual,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::DotDot,
                            ..
                        },
                        right,
                    ),
                ..
            } => {
                let left_t = self.type_check_expr(left)?;
                let right_t = self.type_check_expr(right)?;

                self.eq(left_t, right_t, &tok.span)?;
                let t = match left_t {
                    Type::Int => Type::IntRange,
                    Type::UInt => Type::UIntRange,
                    Type::Long => Type::LongRange,
                    Type::ULong => Type::ULongRange,
                    Type::Float => Type::FloatRange,
                    Type::Double => Type::DoubleRange,
                    Type::TString => Type::TStringRange,
                    Type::Char => Type::CharRange,
                    Type::Bool => Type::BoolRange,
                    _ => {
                        return Err(Error::new(
                            ErrorKind::InvalidRange(left_t),
                            self.lexer.span_location(&tok.span),
                        ));
                    }
                };

                ast.type_info = Some(t);
                Ok(t)
            }
            AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::Lesser,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::LesserEqual,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::Greater,
                            ..
                        },
                        right,
                    ),
                ..
            }
            | AstNode {
                kind:
                    AstNodeExpr::Binary(
                        left,
                        tok
                        @
                        Token {
                            kind: TokenKind::GreaterEqual,
                            ..
                        },
                        right,
                    ),
                ..
            } => {
                let left_t = self.type_check_expr(left)?;
                let right_t = self.type_check_expr(right)?;

                self.coalesce_types(left_t, right_t, &tok)?;
                ast.type_info = Some(Type::Bool);
                Ok(Type::Bool)
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
            _ => unreachable!(),
        }
    }

    fn block(&self, stmts: &mut Statements) -> Result<Type, Error> {
        for stmt in stmts.iter_mut() {
            match stmt {
                AstNodeStmt::Expr(stmt_expr) => {
                    self.type_check_expr(stmt_expr)?;
                }
                AstNodeStmt::While { .. } => unimplemented!(),
            }
        }
        Ok(match stmts.last() {
            Some(AstNodeStmt::Expr(stmt_expr)) => stmt_expr.type_info.unwrap(),
            _ => Type::Unit,
        })
    }

    fn when_expr(&self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = ast.type_info {
            return Ok(t);
        }

        match ast {
            AstNode {
                kind:
                    AstNodeExpr::WhenExpr {
                        subject: Some(subject),
                        entries,
                        ..
                    },
                ..
            } => {
                let subject_t = self.type_check_expr(subject)?;
                for entry in entries.iter_mut() {
                    let cond_t = self.type_check_expr(&mut entry.cond)?;
                    self.eq(subject_t, cond_t, &entry.cond_start_tok.span)?;

                    self.statements(&mut entry.body)?;
                }
                Ok(Type::Unit)
            }
            AstNode {
                kind:
                    AstNodeExpr::WhenExpr {
                        subject: None,
                        entries,
                        ..
                    },
                ..
            } => {
                for entry in entries.iter_mut() {
                    let cond_t = self.type_check_expr(&mut entry.cond)?;
                    self.eq(cond_t, Type::Bool, &entry.cond_start_tok.span)?;
                    self.statements(&mut entry.body)?;
                }
                Ok(Type::Unit)
            }
            _ => unreachable!(),
        }
    }

    fn type_check_if_expr(&self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = ast.type_info {
            return Ok(t);
        }

        match ast {
            AstNode {
                kind:
                    AstNodeExpr::IfExpr {
                        cond,
                        cond_start_tok,
                        if_body,
                        else_body,
                        else_body_tok,
                    },
                ..
            } => {
                let t = self.type_check_expr(cond)?;
                self.eq(t, Type::Bool, &cond_start_tok.span)?;

                let if_body_t = self.block(if_body)?;
                let else_body_t = self.block(else_body)?;

                /* Kotlinc(tm) actually does not check that, the type is Any
                 which leads to weird, unchecked code like this that does not
                 raise any compile-time error: `(if (1<2) "foo" else false) as String`,
                but will potentially raise a runtime error.
                */
                if if_body_t == Type::Unit || else_body_t == Type::Unit {
                    return Ok(Type::Unit);
                }

                let t = self.coalesce_types(if_body_t, else_body_t, &else_body_tok)?;
                ast.type_info = Some(t);
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn type_check_expr(&self, ast: &mut AstNode) -> Result<Type, Error> {
        match ast {
            AstNode {
                kind: AstNodeExpr::Literal(..),
                ..
            } => self.type_check_literal(ast),
            AstNode {
                kind: AstNodeExpr::Unary(..),
                ..
            } => self.type_check_unary(ast),
            AstNode {
                kind: AstNodeExpr::Binary(..),
                ..
            } => self.type_check_binary(ast),
            AstNode {
                kind: AstNodeExpr::Grouping(expr),
                ..
            } => self.type_check_expr(&mut (**expr)),
            AstNode {
                kind: AstNodeExpr::IfExpr { .. },
                ..
            } => self.type_check_if_expr(ast),
            AstNode {
                kind: AstNodeExpr::WhenExpr { .. },
                ..
            } => self.when_expr(ast),
        }
    }

    fn coalesce_types(&self, left: Type, right: Type, token: &Token) -> Result<Type, Error> {
        match (left, right) {
            (Type::Char, Type::Char) => Ok(Type::Char),
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::UInt, Type::UInt) => Ok(Type::UInt),
            (Type::Long, Type::Long) | (Type::Long, Type::Int) | (Type::Int, Type::Long) => {
                Ok(Type::Long)
            }
            (Type::UInt, Type::ULong) | (Type::ULong, Type::UInt) | (Type::ULong, Type::ULong) => {
                Ok(Type::ULong)
            }
            // Plus
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
                self.lexer.span_location(&token.span))),
        }
    }
}
