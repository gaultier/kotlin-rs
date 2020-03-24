use crate::error::*;
use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use log::debug;
use std::collections::BTreeMap;

pub(crate) struct TypeChecker<'a> {
    lexer: &'a Lexer,
    resolution: &'a Resolution,
    types: BTreeMap<NodeId, Type>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(lexer: &'a Lexer, resolution: &'a Resolution) -> TypeChecker<'a> {
        TypeChecker {
            lexer,
            resolution,
            types: BTreeMap::new(),
        }
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
        &mut self,
        cond: &mut AstNode,
        block: &mut Block,
        cond_start_tok: &Token,
    ) -> Result<Type, Error> {
        let cond_t = self.expr(cond)?;
        self.eq(cond_t, Type::Bool, &cond_start_tok.span)?;
        self.statements(block)?;
        Ok(Type::Unit)
    }

    fn var_def(&mut self, value: &mut AstNode, id: NodeId) -> Result<Type, Error> {
        let t = self.expr(value)?;
        self.types.insert(id, t);

        Ok(t)
    }

    fn statement(&mut self, statement: &mut AstNodeStmt) -> Result<Type, Error> {
        match statement {
            AstNodeStmt::Expr(expr) => self.expr(expr),
            AstNodeStmt::DoWhile {
                cond,
                cond_start_tok,
                body,
            }
            | AstNodeStmt::While {
                cond,
                cond_start_tok,
                body,
            } => self.while_stmt(cond, body, &cond_start_tok),
            AstNodeStmt::VarDefinition { value, id, .. } => self.var_def(value, *id),
        }
    }

    pub fn statements(&mut self, statements: &mut Block) -> Result<Type, Error> {
        for stmt in statements.body.iter_mut() {
            self.statement(stmt)?;
        }

        Ok(statements
            .body
            .last()
            .map(|stmt| match stmt {
                AstNodeStmt::Expr(expr) => *self.types.get(&expr.id).unwrap_or(&Type::Unit),
                _ => Type::Unit,
            })
            .unwrap_or(Type::Unit))
    }

    fn unary(&mut self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id) {
            return Ok(*t);
        }

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
                id,
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
                id,
                ..
            } => {
                let t = self.expr(right)?;
                self.types.insert(*id, t);
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
                id,
                ..
            } => {
                let right_t = self.expr(right)?;
                let t = self.coalesce_types(Type::Bool, right_t, tok)?;
                self.types.insert(*id, t);
                Ok(Type::Bool)
            }
            _ => unreachable!(),
        }
    }

    fn literal(&mut self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id) {
            return Ok(*t);
        }

        let t = match ast {
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Int(_),
                        ..
                    }),
                ..
            } => Type::Int,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::UInt(_),
                        ..
                    }),
                ..
            } => Type::UInt,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Long(_),
                        ..
                    }),
                ..
            } => Type::Long,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::ULong(_),
                        ..
                    }),
                ..
            } => Type::ULong,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Float(_),
                        ..
                    }),
                ..
            } => Type::Float,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Double(_),
                        ..
                    }),
                ..
            } => Type::Double,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Null,
                        ..
                    }),
                ..
            } => Type::Null,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Bool(_),
                        ..
                    }),
                ..
            } => Type::Bool,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::TString,
                        ..
                    }),
                ..
            } => Type::TString,
            AstNode {
                kind:
                    AstNodeExpr::Literal(Token {
                        kind: TokenKind::Char(_),
                        ..
                    }),
                ..
            } => Type::Char,
            _ => unreachable!(),
        };

        self.types.insert(ast.id, t);
        Ok(t)
    }

    fn binary(&mut self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id) {
            return Ok(*t);
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
                id,
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
                id,
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
                id,
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
                id,
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
                id,
                ..
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;

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

                self.types.insert(*id, t);
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
                id,
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
                id,
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
                id,
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
                id,
                ..
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;

                self.coalesce_types(left_t, right_t, &tok)?;

                let t = Type::Bool;
                self.types.insert(*id, t);
                Ok(t)
            }
            AstNode {
                kind: AstNodeExpr::Binary(left, tok, right),
                id,
                ..
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;
                let t = self.coalesce_types(left_t, right_t, &tok)?;
                self.types.insert(*id, t);
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn block(&mut self, block: &mut Block) -> Result<Type, Error> {
        for stmt in block.body.iter_mut() {
            self.statement(stmt)?;
        }
        Ok(match block.body.last() {
            Some(AstNodeStmt::Expr(stmt_expr)) => {
                *self.types.get(&stmt_expr.id).unwrap_or(&Type::Unit)
            }
            _ => Type::Unit,
        })
    }

    fn when_expr(&mut self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id) {
            return Ok(*t);
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
                let subject_t = self.expr(subject)?;
                for entry in entries.iter_mut() {
                    let cond_t = self.expr(&mut entry.cond)?;
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
                    let cond_t = self.expr(&mut entry.cond)?;
                    self.eq(cond_t, Type::Bool, &entry.cond_start_tok.span)?;
                    self.statements(&mut entry.body)?;
                }
                Ok(Type::Unit)
            }
            _ => unreachable!(),
        }
    }

    fn if_expr(&mut self, ast: &mut AstNode) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id) {
            return Ok(*t);
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
                id,
                ..
            } => {
                let t = self.expr(cond)?;
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
                self.types.insert(*id, t);
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn var_ref(&mut self, id: NodeId) -> Result<Type, Error> {
        let var_usage_ref = self.resolution.get(&id).unwrap();
        debug!("var ref: id={} var_usage_ref={:?}", id, &var_usage_ref);
        let t = *(self.types.get(&var_usage_ref.node_id_ref).unwrap());
        self.types.insert(id, t);
        Ok(t)
    }

    fn expr(&mut self, ast: &mut AstNode) -> Result<Type, Error> {
        match ast {
            AstNode {
                kind: AstNodeExpr::Literal(..),
                ..
            } => self.literal(ast),
            AstNode {
                kind: AstNodeExpr::Unary(..),
                ..
            } => self.unary(ast),
            AstNode {
                kind: AstNodeExpr::Binary(..),
                ..
            } => self.binary(ast),
            AstNode {
                kind: AstNodeExpr::Grouping(expr),
                ..
            } => self.expr(&mut (**expr)),
            AstNode {
                kind: AstNodeExpr::IfExpr { .. },
                ..
            } => self.if_expr(ast),
            AstNode {
                kind: AstNodeExpr::WhenExpr { .. },
                ..
            } => self.when_expr(ast),
            AstNode {
                kind: AstNodeExpr::VarRef(_),
                id,
                ..
            } => self.var_ref(*id),
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
