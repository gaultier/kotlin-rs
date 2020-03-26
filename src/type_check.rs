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

    fn eq(&self, left: &Type, right: &Type, span: &Span) -> Result<(), Error> {
        if left != right {
            Err(Error::new(
                ErrorKind::IncompatibleTypes(left.clone(), right.clone()),
                self.lexer.span_location(&span),
            ))
        } else {
            Ok(())
        }
    }

    fn while_stmt(
        &mut self,
        cond: &AstNodeExpr,
        block: &Block,
        cond_start_tok: &Token,
    ) -> Result<Type, Error> {
        let cond_t = self.expr(cond)?;
        self.eq(&cond_t, &Type::Bool, &cond_start_tok.span)?;
        self.statements(block)?;
        Ok(Type::Unit)
    }

    fn var_def(&mut self, value: &AstNodeExpr, id: NodeId) -> Result<Type, Error> {
        let t = self.expr(value)?;
        self.types.insert(id, t.clone());

        Ok(t)
    }

    fn assign(
        &mut self,
        target: &AstNodeExpr,
        value: &AstNodeExpr,
        span: &Span,
    ) -> Result<Type, Error> {
        let target_t = self.expr(target)?;
        let value_t = self.expr(value)?;
        self.eq(&target_t, &value_t, span)?;
        Ok(Type::Unit)
    }

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<Type, Error> {
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
            AstNodeStmt::Assign {
                target,
                value,
                span,
                ..
            } => self.assign(target, value, span),
            AstNodeStmt::FnDefinition {
                fn_name,
                args,
                body,
                flags,
                id,
            } => self.fn_def(fn_name, args, body, *flags, *id),
        }
    }

    pub fn statements(&mut self, statements: &Block) -> Result<Type, Error> {
        for stmt in statements.body.iter() {
            self.statement(stmt)?;
        }

        Ok(statements
            .body
            .last()
            .map(|stmt| match stmt {
                AstNodeStmt::Expr(expr) => {
                    self.types.get(&expr.id()).unwrap_or(&Type::Unit).clone()
                }
                _ => Type::Unit,
            })
            .unwrap_or(Type::Unit))
    }

    fn unary(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        match ast {
            AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Minus,
                        ..
                    },
                expr: right,
                id,
                ..
            }
            | AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Plus,
                        ..
                    },
                expr: right,
                id,
                ..
            } => {
                let t = self.expr(right)?;
                self.types.insert(*id, t.clone());
                Ok(t)
            }
            AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::PlusPlus,
                        span,
                    },
                expr: right,
                id,
                ..
            }
            | AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::MinusMinus,
                        span,
                    },
                expr: right,
                id,
                ..
            } => {
                let t = self.expr(right)?;
                match t {
                    Type::Int
                    | Type::UInt
                    | Type::Long
                    | Type::ULong
                    | Type::Float
                    | Type::Double => {
                        self.types.insert(*id, t.clone());
                        Ok(t)
                    }
                    _ => Err(Error::new(
                        // FIXME: should not be Int
                        ErrorKind::IncompatibleTypes(t, Type::Int),
                        self.lexer.span_location(span),
                    )),
                }
            }

            AstNodeExpr::Unary {
                token:
                    tok
                    @
                    Token {
                        kind: TokenKind::Bang,
                        ..
                    },
                expr: right,
                id,
                ..
            } => {
                let right_t = self.expr(right)?;
                let t = self.coalesce_types(&Type::Bool, &right_t, tok)?;
                self.types.insert(*id, t);
                Ok(Type::Bool)
            }
            _ => unreachable!(),
        }
    }

    fn literal(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        let t = match ast {
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Int(_),
                    ..
                },
                _,
            ) => Type::Int,
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::UInt(_),
                    ..
                },
                _,
            ) => Type::UInt,
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Long(_),
                    ..
                },
                _,
            ) => Type::Long,
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::ULong(_),
                    ..
                },
                _,
            ) => Type::ULong,
            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Float(_),
                    ..
                },
                _,
            ) => Type::Float,

            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Double(_),
                    ..
                },
                _,
            ) => Type::Double,

            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Null,
                    ..
                },
                _,
            ) => Type::Null,

            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Bool(_),
                    ..
                },
                _,
            ) => Type::Bool,

            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::TString,
                    ..
                },
                _,
            ) => Type::TString,

            AstNodeExpr::Literal(
                Token {
                    kind: TokenKind::Char(_),
                    ..
                },
                _,
            ) => Type::Char,
            _ => unreachable!(),
        };

        self.types.insert(ast.id(), t.clone());
        Ok(t)
    }

    fn binary(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        match ast {
            AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::BangEqual,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::EqualEqual,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::BangEqualEqual,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::EqualEqualEqual,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::DotDot,
                        ..
                    },
                right,
                id,
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;

                self.eq(&left_t, &right_t, &op.span)?;
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
                            self.lexer.span_location(&op.span),
                        ));
                    }
                };

                self.types.insert(*id, t.clone());
                Ok(t)
            }

            AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::Lesser,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::LesserEqual,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::Greater,
                        ..
                    },
                right,
                id,
            }
            | AstNodeExpr::Binary {
                left,
                op:
                    op
                    @
                    Token {
                        kind: TokenKind::GreaterEqual,
                        ..
                    },
                right,
                id,
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;

                self.coalesce_types(&left_t, &right_t, &op)?;

                let t = Type::Bool;
                self.types.insert(*id, t.clone());
                Ok(t)
            }
            AstNodeExpr::Binary {
                left,
                op,
                right,
                id,
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;
                let t = self.coalesce_types(&left_t, &right_t, &op)?;
                self.types.insert(*id, t.clone());
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn block(&mut self, block: &Block) -> Result<Type, Error> {
        for stmt in block.body.iter() {
            self.statement(stmt)?;
        }
        Ok(match block.body.last() {
            Some(AstNodeStmt::Expr(stmt_expr)) => self
                .types
                .get(&stmt_expr.id())
                .unwrap_or(&Type::Unit)
                .clone(),
            _ => Type::Unit,
        })
    }

    fn when_expr(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        match ast {
            AstNodeExpr::WhenExpr {
                subject: Some(subject),
                entries,
                ..
            } => {
                let subject_t = self.statement(subject)?;
                for entry in entries.iter() {
                    let cond_t = self.expr(&entry.cond)?;
                    self.eq(&subject_t, &cond_t, &entry.cond_start_tok.span)?;

                    self.statements(&entry.body)?;
                }
                Ok(Type::Unit)
            }
            AstNodeExpr::WhenExpr {
                subject: None,
                entries,
                ..
            } => {
                for entry in entries.iter() {
                    let cond_t = self.expr(&entry.cond)?;
                    self.eq(&cond_t, &Type::Bool, &entry.cond_start_tok.span)?;
                    self.statements(&entry.body)?;
                }
                Ok(Type::Unit)
            }
            _ => unreachable!(),
        }
    }

    fn if_expr(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        match ast {
            AstNodeExpr::IfExpr {
                cond,
                cond_start_tok,
                if_body,
                else_body,
                else_body_tok,
                id,
            } => {
                let t = self.expr(cond)?;
                self.eq(&t, &Type::Bool, &cond_start_tok.span)?;

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

                let t = self.coalesce_types(&if_body_t, &else_body_t, &else_body_tok)?;
                self.types.insert(*id, t.clone());
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn var_ref(&mut self, id: NodeId) -> Result<Type, Error> {
        let var_usage_ref = self.resolution.get(&id).unwrap();
        let t = self.types.get(&var_usage_ref.node_ref_id).unwrap().clone();
        debug!(
            "var ref: id={} var_usage_ref={:?} type={}",
            id, &var_usage_ref, t
        );
        self.types.insert(id, t.clone());
        Ok(t)
    }

    fn fn_call(
        &mut self,
        fn_name: &AstNodeExpr,
        call_span: &Span,
        args: &[AstNodeExpr],
        id: NodeId,
    ) -> Result<Type, Error> {
        let t = self.expr(fn_name)?;
        for arg in args {
            self.expr(arg)?;
        }

        debug!("fn ref: id={} t={}", id, t);

        match t {
            Type::Function { return_t, .. } => Ok(*return_t),
            _ => Err(Error::new(
                ErrorKind::NotACallable(t),
                self.lexer.span_location(call_span),
            )),
        }
    }

    fn fn_def(
        &mut self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        flags: u16,
        id: NodeId,
    ) -> Result<Type, Error> {
        let args_t = args
            .iter()
            .map(|arg| self.expr(arg))
            .collect::<Result<Vec<_>, Error>>()?;

        let return_t = self.statement(body)?;

        let t = Type::Function {
            args: args_t,
            return_t: Box::new(return_t),
        };

        self.types.insert(id, t.clone());

        debug!(
            "fn def: fn_name={:?} args={:?} body={:?} flags={} id={} type={:?}",
            fn_name, args, body, flags, id, t
        );

        Ok(t)
    }

    fn expr(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        match ast {
            AstNodeExpr::Literal(..) => self.literal(ast),
            AstNodeExpr::Unary { .. } => self.unary(ast),
            AstNodeExpr::Binary { .. } => self.binary(ast),
            AstNodeExpr::Grouping(expr, _) => self.expr(expr),
            AstNodeExpr::IfExpr { .. } => self.if_expr(ast),
            AstNodeExpr::WhenExpr { .. } => self.when_expr(ast),
            AstNodeExpr::VarRef(_, id) => self.var_ref(*id),
            AstNodeExpr::FnCall {
                fn_name,
                args,
                call_span,
                id,
                ..
            } => self.fn_call(fn_name, call_span, args, *id),
        }
    }

    fn coalesce_types(&self, left: &Type, right: &Type, token: &Token) -> Result<Type, Error> {
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
                ErrorKind::IncompatibleTypes(left.clone(), right.clone()),
                self.lexer.span_location(&token.span))),
        }
    }
}
