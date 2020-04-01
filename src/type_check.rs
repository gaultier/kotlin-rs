use crate::error::*;
use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use log::debug;

pub(crate) struct TypeChecker<'a> {
    lexer: &'a Lexer,
    resolution: &'a Resolution,
    types: &'a mut Types,
    current_fn_id: Option<NodeId>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        lexer: &'a Lexer,
        resolution: &'a Resolution,
        types: &'a mut Types,
    ) -> TypeChecker<'a> {
        TypeChecker {
            lexer,
            resolution,
            types,
            current_fn_id: None,
        }
    }

    // (left, right) can also be meant as (found, expected)
    fn is_type(&self, left: &Type, right: &Type, span: &Span) -> Result<(), Error> {
        if right == &Type::Any {
            return Ok(());
        }
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
        block: &AstNodeStmt,
        cond_start_tok: &Token,
    ) -> Result<Type, Error> {
        let cond_t = self.expr(cond)?;
        self.is_type(&cond_t, &Type::Boolean, &cond_start_tok.span)?;
        self.statement(block)?;
        Ok(Type::Unit)
    }

    fn var_def(
        &mut self,
        value: &AstNodeExpr,
        identifier: &Token,
        id: NodeId,
    ) -> Result<Type, Error> {
        let value_t = self.expr(value)?;
        if let Some(t) = self.types.get(&id) {
            // If a type was specified explicitely, check that it matches the implicit type of the
            // value.
            self.is_type(&value_t, t, &identifier.span)?;
        } else {
            // Otherwise infer the type
            self.types.insert(id, value_t.clone());
        }

        Ok(value_t)
    }

    fn assign(
        &mut self,
        target: &AstNodeExpr,
        value: &AstNodeExpr,
        span: &Span,
    ) -> Result<Type, Error> {
        let target_t = self.expr(target)?;
        let value_t = self.expr(value)?;
        self.is_type(&target_t, &value_t, span)?;
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
            AstNodeStmt::VarDefinition {
                value,
                id,
                identifier,
                ..
            } => self.var_def(value, identifier, *id),
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
                return_t_span,
            } => self.fn_def(fn_name, args, body, *flags, return_t_span, *id),
            AstNodeStmt::Block { body, .. } => self.block(body),
            AstNodeStmt::Println(expr) => {
                self.expr(expr)?;
                Ok(Type::Unit)
            }
        }
    }

    pub fn check_types(&mut self, statements: &AstNodeStmt) -> Result<Types, Error> {
        self.statements(statements)?;
        Ok(self.types.clone())
    }

    fn statements(&mut self, statements: &AstNodeStmt) -> Result<Type, Error> {
        self.statement(statements)
    }

    fn unary(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

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
                let t = self.coalesce_types(&Type::Boolean, &right_t, tok)?;
                self.types.insert(*id, t);
                Ok(Type::Boolean)
            }
            _ => unreachable!(),
        }
    }

    fn literal(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        // Already filled in parser
        Ok(self.types.get(&ast.id()).unwrap().clone())
    }

    fn binary(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

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
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;

                self.is_type(&left_t, &right_t, &op.span)?;

                self.types.insert(*id, Type::Boolean);
                Ok(Type::Boolean)
            }
            AstNodeExpr::Binary {
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

                self.is_type(&left_t, &right_t, &op.span)?;
                let t = match left_t {
                    Type::Int => Type::IntRange,
                    Type::UInt => Type::UIntRange,
                    Type::Long => Type::LongRange,
                    Type::ULong => Type::ULongRange,
                    Type::Float => Type::FloatRange,
                    Type::Double => Type::DoubleRange,
                    Type::TString => Type::TStringRange,
                    Type::Char => Type::CharRange,
                    Type::Boolean => Type::BooleanRange,
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

                let t = Type::Boolean;
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

    fn block(&mut self, block: &[AstNodeStmt]) -> Result<Type, Error> {
        for stmt in block {
            self.statement(stmt)?;
        }
        Ok(match block.last() {
            Some(AstNodeStmt::Expr(stmt_expr)) => self
                .types
                .get(&stmt_expr.id())
                .unwrap_or(&Type::Unit)
                .clone(),
            _ => Type::Unit,
        })
    }

    fn when_expr(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

        match ast {
            AstNodeExpr::WhenExpr {
                subject: Some(subject),
                entries,
                ..
            } => {
                let subject_t = self.statement(subject)?;
                for entry in entries.iter() {
                    let cond_t = self.expr(&entry.cond)?;
                    self.is_type(&subject_t, &cond_t, &entry.cond_start_tok.span)?;

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
                    self.is_type(&cond_t, &Type::Boolean, &entry.cond_start_tok.span)?;
                    self.statements(&entry.body)?;
                }
                Ok(Type::Unit)
            }
            _ => unreachable!(),
        }
    }

    fn if_expr(&mut self, ast: &AstNodeExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

        match ast {
            AstNodeExpr::IfExpr {
                cond,
                cond_span,
                if_body,
                else_body,
                else_body_span,
                id,
            } => {
                let t = self.expr(cond)?;
                self.is_type(&t, &Type::Boolean, &cond_span)?;

                let if_body_t = self.statement(if_body)?;
                let else_body_t = self.statement(else_body)?;

                /* Kotlinc(tm) actually does not check that the type is Any
                 which leads to weird, unchecked code like this that does not
                 raise any compile-time error: `(if (1<2) "foo" else false) as String`,
                but will potentially raise a runtime error.
                */
                let t = if if_body_t == Type::Unit || else_body_t == Type::Unit {
                    Type::Unit
                } else if if_body_t == Type::Any || else_body_t == Type::Any {
                    Type::Any
                } else {
                    self.is_type(&if_body_t, &else_body_t, &else_body_span)?;
                    if_body_t
                };

                self.types.insert(*id, t.clone());
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn var_ref(&mut self, id: NodeId) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&id) {
            return Ok(t.clone());
        }

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
        if let Some(t) = self.types.get(&id) {
            return Ok(t.clone());
        }

        let t = self.expr(fn_name)?;

        debug!("fn ref: id={} t={}", id, t);

        match t {
            Type::Function {
                return_t,
                args: args_t,
            } => {
                for (arg, expected_arg_t) in args.iter().zip(args_t) {
                    let found_arg_t = self.expr(arg)?;
                    self.is_type(&found_arg_t, &expected_arg_t, call_span)?;
                }
                Ok(return_t.unwrap_or(Type::Any))
            }
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
        return_t_span: &Span,
        id: NodeId,
    ) -> Result<Type, Error> {
        let current_fn_id = self.current_fn_id;
        self.current_fn_id = Some(id);
        let mut found_return_t = self.statement(body)?;
        if found_return_t == Type::Unit {
            found_return_t = self
                .types
                .get(&id)
                .unwrap_or(&Type::Unit)
                .fn_return_t()
                .unwrap_or(Type::Unit);
        }

        match self.types.get(&id) {
            Some(Type::Function { return_t, .. }) if return_t.is_some() => {
                let expected_return_t = return_t.clone().unwrap();

                self.is_type(&found_return_t, &expected_return_t, &return_t_span)?;
            }
            Some(Type::Function { .. }) => {
                // Noop
            }
            Some(_) | None => unreachable!(),
        }

        let args_t = args
            .iter()
            .map(|arg| self.expr(arg))
            .collect::<Result<Vec<_>, Error>>()?;
        debug!("fn_def: args_t={:?} args={:?}", args_t, args);

        let t = Type::Function {
            args: args_t,
            return_t: Box::new(Some(found_return_t)),
        };

        self.types.insert(id, t.clone());

        debug!(
            "fn def: fn_name={:?} args={:?} body={:?} flags={} id={} type={:?}",
            fn_name, args, body, flags, id, t
        );

        self.current_fn_id = current_fn_id;

        Ok(t)
    }

    fn range_test(&mut self, range: &AstNodeExpr, span: &Span, id: NodeId) -> Result<Type, Error> {
        let t = self.expr(range)?;
        if t.is_range() {
            self.types.insert(id, Type::Boolean);
            Ok(Type::Boolean)
        } else {
            Err(Error::new(
                ErrorKind::IncompatibleTypes(t.clone(), Type::IntRange), // FIXME
                self.lexer.span_location(&span),
            ))
        }
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
            AstNodeExpr::Jump {
                id,
                kind: JumpKind::Return,
                expr,
                span,
            } => {
                let return_t = if let Some(expr) = expr {
                    self.expr(expr)?
                } else {
                    Type::Unit
                };
                self.types.insert(*id, Type::Nothing);

                // Safe because of the resolver checks
                let current_fn_id = self.current_fn_id.unwrap();

                // Is the function type fully known?
                if let Some(fn_t) = self.types.get(&current_fn_id) {
                    // If at that point the return type of the function is known (either declared
                    // or inferred), check that this type and the type of the return expression
                    // match
                    if let Some(prior_return_t) = fn_t.fn_return_t() {
                        self.is_type(&return_t, &prior_return_t, span)?;
                    } else {
                        // Fill the inferred return type
                        let new_fn_t = fn_t.clone();
                        self.types
                            .insert(current_fn_id, new_fn_t.with_fn_return_t(&return_t));
                    }
                } else {
                }

                Ok(Type::Nothing)
            }
            AstNodeExpr::Jump { id, .. } => Ok(self.types.get(id).cloned().unwrap()),
            AstNodeExpr::RangeTest {
                range, span, id, ..
            } => self.range_test(range, span, *id),
        }
    }

    fn coalesce_types(&self, left: &Type, right: &Type, token: &Token) -> Result<Type, Error> {
        match (left, right) {
            (Type::Char, Type::Char) => Ok(Type::Char),
            (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
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
            | (Type::TString, Type::Boolean)
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
