use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use crate::session::{Session, Span};
use log::debug;

pub(crate) struct TypeChecker<'a> {
    session: &'a Session<'a>,
    resolution: &'a Resolution,
    types: &'a mut Types,
    current_fn_id: Option<Id>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        session: &'a Session,
        resolution: &'a Resolution,
        types: &'a mut Types,
    ) -> TypeChecker<'a> {
        TypeChecker {
            session,
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
                self.session.span_location(&span),
            ))
        } else {
            Ok(())
        }
    }

    fn do_while_stmt(
        &mut self,
        cond: &AstExpr,
        block: &AstStmt,
        span: &Span,
    ) -> Result<Type, Error> {
        // The types in the body are inferred before the types in the condition since the condition
        // could use variables declared in the body
        self.statement(block)?;
        let cond_t = self.expr(cond)?;
        self.is_type(&cond_t, &Type::Boolean, &span)?;
        Ok(Type::Unit)
    }

    fn while_stmt(&mut self, cond: &AstExpr, block: &AstStmt, span: &Span) -> Result<Type, Error> {
        let cond_t = self.expr(cond)?;
        self.is_type(&cond_t, &Type::Boolean, &span)?;
        self.statement(block)?;
        Ok(Type::Unit)
    }

    fn var_def(&mut self, value: &AstExpr, identifier: &Token, id: Id) -> Result<Type, Error> {
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

    fn assign(&mut self, target: &AstExpr, value: &AstExpr, span: &Span) -> Result<Type, Error> {
        let target_t = self.expr(target)?;
        let value_t = self.expr(value)?;
        self.is_type(&target_t, &value_t, span)?;
        Ok(Type::Unit)
    }

    fn statement(&mut self, statement: &AstStmt) -> Result<Type, Error> {
        match statement {
            AstStmt::Expr(expr) => self.expr(expr),
            AstStmt::DoWhile { cond, span, body } => self.do_while_stmt(cond, body, span),
            AstStmt::While { cond, span, body } => self.while_stmt(cond, body, span),
            AstStmt::VarDefinition {
                value,
                id,
                identifier,
                ..
            } => self.var_def(value, identifier, *id),
            AstStmt::Assign {
                target,
                value,
                span,
                ..
            } => self.assign(target, value, span),
            AstStmt::FnDefinition {
                fn_name,
                args,
                body,
                flags,
                id,
                return_t_span,
            } => self.fn_def(fn_name, args, body, *flags, return_t_span, *id),
            AstStmt::Block { body, .. } => self.block(body),
            AstStmt::Class {
                name_span,
                body,
                id,
                ..
            } => {
                let name = self.session.src[name_span.start..name_span.end].into();
                let t = Type::UserType { name };
                self.types.insert(*id, t.clone());

                self.statements(body)?;

                Ok(t)
            }
        }
    }

    pub fn check_types(&mut self, statements: &AstStmt) -> Result<Types, Error> {
        self.statements(statements)?;
        Ok(self.types.clone())
    }

    fn statements(&mut self, statements: &AstStmt) -> Result<Type, Error> {
        self.statement(statements)
    }

    fn unary(&mut self, ast: &AstExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

        match ast {
            AstExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Minus,
                        ..
                    },
                expr: right,
                id,
                ..
            }
            | AstExpr::Unary {
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
            AstExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::PlusPlus,
                        span,
                    },
                expr: right,
                id,
                ..
            }
            | AstExpr::Unary {
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
                    Type::Int | Type::Long | Type::Float | Type::Double => {
                        self.types.insert(*id, t.clone());
                        Ok(t)
                    }
                    _ => Err(Error::new(
                        // FIXME: should not be Int
                        ErrorKind::IncompatibleTypes(t, Type::Int),
                        self.session.span_location(span),
                    )),
                }
            }

            AstExpr::Unary {
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

    fn literal(&mut self, ast: &AstExpr) -> Result<Type, Error> {
        // Already filled in parser
        Ok(self.types.get(&ast.id()).unwrap().clone())
    }

    fn binary(&mut self, ast: &AstExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

        match ast {
            AstExpr::Binary {
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
            | AstExpr::Binary {
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
            | AstExpr::Binary {
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
            | AstExpr::Binary {
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
            AstExpr::Binary {
                left,
                op:
                    Token {
                        kind: TokenKind::DotDot,
                        ..
                    },
                right,
                id,
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;

                self.is_type(&right_t, &left_t, &right.span())?;
                let t = match left_t {
                    Type::Int => Type::IntRange,
                    Type::Long => Type::LongRange,
                    Type::Float => Type::FloatRange,
                    Type::Double => Type::DoubleRange,
                    Type::TString => Type::TStringRange,
                    Type::Char => Type::CharRange,
                    Type::Boolean => Type::BooleanRange,
                    _ => {
                        return Err(Error::new(
                            ErrorKind::InvalidRange(left_t),
                            self.session.span_location(&ast.span()),
                        ));
                    }
                };

                self.types.insert(*id, t.clone());
                Ok(t)
            }

            AstExpr::Binary {
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
            | AstExpr::Binary {
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
            | AstExpr::Binary {
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
            | AstExpr::Binary {
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
            AstExpr::Binary {
                left,
                op:
                    Token {
                        kind: TokenKind::KeywordIs,
                        ..
                    },
                id,
                ..
            } => {
                self.expr(left)?;
                Ok(self.types.get(id).cloned().unwrap())
            }
            AstExpr::Binary {
                left,
                op:
                    Token {
                        kind: TokenKind::KeywordIn,
                        span,
                    },
                right,
                id,
            } => {
                let left_t = self.expr(left)?;
                let right_t = self.expr(right)?;
                let t = match (&left_t, &right_t) {
                    (Type::Int, Type::IntRange)
                    | (Type::Long, Type::LongRange)
                    | (Type::Float, Type::FloatRange)
                    | (Type::Double, Type::DoubleRange)
                    | (Type::TString, Type::TStringRange)
                    | (Type::Char, Type::CharRange)
                    | (Type::Boolean, Type::BooleanRange) => Ok(Type::Boolean),
                    _ => {
                        Err(Error::new(
                            ErrorKind::IncompatibleTypes(right_t, Type::IntRange), // FIXME
                            self.session.span_location(&span),
                        ))
                    }
                }?;
                self.types.insert(*id, t.clone());
                Ok(t)
            }
            AstExpr::Binary {
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

    fn block(&mut self, block: &[AstStmt]) -> Result<Type, Error> {
        for stmt in block {
            self.statement(stmt)?;
        }
        Ok(match block.last() {
            Some(AstStmt::Expr(stmt_expr)) => self
                .types
                .get(&stmt_expr.id())
                .unwrap_or(&Type::Unit)
                .clone(),
            _ => Type::Unit,
        })
    }

    fn when_expr(&mut self, ast: &AstExpr, id: Id) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

        let else_entry = match ast {
            AstExpr::WhenExpr {
                subject: Some(subject),
                entries,
                else_entry,
                ..
            } => {
                let subject_t = self.statement(subject)?;
                for entry in entries.iter() {
                    let cond_t = self.expr(&entry.cond)?;
                    match entry.cond {
                        AstExpr::RangeTest { .. } => {
                            self.is_type(&cond_t, &Type::Boolean, &entry.span)?
                        }
                        _ => self.is_type(&cond_t, &subject_t, &entry.span)?,
                    }

                    let t = self.statements(&entry.body)?;
                    self.types.insert(entry.id, t);
                }
                else_entry
            }
            AstExpr::WhenExpr {
                subject: None,
                entries,
                else_entry,
                ..
            } => {
                for entry in entries.iter() {
                    let cond_t = self.expr(&entry.cond)?;
                    self.is_type(&cond_t, &Type::Boolean, &entry.span)?;
                    let t = self.statements(&entry.body)?;
                    self.types.insert(entry.id, t);
                }
                else_entry
            }
            _ => unreachable!(),
        };

        // TODO: should we save the (id, type) in `types` ?
        let t = if let Some(else_entry) = else_entry {
            let t = self.statement(&*else_entry)?;
            self.types.insert(id, t.clone());
            t
        } else {
            Type::Unit
        };
        Ok(t)
    }

    fn if_expr(&mut self, ast: &AstExpr) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&ast.id()) {
            return Ok(t.clone());
        }

        match ast {
            AstExpr::IfExpr {
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
                debug!(
                    "if_expr: id={} if_body_t={} else_body_t={}",
                    id, if_body_t, else_body_t
                );

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
                debug!("if_expr: id={} t={}", id, t);
                Ok(t)
            }
            _ => unreachable!(),
        }
    }

    fn var_ref(&mut self, id: Id) -> Result<Type, Error> {
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
        fn_name: &AstExpr,
        span: &Span,
        args: &[AstExpr],
        id: Id,
    ) -> Result<Type, Error> {
        if let Some(t) = self.types.get(&id) {
            return Ok(t.clone());
        }

        let t = self.expr(fn_name)?;

        debug!("fn call: id={} t={}", id, t);

        match t {
            Type::Function {
                return_t,
                args: args_t,
            } => {
                if args_t.len() != args.len() {
                    return Err(Error::new(
                        ErrorKind::WrongNumberOfArguments {
                            expected: args_t.len(),
                            found: args.len(),
                        },
                        self.session.span_location(span),
                    ));
                }

                for (arg, expected_arg_t) in args.iter().zip(args_t) {
                    let found_arg_t = self.expr(arg)?;
                    self.is_type(&found_arg_t, &expected_arg_t, span)?;
                }
                let return_t = return_t.unwrap_or(Type::Any);
                self.types.insert(id, return_t.clone());
                Ok(return_t)
            }
            _ => Err(Error::new(
                ErrorKind::NotACallable(t),
                self.session.span_location(span),
            )),
        }
    }

    fn fn_def(
        &mut self,
        fn_name: &AstExpr,
        args: &[AstExpr],
        body: &AstStmt,
        flags: u16,
        return_t_span: &Span,
        id: Id,
    ) -> Result<Type, Error> {
        let current_fn_id = self.current_fn_id;
        self.current_fn_id = Some(id);

        debug!("fn_def: id={} t={}", id, self.types.get(&id).unwrap());

        // If the function was defined using the short form `fun add(a:Int, b:Int) = a+b`, the
        // type is inferred to be the one of the expression if not explicitely defined.
        // If the function was defined using the long form `fun add(a:Int, b:Int) { return a+b; }`, the type is the explicit type. If not given it is Unit.
        let found_return_t = match body {
            AstStmt::Expr(e) => {
                let found_return_t = self.expr(e)?;
                let explicit_return_t = self.types.get(&id).map(|t| t.fn_return_t()).flatten();
                if let Some(explicit_return_t) = explicit_return_t {
                    debug!(
                        "fn_def short form: found_return_t={} explicit_return_t={}",
                        &found_return_t, &explicit_return_t
                    );
                    self.is_type(&found_return_t, &explicit_return_t, &return_t_span)?;
                }
                found_return_t
            }
            AstStmt::Block { .. } => {
                self.statement(body)?;
                let explicit_return_t = self
                    .types
                    .get(&id)
                    .map(|t| t.fn_return_t())
                    .flatten()
                    .unwrap_or(Type::Unit);

                debug!("fn_def long form: explicit_return_t={}", &explicit_return_t);

                explicit_return_t
            }
            _ => unreachable!(),
        };

        // Fill the inferred return type

        let fn_t = self
            .types
            .get(&id)
            .unwrap()
            .with_fn_return_t(&found_return_t);
        self.types.insert(id, fn_t.clone());

        debug!(
            "fn def: fn_name={:?} args={:?} body={:?} flags={} id={} type={:?}",
            fn_name, args, body, flags, id, fn_t
        );

        self.current_fn_id = current_fn_id;

        Ok(fn_t)
    }

    fn range_test(&mut self, range: &AstExpr, span: &Span, id: Id) -> Result<Type, Error> {
        let t = self.expr(range)?;
        if t.is_range() {
            self.types.insert(id, Type::Boolean);
            Ok(Type::Boolean)
        } else {
            Err(Error::new(
                ErrorKind::IncompatibleTypes(t, Type::IntRange), // FIXME
                self.session.span_location(&span),
            ))
        }
    }

    fn expr(&mut self, ast: &AstExpr) -> Result<Type, Error> {
        match ast {
            AstExpr::Literal(..) => self.literal(ast),
            AstExpr::Unary { .. } => self.unary(ast),
            AstExpr::Binary { .. } => self.binary(ast),
            AstExpr::Grouping(expr, id) => {
                let t = self.expr(expr).unwrap();
                self.types.insert(*id, t.clone());
                Ok(t)
            }
            AstExpr::IfExpr { .. } => self.if_expr(ast),
            AstExpr::WhenExpr { id, .. } => self.when_expr(ast, *id),
            AstExpr::VarRef(_, id) => self.var_ref(*id),
            AstExpr::FnCall {
                fn_name,
                args,
                span,
                id,
                ..
            } => self.fn_call(fn_name, span, args, *id),
            AstExpr::Jump {
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
                let fn_t = self.types.get(&current_fn_id).unwrap();

                // If at that point the return type of the function is known (either declared
                // or inferred), check that this type and the type of the return expression
                // match
                // Otherwise fill the inferred return type
                if let Some(prior_return_t) = fn_t.fn_return_t() {
                    self.is_type(&return_t, &prior_return_t, span)?;
                } else {
                    let fn_t = fn_t.with_fn_return_t(&return_t);
                    debug!("return: fn_t={} current_fn_id={}", fn_t, current_fn_id);
                    self.types.insert(current_fn_id, fn_t);
                }

                Ok(Type::Nothing)
            }
            AstExpr::Jump { id, .. } => Ok(self.types.get(id).cloned().unwrap()),
            AstExpr::RangeTest {
                range, span, id, ..
            } => self.range_test(range, span, *id),
            AstExpr::TypeTest { id, .. } => Ok(self.types.get(id).cloned().unwrap()),
            AstExpr::Println(expr, id) => {
                self.expr(expr)?;
                self.types.insert(*id, Type::Unit);
                Ok(Type::Unit)
            }
        }
    }

    fn coalesce_types(&self, left: &Type, right: &Type, token: &Token) -> Result<Type, Error> {
        match (left, right) {
            (Type::Char, Type::Char) => Ok(Type::Char),
            (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Long, Type::Long) | (Type::Long, Type::Int) | (Type::Int, Type::Long) => {
                Ok(Type::Long)
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
            | (Type::TString, Type::Long)
            | (Type::Long, Type::TString)
            | (Type::TString, Type::Float)
            | (Type::Float, Type::TString)
            | (Type::TString, Type::Double)
            | (Type::Double, Type::TString)
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
                self.session.span_location(&token.span))),
        }
    }
}
