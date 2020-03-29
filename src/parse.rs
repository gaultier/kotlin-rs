use crate::error::*;
use crate::lex::*;
use log::debug;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Unit,
    IntRange,
    UIntRange,
    LongRange,
    ULongRange,
    FloatRange,
    DoubleRange,
    TStringRange,
    CharRange,
    BoolRange,
    Function {
        args: Vec<Type>,
        return_t: Box<Type>,
    },
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
            Type::Unit => write!(f, "Unit"),
            Type::BoolRange => write!(f, "BoolRange"),
            Type::IntRange => write!(f, "IntRange"),
            Type::UIntRange => write!(f, "UIntRange"),
            Type::LongRange => write!(f, "LongRange"),
            Type::ULongRange => write!(f, "ULongRange"),
            Type::FloatRange => write!(f, "FloatRange"),
            Type::DoubleRange => write!(f, "DoubleRange"),
            Type::CharRange => write!(f, "CharRange"),
            Type::TStringRange => write!(f, "StringRange"),
            Type::Function { args, return_t } => {
                write!(f, "(")?;
                for arg in args {
                    arg.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, ") -> ")?;
                return_t.fmt(f)
            }
        }
    }
}

pub(crate) type Types = BTreeMap<NodeId, Type>;

impl Token {
    pub(crate) fn literal_type(&self) -> Type {
        match self.kind {
            TokenKind::Int(_) => Type::Int,
            TokenKind::Long(_) => Type::Long,
            TokenKind::UInt(_) => Type::UInt,
            TokenKind::ULong(_) => Type::ULong,
            TokenKind::Float(_) => Type::Float,
            TokenKind::Double(_) => Type::Double,
            TokenKind::Bool(_) => Type::Bool,
            TokenKind::TString => Type::TString,
            TokenKind::Char(_) => Type::Char,
            TokenKind::Null => Type::Null,
            _ => unreachable!(),
        }
    }

    pub(crate) fn simple_identifier_type(&self, src: &str) -> Type {
        assert_eq!(self.kind, TokenKind::Identifier);
        let identifier = &src[self.span.start..self.span.end];
        match &identifier {
            &"Int" => Type::Int,
            &"Long" => Type::Long,
            &"UInt" => Type::UInt,
            &"ULong" => Type::ULong,
            &"Float" => Type::Float,
            &"Double" => Type::Double,
            &"Bool" => Type::Bool,
            &"String" => Type::TString,
            &"Char" => Type::Char,
            // FIXME
            &"Null" => Type::Null,
            _ => unreachable!(),
        }
    }
}

pub(crate) const FLAG_VAR: u8 = 0;
pub(crate) const FLAG_VAL: u8 = 1;
pub(crate) const FLAG_FN: u8 = 2;

#[derive(Debug)]
pub enum AstNodeStmt {
    Expr(AstNodeExpr),
    While {
        cond: AstNodeExpr,
        cond_start_tok: Token,
        body: Box<AstNodeStmt>, // Block
    },
    DoWhile {
        cond: AstNodeExpr,
        cond_start_tok: Token,
        body: Box<AstNodeStmt>, // Block
    },
    VarDefinition {
        identifier: Token,
        value: AstNodeExpr,
        id: NodeId,
        flags: u8,
    },
    Assign {
        target: AstNodeExpr,
        value: AstNodeExpr,
        span: Span,
        op: TokenKind,
    },
    FnDefinition {
        fn_name: AstNodeExpr,
        args: Vec<AstNodeExpr>,
        id: NodeId,
        flags: u16,
        body: Box<AstNodeStmt>,
    },
    Block {
        body: Statements,
        id: NodeId,
    },
    Println(AstNodeExpr),
}

pub type Statements = Vec<AstNodeStmt>;
pub type StatementsSlice = [AstNodeStmt];
pub type NodeId = usize;

#[derive(Debug)]
pub struct WhenEntry {
    pub cond: AstNodeExpr,
    pub body: AstNodeStmt, // Block
    pub cond_start_tok: Token,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryKind {
    Prefix,
    Postfix,
}

#[derive(Debug)]
pub enum AstNodeExpr {
    Binary {
        left: Box<AstNodeExpr>,
        op: Token,
        right: Box<AstNodeExpr>,
        id: NodeId,
    },
    Unary {
        token: Token,
        expr: Box<AstNodeExpr>,
        kind: UnaryKind,
        id: NodeId,
    },
    Literal(Token, NodeId),
    Grouping(Box<AstNodeExpr>, NodeId),
    IfExpr {
        cond: Box<AstNodeExpr>,
        cond_span: Span,
        if_body: Box<AstNodeStmt>,   // Block
        else_body: Box<AstNodeStmt>, // Block
        else_body_span: Span,
        id: NodeId,
    },
    WhenExpr {
        subject: Option<Box<AstNodeStmt>>,
        entries: Vec<WhenEntry>,
        else_entry: Option<Box<AstNodeStmt>>, // Block
        id: NodeId,
    },
    VarRef(Span, NodeId),
    FnCall {
        fn_name: Box<AstNodeExpr>,
        call_span: Span,
        args: Vec<AstNodeExpr>,
        id: NodeId,
    },
}

impl AstNodeExpr {
    pub(crate) fn id(&self) -> NodeId {
        match self {
            AstNodeExpr::Binary { id, .. }
            | AstNodeExpr::Unary { id, .. }
            | AstNodeExpr::IfExpr { id, .. }
            | AstNodeExpr::Literal(_, id)
            | AstNodeExpr::WhenExpr { id, .. }
            | AstNodeExpr::VarRef(_, id)
            | AstNodeExpr::FnCall { id, .. }
            | AstNodeExpr::Grouping(_, id) => *id,
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: &'a mut Lexer,
    pub(crate) types: Types,
    pub(crate) current_id: usize,
}

impl Parser<'_> {
    /// Skip over unsignificant tokens
    fn next_parse_token(&mut self) -> Result<Token, Error> {
        loop {
            let token = self.lexer.next_token()?;
            match token.kind {
                TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment { .. } => {
                }
                _ => {
                    return Ok(token);
                }
            }
        }
    }

    fn advance(&mut self) -> Result<(), Error> {
        self.previous = self.current;
        self.current = Some(self.next_parse_token()?);
        Ok(())
    }

    fn skip_newlines(&mut self) -> Result<(), Error> {
        match self.previous {
            Some(Token {
                kind: TokenKind::Newline,
                ..
            }) => {
                self.advance()?;
                self.skip_newlines()
            }
            _ => Ok(()),
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Result<Token, Error> {
        let previous = self.previous.unwrap();
        match previous {
            Token { kind: k, .. } if k == kind => {
                self.advance()?;
                Ok(previous)
            }
            _ => Err(Error::new(
                ErrorKind::ExpectedToken(
                    kind,
                    self.lexer.src[previous.span.start..previous.span.end].to_string(),
                ),
                self.lexer.span_location(&previous.span),
            )),
        }
    }

    fn eat_opt(&mut self, kind: TokenKind) -> Result<bool, Error> {
        match self.previous {
            Some(Token { kind: k, .. }) if k == kind => {
                self.eat(kind).map(|_| ())?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn block(&mut self) -> Result<AstNodeStmt, Error> {
        self.eat(TokenKind::LeftCurlyBracket)?;
        self.skip_newlines()?;
        let stmts = self.block_statements()?;
        self.skip_newlines()?;
        self.eat(TokenKind::RightCurlyBracket)?;
        Ok(stmts)
    }

    // if-body. for-body, etc
    fn control_structure_body(&mut self) -> Result<AstNodeStmt, Error> {
        match self.previous {
            Some(Token { kind: k, .. }) if k == TokenKind::LeftCurlyBracket => self.block(),
            _ => Ok(AstNodeStmt::Block {
                id: self.next_id(),
                body: vec![self.statement()?],
            }),
        }
    }

    fn while_stmt(&mut self) -> Result<AstNodeStmt, Error> {
        self.eat(TokenKind::KeywordWhile)?;
        self.skip_newlines()?;
        let cond_start_tok = self.eat(TokenKind::LeftParen)?;
        let cond = self.expr()?;
        self.eat(TokenKind::RightParen)?;
        self.skip_newlines()?;

        let body = match self.previous.unwrap().kind {
            TokenKind::Semicolon => AstNodeStmt::Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };

        Ok(AstNodeStmt::While {
            cond,
            cond_start_tok,
            body: Box::new(body),
        })
    }

    fn do_while_stmt(&mut self) -> Result<AstNodeStmt, Error> {
        self.eat(TokenKind::KeywordDo)?;
        self.skip_newlines()?;

        let body = match self.previous.unwrap().kind {
            TokenKind::KeywordWhile => AstNodeStmt::Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };
        self.eat(TokenKind::KeywordWhile)?;
        self.skip_newlines()?;
        let cond_start_tok = self.eat(TokenKind::LeftParen)?;
        let cond = self.expr()?;
        self.eat(TokenKind::RightParen)?;

        Ok(AstNodeStmt::DoWhile {
            cond,
            cond_start_tok,
            body: Box::new(body),
        })
    }

    fn when_cond(&mut self) -> Result<AstNodeExpr, Error> {
        // TODO: allow range & type test here
        self.expr()
    }

    fn when_entry(&mut self) -> Result<WhenEntry, Error> {
        let cond = self.when_cond()?;
        self.skip_newlines()?;

        let cond_start_tok = self.eat(TokenKind::Arrow)?;
        self.skip_newlines()?;

        let body = self.control_structure_body()?;

        self.eat_opt(TokenKind::Semicolon)?;

        Ok(WhenEntry {
            cond,
            body,
            cond_start_tok,
        })
    }

    fn when_entries(&mut self) -> Result<(Vec<WhenEntry>, Option<AstNodeStmt>), Error> {
        let mut entries = Vec::new();

        loop {
            match self.previous {
                Some(Token {
                    kind: TokenKind::RightCurlyBracket,
                    ..
                }) => {
                    return Ok((entries, None));
                }
                Some(Token {
                    kind: TokenKind::KeywordElse,
                    ..
                }) => {
                    self.eat(TokenKind::KeywordElse)?;
                    self.skip_newlines()?;
                    self.eat(TokenKind::Arrow)?;
                    self.skip_newlines()?;
                    let else_entry = Some(self.control_structure_body()?);
                    self.eat_opt(TokenKind::Semicolon)?;
                    self.skip_newlines()?;

                    return Ok((entries, else_entry));
                }
                _ => {
                    entries.push(self.when_entry()?);
                    self.skip_newlines()?;
                }
            }
        }
    }

    fn when_subject(&mut self) -> Result<Option<Box<AstNodeStmt>>, Error> {
        match self.previous {
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {
                self.eat(TokenKind::LeftParen)?;

                let identifier = loop {
                    match self.previous.unwrap().kind {
                        TokenKind::Newline => {
                            self.advance()?;
                        }
                        TokenKind::KeywordVal => {
                            self.advance()?;
                            self.skip_newlines()?;
                            let identifier = self.eat(TokenKind::Identifier)?;
                            self.skip_newlines()?;
                            self.eat(TokenKind::Equal)?;
                            self.skip_newlines()?;
                            break Some(identifier);
                        }
                        _ => {
                            break None;
                        }
                    }
                };

                let value = self.expr()?;
                self.eat(TokenKind::RightParen)?;

                if let Some(identifier) = identifier {
                    // Ok(Some(Box::new(AstNode::AstNodeExpr())))
                    Ok(Some(Box::new(AstNodeStmt::VarDefinition {
                        identifier,
                        value,
                        id: self.next_id(),
                        flags: FLAG_VAL,
                    })))
                } else {
                    Ok(Some(Box::new(AstNodeStmt::Expr(value))))
                }
            }
            _ => Ok(None),
        }
    }

    fn when_expr(&mut self) -> Result<AstNodeExpr, Error> {
        self.eat(TokenKind::KeywordWhen)?;
        self.skip_newlines()?;

        let subject = self.when_subject()?;

        self.eat(TokenKind::LeftCurlyBracket)?;
        self.skip_newlines()?;

        let (entries, else_entry) = self.when_entries()?;

        self.skip_newlines()?;
        self.eat(TokenKind::RightCurlyBracket)?;

        Ok(AstNodeExpr::WhenExpr {
            entries,
            subject,
            else_entry: else_entry.map(|x| Box::new(x)),
            id: self.next_id(),
        })
    }

    fn if_expr(&mut self) -> Result<AstNodeExpr, Error> {
        self.eat(TokenKind::KeywordIf)?;
        self.skip_newlines()?;

        let cond_span = self.eat(TokenKind::LeftParen)?.span;
        self.skip_newlines()?;

        let cond = self.expr()?;
        self.skip_newlines()?;

        self.eat(TokenKind::RightParen)?;
        self.skip_newlines()?;

        let if_body = match self.previous {
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => {
                self.advance()?;
                AstNodeStmt::Block {
                    id: self.next_id(),
                    body: vec![],
                }
            }
            Some(Token {
                kind: TokenKind::KeywordElse,
                ..
            }) => AstNodeStmt::Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };
        self.skip_newlines()?;

        let else_body_span = self.eat(TokenKind::KeywordElse)?.span;
        self.skip_newlines()?;

        let else_body = match self.previous {
            Some(Token {
                kind: TokenKind::Eof,
                ..
            })
            | Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => AstNodeStmt::Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };

        Ok(AstNodeExpr::IfExpr {
            cond: Box::new(cond),
            cond_span,
            if_body: Box::new(if_body),
            else_body: Box::new(else_body),
            else_body_span,
            id: self.next_id(),
        })
    }

    fn primary(&mut self) -> Result<AstNodeExpr, Error> {
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Int(_)
            | TokenKind::Long(_)
            | TokenKind::UInt(_)
            | TokenKind::ULong(_)
            | TokenKind::Float(_)
            | TokenKind::Double(_)
            | TokenKind::Bool(_)
            | TokenKind::TString
            | TokenKind::Char(_)
            | TokenKind::Null => {
                self.advance()?;
                self.skip_newlines()?;
                let id = self.next_id();
                self.types.insert(id, previous.literal_type());
                Ok(AstNodeExpr::Literal(previous, id))
            }
            TokenKind::LeftParen => {
                self.advance()?;
                self.skip_newlines()?;
                let expr = self.expr()?;
                self.eat(TokenKind::RightParen)?;
                Ok(AstNodeExpr::Grouping(Box::new(expr), self.next_id()))
            }
            TokenKind::KeywordIf => self.if_expr(),
            TokenKind::KeywordWhen => self.when_expr(),
            TokenKind::Identifier => {
                self.advance()?;
                Ok(AstNodeExpr::VarRef(previous.span, self.next_id()))
            }
            _ => Err(Error::new(
                ErrorKind::ExpectedPrimary,
                self.lexer.span_location(&previous.span),
            )),
        }
    }

    fn simple_identifier(&mut self) -> Result<AstNodeExpr, Error> {
        let tok = self.eat(TokenKind::Identifier)?;
        Ok(AstNodeExpr::VarRef(tok.span, self.next_id()))
    }

    fn fn_call_args(&mut self, args: &mut Vec<AstNodeExpr>) -> Result<(), Error> {
        let previous = self.previous.unwrap();

        if previous.kind == TokenKind::RightParen {
            self.advance()?;
            return Ok(());
        }

        args.push(self.expr()?);

        match self.previous.unwrap().kind {
            TokenKind::Comma => {
                self.advance()?;
                self.fn_call_args(args)
            }
            TokenKind::RightParen => self.fn_call_args(args),
            _ => Err(Error::new(
                ErrorKind::UnexpectedToken(
                    previous.kind,
                    self.lexer.src[previous.span.start..previous.span.end].to_string(),
                ),
                self.lexer.span_location(&previous.span),
            )),
        }
    }

    fn call_suffix(&mut self, fn_name: AstNodeExpr) -> Result<AstNodeExpr, Error> {
        let call_span = self.eat(TokenKind::LeftParen)?.span;
        self.skip_newlines()?;

        let mut args = Vec::new();
        self.fn_call_args(&mut args)?;
        self.skip_newlines()?;

        Ok(AstNodeExpr::FnCall {
            fn_name: Box::new(fn_name),
            call_span,
            args,
            id: self.next_id(),
        })
    }

    fn unary_prefix(&mut self) -> Result<AstNodeExpr, Error> {
        // TODO: annotation, label

        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::PlusPlus
            | TokenKind::MinusMinus
            | TokenKind::Plus
            | TokenKind::Bang
            | TokenKind::Minus => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.unary_prefix()?;
                Ok(AstNodeExpr::Unary {
                    token: previous,
                    expr: Box::new(right),
                    kind: UnaryKind::Prefix,
                    id: self.next_id(),
                })
            }
            _ => self.primary(),
        }
    }

    fn postfix_unary_suffix(&mut self, acc: AstNodeExpr) -> Result<AstNodeExpr, Error> {
        let previous = self.previous.unwrap();
        debug!("postfix_unary_suffix: tok={:?} acc={:?}", previous, acc);
        match previous.kind {
            TokenKind::PlusPlus | TokenKind::MinusMinus | TokenKind::BangBang => {
                self.advance()?;
                let unary = AstNodeExpr::Unary {
                    token: previous,
                    expr: Box::new(acc),
                    kind: UnaryKind::Postfix,
                    id: self.next_id(),
                };

                self.postfix_unary_suffix(unary)
            }
            TokenKind::LeftParen => self.call_suffix(acc),
            _ => Ok(acc),
        }
    }

    fn postfix_unary_expr(&mut self) -> Result<AstNodeExpr, Error> {
        let prim = self.primary()?;
        match self.previous.unwrap().kind {
            TokenKind::LeftParen
            | TokenKind::PlusPlus
            | TokenKind::MinusMinus
            | TokenKind::BangBang => self.postfix_unary_suffix(prim),
            _ => Ok(prim),
        }
    }

    fn prefix_unary_expr(&mut self) -> Result<AstNodeExpr, Error> {
        match self.previous.unwrap().kind {
            TokenKind::PlusPlus
            | TokenKind::MinusMinus
            | TokenKind::Plus
            | TokenKind::Bang
            | TokenKind::Minus => self.unary_prefix(),
            _ => self.postfix_unary_expr(),
        }
    }

    fn as_expr(&mut self) -> Result<AstNodeExpr, Error> {
        // TODO: as
        self.prefix_unary_expr()
    }

    fn multiplication(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.as_expr()?;
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Percent | TokenKind::Star | TokenKind::Slash => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.multiplication()?;
                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: previous,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn addition(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.multiplication()?;
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Plus | TokenKind::Minus => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.addition()?;
                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: previous,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn range(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.addition()?;
        match self.previous.unwrap().kind {
            TokenKind::DotDot => {
                let previous = self.eat(TokenKind::DotDot)?;
                self.skip_newlines()?;
                let right = self.range()?;

                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: previous,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn comparison(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.range()?;
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Lesser
            | TokenKind::LesserEqual => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.comparison()?;
                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: previous,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn equality(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.comparison()?;
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::BangEqual
            | TokenKind::EqualEqual
            | TokenKind::EqualEqualEqual
            | TokenKind::BangEqualEqual => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.equality()?;
                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: previous,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn conjunction(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.equality()?;
        self.skip_newlines()?;

        match self.previous.unwrap().kind {
            TokenKind::AmpersandAmpersand => {
                let tok = self.eat(TokenKind::AmpersandAmpersand)?;
                self.skip_newlines()?;
                let right = self.conjunction()?;
                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: tok,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn disjunction(&mut self) -> Result<AstNodeExpr, Error> {
        let left = self.conjunction()?;
        self.skip_newlines()?;

        match self.previous.unwrap().kind {
            TokenKind::PipePipe => {
                let tok = self.eat(TokenKind::PipePipe)?;
                self.skip_newlines()?;
                let right = self.disjunction()?;
                Ok(AstNodeExpr::Binary {
                    left: Box::new(left),
                    op: tok,
                    right: Box::new(right),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn expr(&mut self) -> Result<AstNodeExpr, Error> {
        self.disjunction()
    }

    fn var_def(&mut self) -> Result<AstNodeStmt, Error> {
        let flags = match self.previous.unwrap().kind {
            TokenKind::KeywordVar => {
                self.advance()?;
                FLAG_VAR
            }
            TokenKind::KeywordVal => {
                self.advance()?;
                FLAG_VAL
            }
            _ => unreachable!(),
        };

        self.skip_newlines()?;
        let identifier = self.eat(TokenKind::Identifier)?;
        self.skip_newlines()?;

        match self.previous.unwrap().kind {
            TokenKind::Colon => {
                self.eat(TokenKind::Colon)?;
                self.skip_newlines()?;
                let type_literal_tok = self.previous.unwrap();
                let type_literal_expr = self.simple_identifier()?;
                let id = type_literal_expr.id();
                let t = type_literal_tok.simple_identifier_type(&self.lexer.src);
                debug!("var_def: t={}", &t);
                self.types.insert(id, t);

                self.skip_newlines()?;
            }
            _ => {}
        }

        self.eat(TokenKind::Equal)?;
        self.skip_newlines()?;
        let value = self.expr()?;
        self.skip_newlines()?;
        self.eat_opt(TokenKind::Semicolon)?;
        self.skip_newlines()?;

        Ok(AstNodeStmt::VarDefinition {
            identifier,
            value,
            id: self.next_id(),
            flags,
        })
    }

    fn assign(&mut self) -> Result<AstNodeStmt, Error> {
        let span = self.current.unwrap().span;
        let target = self.primary()?;

        // Operator
        let op = self.previous.unwrap().kind;
        self.advance()?;

        self.skip_newlines()?;
        debug!("assignement");
        let value = self.expr()?;
        Ok(AstNodeStmt::Assign {
            target,
            value,
            span,
            op,
        })
    }

    fn fn_def_args(&mut self, args: &mut Vec<AstNodeExpr>) -> Result<(), Error> {
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::RightParen => {
                self.advance()?;
                Ok(())
            }
            TokenKind::Identifier => {
                args.push(self.simple_identifier()?);
                self.skip_newlines()?;

                self.eat(TokenKind::Colon)?;
                self.skip_newlines()?;
                let type_literal_tok = self.previous.unwrap();
                let type_literal_expr = self.simple_identifier()?;
                let id = type_literal_expr.id();
                self.types
                    .insert(id, type_literal_tok.simple_identifier_type(&self.lexer.src));
                debug!(
                    "fn_def: arg={:?} t={:?}",
                    args.last().unwrap(),
                    type_literal_expr
                );

                self.skip_newlines()?;
                if self.previous.unwrap().kind != TokenKind::RightParen {
                    self.eat(TokenKind::Comma)?;
                    self.skip_newlines()?;
                }
                self.fn_def_args(args)
            }
            _ => Err(Error::new(
                ErrorKind::UnexpectedToken(
                    previous.kind,
                    self.lexer.src[previous.span.start..previous.span.end].to_string(),
                ),
                self.lexer.span_location(&previous.span),
            )),
        }
    }

    fn fn_def(&mut self) -> Result<AstNodeStmt, Error> {
        // TODO: type, etc
        self.eat(TokenKind::KeywordFun)?;
        self.skip_newlines()?;
        let fn_name = self.primary()?;
        self.skip_newlines()?;

        self.eat(TokenKind::LeftParen)?;
        let mut args = Vec::new();
        self.fn_def_args(&mut args)?;
        self.skip_newlines()?;
        let body = match self.previous.unwrap().kind {
            TokenKind::Equal => {
                self.advance()?;
                self.skip_newlines()?;
                AstNodeStmt::Expr(self.expr()?)
            }
            _ => AstNodeStmt::Block {
                id: self.next_id(),
                body: vec![self.control_structure_body()?],
            },
        };

        Ok(AstNodeStmt::FnDefinition {
            fn_name,
            args,
            body: Box::new(body),
            id: self.next_id(),
            flags: FLAG_FN as u16,
        })
    }

    fn declaration(&mut self) -> Result<AstNodeStmt, Error> {
        match self.previous.unwrap().kind {
            TokenKind::KeywordFun => self.fn_def(),
            _ => unimplemented!(),
        }
    }

    fn statement(&mut self) -> Result<AstNodeStmt, Error> {
        let cur_kind = self.current.unwrap().kind;

        match self.previous.unwrap().kind {
            TokenKind::KeywordWhile => self.while_stmt(),
            TokenKind::KeywordDo => self.do_while_stmt(),
            TokenKind::KeywordVal | TokenKind::KeywordVar => self.var_def(),
            TokenKind::KeywordPrintln => {
                self.advance()?;
                self.eat(TokenKind::LeftParen)?;
                let expr = self.expr()?;
                self.eat(TokenKind::RightParen)?;
                Ok(AstNodeStmt::Println(expr))
            }
            TokenKind::Identifier
                if cur_kind == TokenKind::Equal
                    || cur_kind == TokenKind::MinusEqual
                    || cur_kind == TokenKind::PlusEqual
                    || cur_kind == TokenKind::StarEqual
                    || cur_kind == TokenKind::SlashEqual
                    || cur_kind == TokenKind::PercentEqual =>
            {
                self.assign()
            }
            TokenKind::KeywordFun => self.declaration(),
            _ => Ok(AstNodeStmt::Expr(self.expr()?)),
        }
    }

    fn block_statements(&mut self) -> Result<AstNodeStmt, Error> {
        let mut body = Vec::new();

        while let Some(tok) = &self.previous {
            match tok.kind {
                TokenKind::RightCurlyBracket => {
                    break;
                }
                TokenKind::Semicolon | TokenKind::Newline => {
                    self.advance()?;
                }
                _ => {
                    body.push(self.statement()?);
                }
            }
        }
        Ok(AstNodeStmt::Block {
            id: self.next_id(),
            body,
        })
    }

    fn statements(&mut self) -> Result<AstNodeStmt, Error> {
        let mut body = Vec::new();

        while let Some(tok) = &self.previous {
            debug!("statements: tok={:?}", tok);
            match tok.kind {
                TokenKind::Eof => {
                    break;
                }
                TokenKind::Semicolon | TokenKind::Newline => {
                    self.advance()?;
                }
                _ => {
                    body.push(self.statement()?);
                }
            }
        }
        Ok(AstNodeStmt::Block {
            id: self.next_id(),
            body,
        })
    }

    pub fn new(lexer: &mut Lexer) -> Parser {
        Parser {
            previous: None,
            current: None,
            lexer,
            types: BTreeMap::new(),
            current_id: 0,
        }
    }

    fn next_id(&mut self) -> NodeId {
        self.current_id = self
            .current_id
            .checked_add(1)
            .expect("Out of ids, input too big");
        self.current_id
    }

    pub fn parse(&mut self) -> Result<AstNodeStmt, Error> {
        self.advance()?;
        self.skip_newlines()?;
        self.advance()?;
        self.skip_newlines()?;
        self.statements()
    }
}
