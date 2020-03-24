use crate::error::*;
use crate::lex::*;
use log::debug;
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
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub id: NodeId,
    pub body: Statements,
}

pub(crate) const VAR_DEFINITION_FLAG_VAR: u8 = 0;
pub(crate) const VAR_DEFINITION_FLAG_VAL: u8 = 1;

#[derive(Debug)]
pub enum AstNodeStmt {
    Expr(AstNode),
    While {
        cond: AstNode,
        cond_start_tok: Token,
        body: Block,
    },
    DoWhile {
        cond: AstNode,
        cond_start_tok: Token,
        body: Block,
    },
    VarDefinition {
        identifier: Token,
        value: AstNode,
        id: NodeId,
        flags: u8,
    },
    Assign {
        target: AstNode,
        value: AstNode,
        span: Span,
    },
}

pub type Statements = Vec<AstNodeStmt>;
pub type StatementsSlice = [AstNodeStmt];
pub type NodeId = usize;

#[derive(Debug)]
pub struct AstNode {
    pub id: NodeId,
    pub kind: AstNodeExpr,
}

#[derive(Debug)]
pub struct WhenEntry {
    pub cond: AstNode,
    pub body: Block,
    pub cond_start_tok: Token,
}

#[derive(Debug)]
pub enum AstNodeExpr {
    Binary(Box<AstNode>, Token, Box<AstNode>),
    Unary(Token, Box<AstNode>),
    Literal(Token),
    Grouping(Box<AstNode>),
    IfExpr {
        cond: Box<AstNode>,
        cond_start_tok: Token,
        if_body: Block,
        // if_body_tok: Token,
        else_body: Block,
        else_body_tok: Token,
    },
    WhenExpr {
        subject: Option<Box<AstNodeStmt>>,
        entries: Vec<WhenEntry>,
        else_entry: Option<Block>,
    },
    VarRef(Span),
}

#[derive(Debug)]
pub struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: &'a mut Lexer,
    current_id: usize,
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
        match self.previous {
            Some(tok @ Token { .. }) if tok.kind == kind => {
                self.advance()?;
                Ok(tok)
            }
            _ => Err(Error::new(
                ErrorKind::ExpectedToken,
                self.lexer.span_location(&self.previous.unwrap().span),
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

    fn block(&mut self) -> Result<Block, Error> {
        self.eat(TokenKind::LeftCurlyBracket)?;
        self.skip_newlines()?;
        let stmts = self.block_statements()?;
        self.skip_newlines()?;
        self.eat(TokenKind::RightCurlyBracket)?;
        Ok(stmts)
    }

    // if-body. for-body, etc
    fn control_structure_body(&mut self) -> Result<Block, Error> {
        match self.previous {
            Some(Token { kind: k, .. }) if k == TokenKind::LeftCurlyBracket => self.block(),
            _ => Ok(Block {
                id: self.next_id(),
                body: vec![self.statement()?],
            }),
        }
    }

    fn while_stmt(&mut self) -> Result<AstNodeStmt, Error> {
        self.eat(TokenKind::KeywordWhile)?;
        self.skip_newlines()?;
        let cond_start_tok = self.eat(TokenKind::LeftParen)?;
        let cond = self.expression()?;
        self.eat(TokenKind::RightParen)?;
        self.skip_newlines()?;

        let body = match self.previous.unwrap().kind {
            TokenKind::Semicolon => Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };

        Ok(AstNodeStmt::While {
            cond,
            cond_start_tok,
            body,
        })
    }

    fn do_while_stmt(&mut self) -> Result<AstNodeStmt, Error> {
        self.eat(TokenKind::KeywordDo)?;
        self.skip_newlines()?;

        let body = match self.previous.unwrap().kind {
            TokenKind::KeywordWhile => Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };
        self.eat(TokenKind::KeywordWhile)?;
        self.skip_newlines()?;
        let cond_start_tok = self.eat(TokenKind::LeftParen)?;
        let cond = self.expression()?;
        self.eat(TokenKind::RightParen)?;

        Ok(AstNodeStmt::DoWhile {
            cond,
            cond_start_tok,
            body,
        })
    }

    fn when_cond(&mut self) -> Result<AstNode, Error> {
        // TODO: allow range & type test here
        self.expression()
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

    fn when_entries(&mut self) -> Result<(Vec<WhenEntry>, Option<Block>), Error> {
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

                let value = self.expression()?;
                self.eat(TokenKind::RightParen)?;

                if let Some(identifier) = identifier {
                    // Ok(Some(Box::new(AstNode::AstNodeExpr())))
                    Ok(Some(Box::new(AstNodeStmt::VarDefinition {
                        identifier,
                        value,
                        id: self.next_id(),
                        flags: VAR_DEFINITION_FLAG_VAL,
                    })))
                } else {
                    Ok(Some(Box::new(AstNodeStmt::Expr(value))))
                }
            }
            _ => Ok(None),
        }
    }

    fn when_expr(&mut self) -> Result<AstNode, Error> {
        self.eat(TokenKind::KeywordWhen)?;
        self.skip_newlines()?;

        let subject = self.when_subject()?;

        self.eat(TokenKind::LeftCurlyBracket)?;
        self.skip_newlines()?;

        let (entries, else_entry) = self.when_entries()?;

        self.skip_newlines()?;
        self.eat(TokenKind::RightCurlyBracket)?;

        Ok(AstNode {
            kind: AstNodeExpr::WhenExpr {
                entries,
                subject,
                else_entry,
            },
            id: self.next_id(),
        })
    }

    fn if_expr(&mut self) -> Result<AstNode, Error> {
        self.eat(TokenKind::KeywordIf)?;
        self.skip_newlines()?;

        let cond_start_tok = self.eat(TokenKind::LeftParen)?;
        self.skip_newlines()?;

        let cond = self.expression()?;
        self.skip_newlines()?;

        self.eat(TokenKind::RightParen)?;
        self.skip_newlines()?;

        let if_body = match self.previous {
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => {
                self.advance()?;
                Block {
                    id: self.next_id(),
                    body: vec![],
                }
            }
            Some(Token {
                kind: TokenKind::KeywordElse,
                ..
            }) => Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };
        self.skip_newlines()?;

        let else_body_tok = self.eat(TokenKind::KeywordElse)?;
        self.skip_newlines()?;

        let else_body = match self.previous {
            Some(Token {
                kind: TokenKind::Eof,
                ..
            })
            | Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => Block {
                id: self.next_id(),
                body: vec![],
            },
            _ => self.control_structure_body()?,
        };

        Ok(AstNode {
            kind: AstNodeExpr::IfExpr {
                cond: Box::new(cond),
                cond_start_tok,
                if_body,
                else_body,
                else_body_tok,
            },
            id: self.next_id(),
        })
    }

    fn primary(&mut self) -> Result<AstNode, Error> {
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
                // TODO: fill type info here right away
                Ok(AstNode {
                    kind: AstNodeExpr::Literal(previous),
                    id: self.next_id(),
                })
            }
            TokenKind::LeftParen => {
                self.advance()?;
                self.skip_newlines()?;
                let expr = self.expression()?;
                self.eat(TokenKind::RightParen)?;
                Ok(AstNode {
                    kind: AstNodeExpr::Grouping(Box::new(expr)),
                    id: self.next_id(),
                })
            }
            TokenKind::KeywordIf => self.if_expr(),
            TokenKind::KeywordWhen => self.when_expr(),
            TokenKind::Identifier => {
                self.advance()?;
                Ok(AstNode {
                    kind: AstNodeExpr::VarRef(previous.span),
                    id: self.next_id(),
                })
            }
            _ => Err(Error::new(
                ErrorKind::ExpectedPrimary,
                self.lexer.span_location(&previous.span),
            )),
        }
    }

    fn unary(&mut self) -> Result<AstNode, Error> {
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Plus | TokenKind::Bang | TokenKind::Minus => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.unary()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Unary(previous, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => self.primary(),
        }
    }

    fn multiplication(&mut self) -> Result<AstNode, Error> {
        let left = self.unary()?;
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Percent | TokenKind::Star | TokenKind::Slash => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.multiplication()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn addition(&mut self) -> Result<AstNode, Error> {
        let left = self.multiplication()?;
        let previous = self.previous.unwrap();
        match previous.kind {
            TokenKind::Plus | TokenKind::Minus => {
                self.advance()?;
                self.skip_newlines()?;
                let right = self.addition()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn range(&mut self) -> Result<AstNode, Error> {
        let left = self.addition()?;
        match self.previous.unwrap().kind {
            TokenKind::DotDot => {
                let previous = self.eat(TokenKind::DotDot)?;
                self.skip_newlines()?;
                let right = self.range()?;

                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn comparison(&mut self) -> Result<AstNode, Error> {
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
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn equality(&mut self) -> Result<AstNode, Error> {
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
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn conjunction(&mut self) -> Result<AstNode, Error> {
        let left = self.equality()?;
        self.skip_newlines()?;

        match self.previous.unwrap().kind {
            TokenKind::AmpersandAmpersand => {
                let tok = self.eat(TokenKind::AmpersandAmpersand)?;
                self.skip_newlines()?;
                let right = self.conjunction()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), tok, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn disjunction(&mut self) -> Result<AstNode, Error> {
        let left = self.conjunction()?;
        self.skip_newlines()?;

        match self.previous.unwrap().kind {
            TokenKind::PipePipe => {
                let tok = self.eat(TokenKind::PipePipe)?;
                self.skip_newlines()?;
                let right = self.disjunction()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), tok, Box::new(right)),
                    id: self.next_id(),
                })
            }
            _ => Ok(left),
        }
    }

    fn expression(&mut self) -> Result<AstNode, Error> {
        self.disjunction()
    }

    fn var_def(&mut self) -> Result<AstNodeStmt, Error> {
        let flags = match self.previous.unwrap().kind {
            TokenKind::KeywordVar => {
                self.advance()?;
                VAR_DEFINITION_FLAG_VAR
            }
            TokenKind::KeywordVal => {
                self.advance()?;
                VAR_DEFINITION_FLAG_VAL
            }
            _ => unreachable!(),
        };

        self.skip_newlines()?;
        let identifier = self.eat(TokenKind::Identifier)?;
        self.skip_newlines()?;
        self.eat(TokenKind::Equal)?;
        self.skip_newlines()?;
        let value = self.expression()?;
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

    fn statement(&mut self) -> Result<AstNodeStmt, Error> {
        match self.previous.unwrap().kind {
            TokenKind::KeywordWhile => self.while_stmt(),
            TokenKind::KeywordDo => self.do_while_stmt(),
            TokenKind::KeywordVal | TokenKind::KeywordVar => self.var_def(),
            _ => match (self.previous.unwrap().kind, self.current.unwrap().kind) {
                (TokenKind::Identifier, TokenKind::Equal) => {
                    let span = self.current.unwrap().span;
                    let target = self.primary()?;
                    self.eat(TokenKind::Equal)?;
                    self.skip_newlines()?;
                    debug!("assignement");
                    let value = self.expression()?;
                    Ok(AstNodeStmt::Assign {
                        target,
                        value,
                        span,
                    })
                }
                _ => Ok(AstNodeStmt::Expr(self.expression()?)),
            },
        }
    }

    fn block_statements(&mut self) -> Result<Block, Error> {
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
        Ok(Block {
            id: self.next_id(),
            body,
        })
    }

    fn statements(&mut self) -> Result<Block, Error> {
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
        Ok(Block {
            id: self.next_id(),
            body,
        })
    }

    pub fn new(lexer: &mut Lexer) -> Parser {
        Parser {
            previous: None,
            current: None,
            lexer,
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

    pub fn parse(&mut self) -> Result<Block, Error> {
        self.advance()?;
        self.skip_newlines()?;
        self.advance()?;
        self.skip_newlines()?;
        self.statements()
    }
}
