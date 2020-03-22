use crate::error::*;
use crate::lex::*;
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
        }
    }
}

#[derive(Debug)]
pub enum AstNodeStmt {
    Expr(AstNode),
}

pub type Statements = Vec<AstNodeStmt>;

#[derive(Debug)]
pub struct AstNode {
    pub kind: AstNodeExpr,
    pub type_info: Option<Type>,
}

#[derive(Debug)]
pub struct WhenEntry {
    pub cond: AstNode,
    pub body: Statements,
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
        if_body: Statements,
        // if_body_tok: Token,
        else_body: Statements,
        else_body_tok: Token,
    },
    WhenExpr {
        subject: Option<Box<AstNode>>,
        entries: Vec<WhenEntry>,
        else_entry: Option<Vec<AstNodeStmt>>,
    },
}

#[derive(Debug)]
pub struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: &'a mut Lexer,
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
                return Ok(tok);
            }
            _ => Err(Error::new(
                ErrorKind::ExpectedToken,
                self.lexer.span_location(&self.previous.unwrap().span),
            )),
        }
    }

    fn eat_opt(&mut self, kind: TokenKind) -> Result<(), Error> {
        match self.previous {
            Some(Token { kind: k, .. }) if k == kind => self.eat(kind).map(|_| ()),
            _ => Ok(()),
        }
    }

    fn block(&mut self) -> Result<Statements, Error> {
        self.eat(TokenKind::LeftCurlyBracket)?;
        self.skip_newlines()?;
        let stmts = self.block_statements()?;
        self.skip_newlines()?;
        self.eat(TokenKind::RightCurlyBracket)?;
        Ok(stmts)
    }

    // if-body. for-body, etc
    fn control_structure_body(&mut self) -> Result<Statements, Error> {
        match self.previous {
            Some(Token { kind: k, .. }) if k == TokenKind::LeftCurlyBracket => self.block(),
            _ => Ok(vec![self.statement()?]),
        }
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

    fn when_entries(&mut self) -> Result<(Vec<WhenEntry>, Option<Vec<AstNodeStmt>>), Error> {
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

    fn when_subject(&mut self) -> Result<Option<Box<AstNode>>, Error> {
        match self.previous {
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {
                self.eat(TokenKind::LeftParen)?;
                // TODO: val
                let subject = self.expression()?;
                self.eat(TokenKind::RightParen)?;
                Ok(Some(Box::new(subject)))
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
            type_info: None,
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
                vec![]
            }
            Some(Token {
                kind: TokenKind::KeywordElse,
                ..
            }) => vec![],
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
            }) => vec![],
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
            type_info: None,
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
            | TokenKind::Null
            | TokenKind::UnicodeLiteral(_) => {
                self.advance()?;
                self.skip_newlines()?;
                // TODO: fill type info here right away
                Ok(AstNode {
                    kind: AstNodeExpr::Literal(previous),
                    type_info: None,
                })
            }
            TokenKind::LeftParen => {
                self.advance()?;
                self.skip_newlines()?;
                let expr = self.expression()?;
                self.eat(TokenKind::RightParen)?;
                Ok(AstNode {
                    kind: AstNodeExpr::Grouping(Box::new(expr)),
                    type_info: None,
                })
            }
            TokenKind::KeywordIf => self.if_expr(),
            TokenKind::KeywordWhen => self.when_expr(),
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
                    type_info: None,
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
                    type_info: None,
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
                    type_info: None,
                })
            }
            _ => Ok(left),
        }
    }

    fn range(&mut self) -> Result<AstNode, Error> {
        let left = self.addition()?;
        match self.previous {
            Some(Token {
                kind: TokenKind::DotDot,
                ..
            }) => {
                let previous = self.eat(TokenKind::DotDot)?;
                self.skip_newlines()?;
                let right = self.range()?;

                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    type_info: None,
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
                    type_info: None,
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
                    type_info: None,
                })
            }
            _ => Ok(left),
        }
    }

    fn conjunction(&mut self) -> Result<AstNode, Error> {
        let left = self.equality()?;
        self.skip_newlines()?;

        match self.previous {
            Some(Token {
                kind: TokenKind::AmpersandAmpersand,
                ..
            }) => {
                let tok = self.eat(TokenKind::AmpersandAmpersand)?;
                self.skip_newlines()?;
                let right = self.conjunction()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), tok, Box::new(right)),
                    type_info: None,
                })
            }
            _ => Ok(left),
        }
    }

    fn disjunction(&mut self) -> Result<AstNode, Error> {
        let left = self.conjunction()?;
        self.skip_newlines()?;

        match self.previous {
            Some(Token {
                kind: TokenKind::PipePipe,
                ..
            }) => {
                let tok = self.eat(TokenKind::PipePipe)?;
                self.skip_newlines()?;
                let right = self.disjunction()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), tok, Box::new(right)),
                    type_info: None,
                })
            }
            _ => Ok(left),
        }
    }

    fn expression(&mut self) -> Result<AstNode, Error> {
        self.disjunction()
    }

    fn statement(&mut self) -> Result<AstNodeStmt, Error> {
        Ok(AstNodeStmt::Expr(self.expression()?))
    }

    fn block_statements(&mut self) -> Result<Statements, Error> {
        let mut stmts = Vec::new();

        while let Some(tok) = &self.previous {
            match tok.kind {
                TokenKind::RightCurlyBracket => {
                    break;
                }
                TokenKind::Semicolon | TokenKind::Newline => {
                    self.advance()?;
                }
                _ => {
                    stmts.push(self.statement()?);
                }
            }
        }
        Ok(stmts)
    }

    fn statements(&mut self) -> Result<Statements, Error> {
        let mut stmts = Vec::new();

        while let Some(tok) = &self.previous {
            match tok.kind {
                TokenKind::Eof => {
                    break;
                }
                TokenKind::Semicolon | TokenKind::Newline => {
                    self.advance()?;
                }
                _ => {
                    stmts.push(self.statement()?);
                }
            }
        }
        Ok(stmts)
    }

    pub fn new(lexer: &mut Lexer) -> Parser {
        Parser {
            previous: None,
            current: None,
            lexer,
        }
    }

    pub fn parse(&mut self) -> Result<Statements, Error> {
        self.advance()?;
        self.skip_newlines()?;
        self.advance()?;
        self.skip_newlines()?;
        self.statements()
    }
}
