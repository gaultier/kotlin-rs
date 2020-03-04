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
    Expr(AstNode, Token),
}

pub type Statements = Vec<AstNodeStmt>;

#[derive(Debug)]
pub struct AstNode {
    pub kind: AstNodeExpr,
    pub type_info: Option<Type>,
}

#[derive(Debug)]
pub enum AstNodeExpr {
    Binary(Box<AstNode>, Token, Box<AstNode>),
    Unary(Token, Box<AstNode>),
    Literal(Token),
}

#[derive(Debug)]
pub struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn advance(&mut self) -> Result<(), Error> {
        self.previous = self.current.clone();
        self.current = Some(self.lexer.lex()?);
        Ok(())
    }

    fn primary(&mut self) -> Result<AstNode, Error> {
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Int(_)
            | TokenKind::Long(_)
            | TokenKind::UInt(_)
            | TokenKind::ULong(_)
            | TokenKind::Float(_)
            | TokenKind::Double(_)
            | TokenKind::Bool(_)
            | TokenKind::TString
            | TokenKind::Null
            | TokenKind::UnicodeLiteral(_) => {
                self.advance()?;
                // TODO: fill type info here right away
                Ok(AstNode {
                    kind: AstNodeExpr::Literal(previous),
                    type_info: None,
                })
            }
            _ => Err(Error::new(
                ErrorKind::ExpectedPrimary,
                previous.location.start_pos,
                previous.location.start_line,
                previous.location.start_column,
                previous.location.end_pos,
                previous.location.end_line,
                previous.location.end_column,
            )),
        }
    }

    fn unary(&mut self) -> Result<AstNode, Error> {
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Bang | TokenKind::Minus => {
                self.advance()?;
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
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Star | TokenKind::Slash => {
                self.advance()?;
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
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Plus | TokenKind::Minus => {
                self.advance()?;
                let right = self.addition()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    type_info: None,
                })
            }
            _ => Ok(left),
        }
    }

    fn comparison(&mut self) -> Result<AstNode, Error> {
        let left = self.addition()?;
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Lesser
            | TokenKind::LesserEqual => {
                self.advance()?;
                let right = self.comparison()?;
                Ok(AstNode {
                    kind: AstNodeExpr::Binary(Box::new(left), previous, Box::new(right)),
                    type_info: None,
                })
            }
            _ => Ok(left),
        }
    }

    fn expression(&mut self) -> Result<AstNode, Error> {
        self.comparison()
    }

    fn expression_stmt(&mut self) -> Result<AstNodeStmt, Error> {
        let expr = self.expression()?;
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Semicolon | TokenKind::Newline => {
                self.advance()?;
                Ok(AstNodeStmt::Expr(expr, previous))
            }
            _ => Err(Error::new(
                ErrorKind::UnterminatedStatement,
                previous.location.start_pos,
                previous.location.start_line,
                previous.location.start_column,
                previous.location.end_pos,
                previous.location.end_line,
                previous.location.end_column,
            )),
        }
    }

    pub fn new(s: &str) -> Parser {
        Parser {
            previous: None,
            current: None,
            lexer: Lexer::new(s),
        }
    }

    pub fn parse(&mut self) -> Result<Statements, Error> {
        self.advance()?;
        self.advance()?;
        Ok(vec![self.expression_stmt()?])
    }
}
