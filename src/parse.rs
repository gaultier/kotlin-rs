use crate::lex::{Lexer, Token, TokenError, TokenKind, TokenKindError};

#[derive(Debug)]
pub enum AstNodeStmt {
    Expr(AstNodeExpr),
}

#[derive(Debug)]
pub enum AstNodeExpr {
    Binary(Box<AstNodeExpr>, Token, Box<AstNodeExpr>),
    Unary(Token, Box<AstNodeExpr>),
    Literal(Token),
}

#[derive(Debug)]
pub struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn advance(&mut self) -> Result<(), TokenError> {
        self.previous = self.current.clone();
        self.current = Some(self.lexer.lex()?);
        Ok(())
    }

    fn primary(&mut self) -> Result<AstNodeExpr, TokenError> {
        dbg!(&self.previous);
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Int(_)
            | TokenKind::Long(_)
            | TokenKind::UInt(_)
            | TokenKind::ULong(_)
            | TokenKind::Float(_)
            | TokenKind::Double(_)
            | TokenKind::Bool(_)
            | TokenKind::UnicodeLiteral(_) => {
                self.advance()?;
                Ok(AstNodeExpr::Literal(previous))
            }
            _ => Err(TokenError::new(
                TokenKindError::ExpectedPrimary(previous.clone()),
                previous.start_pos,
                previous.start_line,
                previous.start_column,
                previous.end_pos,
                previous.end_line,
                previous.end_column,
            )),
        }
    }

    fn unary(&mut self) -> Result<AstNodeExpr, TokenError> {
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Bang | TokenKind::Minus => {
                self.advance()?;
                let right = self.unary()?;
                Ok(AstNodeExpr::Unary(previous, Box::new(right)))
            }
            _ => self.primary(),
        }
    }

    fn multiplication(&mut self) -> Result<AstNodeExpr, TokenError> {
        let left = self.unary()?;
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Star | TokenKind::Slash => {
                self.advance()?;
                let right = self.multiplication()?;
                Ok(AstNodeExpr::Binary(
                    Box::new(left),
                    previous,
                    Box::new(right),
                ))
            }
            _ => Ok(left),
        }
    }

    fn addition(&mut self) -> Result<AstNodeExpr, TokenError> {
        let left = self.multiplication()?;
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Plus | TokenKind::Minus => {
                self.advance()?;
                let right = self.addition()?;
                Ok(AstNodeExpr::Binary(
                    Box::new(left),
                    previous,
                    Box::new(right),
                ))
            }
            _ => Ok(left),
        }
    }

    fn comparison(&mut self) -> Result<AstNodeExpr, TokenError> {
        let left = self.addition()?;
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Lesser
            | TokenKind::LesserEqual => {
                self.advance()?;
                let right = self.comparison()?;
                Ok(AstNodeExpr::Binary(
                    Box::new(left),
                    previous,
                    Box::new(right),
                ))
            }
            _ => Ok(left),
        }
    }

    fn expression(&mut self) -> Result<AstNodeExpr, TokenError> {
        self.comparison()
    }

    pub fn new(s: &str) -> Parser {
        Parser {
            previous: None,
            current: None,
            lexer: Lexer::new(s),
        }
    }

    pub fn parse(&mut self) -> Result<AstNodeExpr, TokenError> {
        self.advance()?;
        self.advance()?;
        self.expression()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t() {
        let s = "1 + 2";
        let mut parser = Parser::new(&s);
        let ast = parser.parse();
        assert!(ast.is_ok());
        let ast = ast.as_ref().unwrap();
        match ast {
            AstNodeExpr::Binary(left, op, right) => {
                assert!(true);
            }
            _ => assert!(false),
        }
    }
}
