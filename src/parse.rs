use crate::lex::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub enum AstNodeStmt {
    Expr(AstNodeExpr),
}

#[derive(Debug)]
pub enum AstNodeExpr {
    Binary(Box<AstNodeExpr>, Token, Box<AstNodeExpr>),
    Literal(Token),
}

#[derive(Debug)]
pub struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn advance(&mut self) -> Result<(), Token> {
        self.previous = self.current.clone();
        self.current = Some(self.lexer.lex()?);
        Ok(())
    }

    fn primary(&mut self) -> Result<AstNodeExpr, String> {
        dbg!(&self.previous);
        let previous = self.previous.clone().unwrap();
        match previous.kind {
            TokenKind::Int(_) => {
                self.advance().unwrap();
                Ok(AstNodeExpr::Literal(previous))
            }
            _ => unimplemented!(),
        }
    }

    fn multiplication(&mut self) -> Result<AstNodeExpr, String> {
        let left = self.primary().unwrap(); // FIXME
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Star | TokenKind::Slash => {
                self.advance().unwrap();
                let right = self.multiplication().unwrap(); //FIXME
                Ok(AstNodeExpr::Binary(
                    Box::new(left),
                    previous,
                    Box::new(right),
                ))
            }
            _ => Ok(left),
        }
    }

    fn addition(&mut self) -> Result<AstNodeExpr, String> {
        let left = self.multiplication().unwrap(); // FIXME
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Plus | TokenKind::Minus => {
                self.advance().unwrap();
                let right = self.addition().unwrap(); // FIXME
                Ok(AstNodeExpr::Binary(
                    Box::new(left),
                    previous,
                    Box::new(right),
                ))
            }
            _ => Ok(left),
        }
    }

    fn comparison(&mut self) -> Result<AstNodeExpr, String> {
        let left = self.addition().unwrap(); // FIXME
        let previous = self.previous.clone().unwrap();
        dbg!(&self.previous);
        match previous.kind {
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Lesser
            | TokenKind::LesserEqual => {
                self.advance().unwrap();
                let right = self.comparison().unwrap(); // FIXME
                Ok(AstNodeExpr::Binary(
                    Box::new(left),
                    previous,
                    Box::new(right),
                ))
            }
            _ => Ok(left),
        }
    }

    fn expression(&mut self) -> Result<AstNodeExpr, String> {
        self.comparison()
    }

    pub fn new(s: &str) -> Parser {
        Parser {
            previous: None,
            current: None,
            lexer: Lexer::new(s),
        }
    }

    pub fn parse(&mut self) -> Result<AstNodeExpr, String> {
        self.advance().unwrap();
        self.advance().unwrap(); // FIXME
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
        let ast = parser.expression();
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
