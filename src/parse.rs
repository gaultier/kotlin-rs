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
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn primary(&mut self) -> Result<AstNodeExpr, String> {
        let tok = self.lexer.lex().unwrap(); // FIXME
        dbg!(&tok);
        match tok.kind {
            TokenKind::Int(_) => Ok(AstNodeExpr::Literal(tok)),
            _ => unimplemented!(),
        }
    }

    // fn multiplication(&mut self) -> Result<AstNodeExpr, String> {
    //     let left = self.primary().unwrap(); // FIXME
    //     Ok(left)
    // }

    fn addition(&mut self) -> Result<AstNodeExpr, String> {
        let left = self.primary().unwrap(); // FIXME
        let tok = self.lexer.lex().unwrap(); // FIXME
        dbg!(&tok);
        match tok.kind {
            TokenKind::Plus => {
                let right = self.addition().unwrap(); // FIXME
                Ok(AstNodeExpr::Binary(Box::new(left), tok, Box::new(right)))
            }
            _ => Ok(left),
        }
    }

    fn expression(&mut self) -> Result<AstNodeExpr, String> {
        self.addition()
    }

    pub fn new(s: &str) -> Parser {
        Parser {
            lexer: Lexer::new(s),
        }
    }

    pub fn parse(&mut self) -> Result<AstNodeExpr, String> {
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
