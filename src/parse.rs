use crate::lex::{Lexer, Token};
use std::option::Option;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Precedence {
    PNone = 0,
    Assignement = 1, // =
    Or = 2,
    And = 3,
    Equality = 4,   // == !=
    Comparison = 5, // < > <= >=
    Term = 6,       // + -
    Factor = 7,     // * /
    Unary = 8,      // ! -
    Call = 9,       // . ()
    Primary = 10,
}

#[derive(Debug)]
struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: Lexer<'a>,
}

type ParseFn = fn(&mut Parser) -> ();

struct ParseRule {
    precedence: Precedence,
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
}

fn binary(parser: &mut Parser) {
    println!(
        "binary: prev={:?} cur={:?}",
        parser.previous, parser.current
    );
}

const RULES: [ParseRule; 2] = [
    ParseRule {
        precedence: Precedence::Term,
        infix: Some(binary),
        prefix: None,
    },
    ParseRule {
        precedence: Precedence::Factor,
        infix: Some(binary),
        prefix: None,
    },
];

impl<'a> Parser<'a> {
    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            let current = self.lexer.lex();
            if current.is_ok() {
                return;
            }
            println!("Lexer error: {:?}", current);
            // TODO: error
        }
    }

    fn is_at_end(&self) -> bool {
        self.current
            .as_ref()
            .map(|cur| cur.is_eof())
            .unwrap_or(false)
    }

    fn precedence(&mut self, precedence: Precedence) {
        self.advance();

        let kind = &self.current.as_ref().unwrap().kind;
        let prefix_rule = RULES[usize::from(kind)].prefix.unwrap();
        prefix_rule(self);

        let mut current_precedence = Precedence::PNone;
        while !self.is_at_end() && (precedence as u8) <= (current_precedence as u8) {
            self.advance();

            let index = usize::from(&self.current.as_ref().unwrap().kind);
            current_precedence = RULES[index].precedence;
            let infix_rule = RULES[index].infix.unwrap();
            infix_rule(self);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t() {
        dbg!(&RULES[usize::from(TokenKind::Plus)].precedence);
    }
}
