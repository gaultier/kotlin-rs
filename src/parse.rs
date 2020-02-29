use crate::lex::{Lexer, Token};
use std::option::Option;

type Precedence = u8;
const PREC_NONE: Precedence = 0;
const PREC_ASSIGNEMENT: Precedence = 1; // =
const PREC_OR: Precedence = 2;
const PREC_AND: Precedence = 3;
const PREC_EQUALITY: Precedence = 4; // == !=
const PREC_COMPARISON: Precedence = 5; // < > <= >=
const PREC_TERM: Precedence = 6; // + -
const PREC_FACTOR: Precedence = 7; // * /
const PREC_UNARY: Precedence = 8; // ! -
const PREC_CALL: Precedence = 9; // . ()
const PREC_PRIMARY: Precedence = 10;

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
    let previous_type = &parser.previous.as_ref().unwrap().kind;
    let previous_precedence = RULES[usize::from(previous_type)].precedence;
    parser.precedence(previous_precedence + 1);
}

const RULES: [ParseRule; 2] = [
    // Plus
    ParseRule {
        precedence: PREC_TERM,
        infix: Some(binary),
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: Some(binary),
        prefix: None,
    },
];

impl<'a> Parser<'a> {
    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            let cur = self.lexer.lex();
            if let Ok(cur) = cur {
                self.current = Some(cur);
                return;
            } else {
                println!("Lexer error: {:?}", self.current);
                // TODO: error
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current
            .as_ref()
            .map(|cur| cur.is_eof())
            .unwrap_or(true)
    }

    fn precedence(&mut self, precedence: Precedence) {
        dbg!(&self.current);
        self.advance();
        dbg!(&self.current);

        let kind = &self.current.as_ref().unwrap().kind;
        let prefix_rule = RULES[usize::from(kind)].prefix.unwrap();
        prefix_rule(self);

        let mut current_precedence = PREC_NONE;
        while !self.is_at_end() && precedence <= current_precedence {
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
        let s = "1 + 2";
        let mut parser = Parser {
            previous: None,
            current: None,
            lexer: Lexer::new(&s),
        };
        parser.advance();
        parser.precedence(PREC_ASSIGNEMENT);
    }
}
