use crate::lex::{Lexer, Token, TokenKind};
use std::option::Option;

#[derive(Debug, Eq, PartialEq)]
enum Precedence {
    PNone,
    Assignement, // =
    Or,
    And,
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

#[derive(Debug)]
struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: Lexer<'a>,
}

type ParseFn = dyn FnMut(&mut Parser) -> ();

struct ParseRule {
    precedence: Precedence,
    prefix: Option<&'static ParseFn>,
    infix: Option<&'static ParseFn>,
}

fn binary(parser: &mut Parser) {}

const RULES: [ParseRule; 2] = [
    ParseRule {
        precedence: Precedence::Term,
        infix: Some(&binary),
        prefix: None,
    },
 ParseRule {
    precedence: Precedence::Factor,
    infix: Some(&binary),
    prefix: None,
}    
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t() {
        dbg!(&RULES[usize::from(TokenKind::Plus)].precedence);
    }
}
