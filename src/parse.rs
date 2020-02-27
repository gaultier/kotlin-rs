use crate::lex::{Lexer, Token};
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

fn test() {
    let rule_plus = ParseRule {
        precedence: Precedence::Term,
        infix: Some(&binary),
        prefix: None,
    };
}
