use crate::lex::{Lexer,Token};
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
