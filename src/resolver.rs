use crate::error::*;
use crate::lex::{Lexer, Token, TokenKind};
use crate::parse::*;

pub(crate) struct Resolver<'a> {
    lexer: &'a Lexer,
    resolution: Resolution,
}

pub(crate) struct Resolution {}

impl Resolver<'_> {
    pub(crate) fn new(lexer: &Lexer) -> Resolver {
        Resolver {
            lexer,
            resolution: Resolution {},
        }
    }

    pub(crate) fn statements(&mut self, statements: &BlockSlice) -> Result<Resolution, Error> {
        unimplemented!()
    }
}
