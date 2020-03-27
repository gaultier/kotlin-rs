// use crate::lex::{Lexer, Span, Token, TokenKind};
use crate::parse::*;
// use crate::resolver::Resolution;
// use log::debug;

pub(crate) struct MirTransformer {}

impl MirTransformer {
    pub(crate) fn new() -> MirTransformer {
        MirTransformer {}
    }

    pub(crate) fn statements(&self, block: &mut Block) {}
}
