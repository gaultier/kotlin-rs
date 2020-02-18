use std::result::Result;

#[derive(Debug)]
pub enum LexToken {
    Plus,
    Minus,
    Slash,
    Star,
    Equal,
    EqualEqual,
    Eof,
}

pub fn lex(_src: &str) -> Result<LexToken, String> {
    Ok(LexToken::Plus)
}
