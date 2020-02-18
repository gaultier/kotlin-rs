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

pub fn lex(src: &str) -> Result<LexToken, String> {
    if src.is_empty() {
        return Ok(LexToken::Eof);
    }
    dbg!(src);

    match &src[0..1] {
        "+" => Ok(LexToken::Plus),
        "-" => Ok(LexToken::Minus),
        "/" => Ok(LexToken::Star),
        "*" => Ok(LexToken::Slash),
        c => Err(format!("Unknown token `{}`", c)),
    }
}
