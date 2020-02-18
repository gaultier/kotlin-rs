use std::result::Result;

#[derive(Debug, Eq, PartialEq)]
pub enum LexToken {
    Plus,
    Minus,
    Slash,
    Star,
    Equal,
    EqualEqual,
    Eof,
}

pub fn lex(src: &str, i: &mut usize) -> Result<LexToken, String> {
    dbg!(src);
    if *i == src.len() - 1 {
        return Ok(LexToken::Eof);
    }

    match &src[*i..*i + 1] {
        "+" => {
            *i = *i + 1;
            Ok(LexToken::Plus)
        }
        "-" => Ok(LexToken::Minus),
        "/" => Ok(LexToken::Star),
        "*" => Ok(LexToken::Slash),
        c => Err(format!("Unknown token `{}`", c)),
    }
}
