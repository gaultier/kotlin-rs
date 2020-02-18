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
    if *i > src.len() - 1 {
        return Ok(LexToken::Eof);
    }

    match &src[*i..*i + 1] {
        "+" => {
            *i = *i + 1;
            Ok(LexToken::Plus)
        }
        "-" => {
            *i = *i + 1;
            Ok(LexToken::Minus)
        }
        "/" => {
            *i = *i + 1;
            Ok(LexToken::Star)
        }
        "*" => {
            *i = *i + 1;
            Ok(LexToken::Slash)
        }
        "=" => {
            *i = *i + 1;
            Ok(LexToken::Equal)
        }
        c => Err(format!("Unknown token `{}`", c)),
    }
}
