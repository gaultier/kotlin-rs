use std::option::Option;
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

pub struct Lexer {
    src: String,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    fn is_at_end(&self) -> bool {
        self.pos >= self.src.len()
    }

    pub fn new(src: String) -> Lexer {
        Lexer {
            src,
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    fn cur_char(&self) -> Option<&str> {
        self.src.get(self.pos..self.pos + 1)
    }

    fn advance(&mut self) -> Option<&str> {
        self.pos += 1;
        self.column += 1;
        self.src.get(self.pos - 1..self.pos)
    }

    fn match_char(&mut self, c: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.cur_char() {
            Some(ch) if c == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub fn lex(&mut self) -> Result<LexToken, String> {
        dbg!(&self.src);
        if self.is_at_end() {
            return Ok(LexToken::Eof);
        }
        let c = self.advance();

        match c {
            Some("+") => Ok(LexToken::Plus),
            Some("-") => Ok(LexToken::Minus),
            Some("/") => Ok(LexToken::Star),
            Some("*") => Ok(LexToken::Slash),
            Some("=") => {
                if self.match_char("=") {
                    Ok(LexToken::EqualEqual)
                } else {
                    Ok(LexToken::Equal)
                }
            }
            Some(c) => Err(format!("Unknown token `{}`", c)),
            None => unreachable!(),
        }
    }
}
