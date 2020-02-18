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

    fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn lex(&mut self) -> Result<LexToken, String> {
        dbg!(&self.src);
        if self.is_at_end() {
            return Ok(LexToken::Eof);
        }

        match self.cur_char() {
            Some("+") => {
                self.advance();
                Ok(LexToken::Plus)
            }
            Some("-") => {
                self.advance();
                Ok(LexToken::Minus)
            }
            Some("/") => {
                self.advance();
                Ok(LexToken::Star)
            }
            Some("*") => {
                self.advance();
                Ok(LexToken::Slash)
            }
            Some("=") => {
                self.advance();
                Ok(LexToken::Equal)
            }
            Some(c) => Err(format!("Unknown token `{}`", c)),
            None => unreachable!(),
        }
    }
}
