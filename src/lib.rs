use std::option::Option;
use std::result::Result;

#[derive(Debug, Eq, PartialEq)]
pub enum LexTokenKind {
    Plus,
    Minus,
    Slash,
    Star,
    Equal,
    EqualEqual,
    Eof,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LexToken {
    pub kind: LexTokenKind,
    start_line: usize,
    start_column: usize,
    start_pos: usize,
    end_line: usize,
    end_column: usize,
    end_pos: usize,
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

    pub fn new_token_here(&self, kind: LexTokenKind) -> Result<LexToken, String> {
        Ok(LexToken {
            kind,
            end_pos: self.pos - 1,
            end_column: self.column - 1,
            end_line: self.line,
            start_pos: 0,
            start_column: 0,
            start_line: 0,
        })
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
            return self.new_token_here(LexTokenKind::Eof);
        }
        let c = self.advance();

        match c {
            Some("+") => self.new_token_here(LexTokenKind::Plus),
            Some("-") => self.new_token_here(LexTokenKind::Minus),
            Some("/") => self.new_token_here(LexTokenKind::Star),
            Some("*") => self.new_token_here(LexTokenKind::Slash),
            Some("=") => {
                if self.match_char("=") {
                    self.new_token_here(LexTokenKind::EqualEqual)
                } else {
                    self.new_token_here(LexTokenKind::Equal)
                }
            }
            Some(c) => Err(format!("Unknown token `{}`", c)),
            None => unreachable!(),
        }
    }
}
