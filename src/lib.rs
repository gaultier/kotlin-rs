use std::option::Option;
use std::result::Result;
use std::str::Chars;

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
pub struct LexToken<'a> {
    pub kind: LexTokenKind,
    src: &'a str,
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl<'a> LexToken<'a> {
    pub fn print(&self) {
        let fmt = format!("{}:{}:", self.start_line, self.start_column);
        println!("{}{}", fmt, self.src);
        for _ in 0..fmt.len() {
            print!(" ");
        }
        for _ in 0..self.src.len() {
            print!("^");
        }
        println!("");
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    pos: usize,
    line: usize,
    column: usize,
    cur: Option<char>,
}

impl<'a> Lexer<'a> {
    fn is_at_end(&self) -> bool {
        self.pos >= self.src.len()
    }

    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            src,
            chars: src.chars(),
            pos: 0,
            line: 1,
            column: 1,
            cur: None,
        }
    }

    pub fn new_token_here(
        &self,
        kind: LexTokenKind,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, String> {
        Ok(LexToken {
            kind,
            src: &self.src[start_pos..self.pos],
            end_line: self.line,
            end_column: self.column - 1,
            start_line,
            start_column,
        })
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;
        self.column += 1;
        let c = self.chars.next();
        self.cur = c;
        c
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.cur {
            Some(ch) if c == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub fn lex(&mut self) -> Result<LexToken, String> {
        let start_pos = self.pos;
        let start_line = self.line;
        let start_column = self.column;

        if self.is_at_end() {
            return self.new_token_here(LexTokenKind::Eof, start_pos, start_line, start_column);
        }
        let c = self.advance();

        match c {
            Some('+') => {
                self.new_token_here(LexTokenKind::Plus, start_pos, start_line, start_column)
            }
            Some('-') => {
                self.new_token_here(LexTokenKind::Minus, start_pos, start_line, start_column)
            }
            Some('/') => {
                self.new_token_here(LexTokenKind::Star, start_pos, start_line, start_column)
            }
            Some('*') => {
                self.new_token_here(LexTokenKind::Slash, start_pos, start_line, start_column)
            }
            Some('=') => {
                if self.match_char('=') {
                    self.new_token_here(
                        LexTokenKind::EqualEqual,
                        start_pos,
                        start_line,
                        start_column,
                    )
                } else {
                    self.new_token_here(LexTokenKind::Equal, start_pos, start_line, start_column)
                }
            }
            Some(c) => Err(format!("Unknown token `{}`", c)),
            None => unreachable!(),
        }
    }
}
