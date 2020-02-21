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
        println!();
    }

    pub fn new<'b>(
        lexer: &'b Lexer<'a>,
        kind: LexTokenKind,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> LexToken<'a> {
        let src = if kind == LexTokenKind::Eof {
            &lexer.src[0..=0]
        } else {
            &lexer.src[start_pos - 1..lexer.pos - 1]
        };

        LexToken {
            kind,
            src,
            end_line: lexer.line,
            end_column: lexer.column - 2,
            start_line,
            start_column: start_column - 2,
        }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    pos: usize,
    line: usize,
    column: usize,
    cur: [Option<char>; 2],
}

impl<'a> Lexer<'a> {
    fn is_at_end(&self) -> bool {
        self.pos >= (self.src.len() + 2)
    }

    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            src,
            chars: src.chars(),
            pos: 0,
            line: 1,
            column: 1,
            cur: [None, None],
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;
        self.column += 1;
        self.cur[0] = self.cur[1];
        self.cur[1] = self.chars.next();
        self.cur[0]
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.cur[0] {
            Some(ch) if c == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.cur[1] {
                None | Some(' ') | Some('\t') => {
                    self.advance();
                }
                Some(_) => {
                    break;
                }
            }
        }
    }

    pub fn lex(&mut self) -> Result<LexToken, String> {
        self.skip_whitespace();
        let start_pos = self.pos;
        let start_line = self.line;
        let start_column = self.column;

        dbg!(self.cur);
        let c = self.advance();
        dbg!(self.cur);

        match c {
            Some('+') => Ok(LexToken::new(
                &self,
                LexTokenKind::Plus,
                start_pos,
                start_line,
                start_column,
            )),
            Some('-') => Ok(LexToken::new(
                &self,
                LexTokenKind::Minus,
                start_pos,
                start_line,
                start_column,
            )),
            Some('/') => Ok(LexToken::new(
                &self,
                LexTokenKind::Star,
                start_pos,
                start_line,
                start_column,
            )),
            Some('*') => Ok(LexToken::new(
                &self,
                LexTokenKind::Slash,
                start_pos,
                start_line,
                start_column,
            )),
            Some('=') => {
                if self.match_char('=') {
                    Ok(LexToken::new(
                        &self,
                        LexTokenKind::EqualEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(LexToken::new(
                        &self,
                        LexTokenKind::Equal,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some(c) => Err(format!("Unknown token `{}`", c)),
            None => Ok(LexToken::new(
                &self,
                LexTokenKind::Eof,
                start_pos,
                start_line,
                start_column,
            )),
        }
    }
}
