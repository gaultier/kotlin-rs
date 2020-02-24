use std::iter::Iterator;
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
        LexToken {
            kind,
            src: &lexer.src[start_pos - 2..lexer.pos - 2],
            end_line: lexer.line,
            end_column: lexer.column - 3,
            start_line,
            start_column: start_column - 3,
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
        let c = self.peek();
        self.cur[0] = self.cur[1];
        self.cur[1] = self.chars.next();
        c
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        match self.peek() {
            Some(ch) if c == ch => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn peek(&self) -> Option<char> {
        self.cur[0]
    }

    fn peek_next(&self) -> Option<char> {
        self.cur[1]
    }

    fn skip_until(&mut self, c: char) {
        while !self.is_at_end() {
            match self.peek() {
                Some(ch) if c != ch => {
                    self.advance();
                } // keep skipping
                _ => {
                    break;
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                None | Some(' ') | Some('\t') | Some('\r') | Some('\n') => {
                    self.advance();
                }
                // TODO: check that the shebang is on the first line
                Some('#') => match self.peek_next() {
                    Some('!') => {
                        self.skip_until('\n');
                    }
                    _ => {}
                },
                // TODO: add option to store comments in the ast
                Some('/') => match self.peek_next() {
                    Some('/') => {
                        self.skip_until('\n');
                    }
                    _ => {}
                },
                Some(_) => {
                    break;
                }
            }
        }
    }

    pub fn lex(&mut self) -> Option<Result<LexToken, String>> {
        self.skip_whitespace();
        let start_pos = self.pos;
        let start_line = self.line;
        let start_column = self.column;

        dbg!(self.cur);
        let c = self.advance();
        dbg!(self.cur);

        match c {
            Some('+') => Some(Ok(LexToken::new(
                &self,
                LexTokenKind::Plus,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('-') => Some(Ok(LexToken::new(
                &self,
                LexTokenKind::Minus,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('/') => Some(Ok(LexToken::new(
                &self,
                LexTokenKind::Star,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('*') => Some(Ok(LexToken::new(
                &self,
                LexTokenKind::Slash,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('=') => {
                if self.match_char('=') {
                    Some(Ok(LexToken::new(
                        &self,
                        LexTokenKind::EqualEqual,
                        start_pos,
                        start_line,
                        start_column,
                    )))
                } else {
                    Some(Ok(LexToken::new(
                        &self,
                        LexTokenKind::Equal,
                        start_pos,
                        start_line,
                        start_column,
                    )))
                }
            }
            Some(c) => Some(Err(format!("Unknown token `{}`", c))),
            None => None,
        }
    }
}

impl<'b> Iterator for Lexer<'b> {
    type Item = Result<LexToken<'b>, String>;

    fn next<'a: 'b>(&'a mut self) -> Option<Self::Item> {
        self.lex()
    }
}
