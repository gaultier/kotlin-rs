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
    Int(i32),
    Long(i64),
    Shebang,
    Eof,
    // Errors
    Unknown,
    ShebangNotOnFirstLine,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LexToken {
    pub kind: LexTokenKind,
    start_pos: usize,
    start_line: usize,
    start_column: usize,
    end_pos: usize,
    end_line: usize,
    end_column: usize,
}

impl LexToken {
    pub fn print(&self, src: &str) {
        let fmt = format!("{}:{}:", self.start_line, self.start_column);
        println!("{}{}", fmt, &src[self.start_pos..self.end_pos]);
        for _ in 0..fmt.len() {
            print!(" ");
        }
        for _ in self.start_pos..self.end_pos {
            print!("^");
        }
        println!();
    }

    pub fn new(
        lexer: &Lexer,
        kind: LexTokenKind,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> LexToken {
        LexToken {
            kind,
            end_pos: lexer.pos as usize,
            end_line: lexer.line,
            end_column: lexer.column as usize,
            start_pos,
            start_line,
            start_column,
        }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    pos: isize,
    line: usize,
    column: isize,
    cur: [Option<char>; 2],
}

impl<'a> Lexer<'a> {
    fn is_at_end(&self) -> bool {
        self.pos >= (self.src.len() as isize + 2)
    }

    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            src,
            chars: src.chars(),
            pos: -2,
            line: 1,
            column: -1,
            cur: [None, None],
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        self.cur[0] = self.cur[1];
        self.cur[1] = self.chars.next();

        self.pos += c.map(|c| c.len_utf8()).unwrap_or(1) as isize;
        self.column += 1;
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

    fn newline(&mut self) {
        match self.peek() {
            Some('\n') => {
                self.advance();
                self.line += 1;
                self.column = 1;
            }
            _ => unreachable!(),
        }
    }

    fn number(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, LexToken> {
        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }
        if self.peek() == Some('L') {
            let s = &self.src[start_pos..self.pos as usize];
            let n: i64 = s.parse().unwrap();
            self.advance();
            Ok(LexToken::new(
                self,
                LexTokenKind::Long(n),
                start_pos,
                start_line,
                start_column,
            ))
        } else {
            let s = &self.src[start_pos..self.pos as usize];
            let n: i32 = s.parse().unwrap();
            Ok(LexToken::new(
                self,
                LexTokenKind::Int(n),
                start_pos,
                start_line,
                start_column,
            ))
        }
    }

    fn skip_whitespace(&mut self) -> Result<Option<LexToken>, LexToken> {
        while !self.is_at_end() {
            match self.peek() {
                None | Some(' ') | Some('\t') | Some('\r') => {
                    self.advance();
                }
                Some('\n') => {
                    self.newline();
                }
                Some('#') => match self.peek_next() {
                    Some('!') => {
                        let start_pos = self.pos as usize;
                        let start_line = self.line;
                        let start_column = self.column as usize;
                        if self.line != 1 {
                            // Only to get the right `end_pos`, `end_column`
                            self.skip_until('\n');
                            return Err(LexToken::new(
                                self,
                                LexTokenKind::ShebangNotOnFirstLine,
                                start_pos,
                                start_line,
                                start_column,
                            ));
                        }
                        self.skip_until('\n');
                        return Ok(Some(LexToken::new(
                            self,
                            LexTokenKind::Shebang,
                            start_pos,
                            start_line,
                            start_column,
                        )));
                    }
                    _ => {
                        return Ok(None);
                    }
                },
                // TODO: add option to store comments in the ast
                Some('/') => match self.peek_next() {
                    Some('/') => {
                        self.skip_until('\n');
                    }
                    _ => {
                        return Ok(None);
                    }
                },
                Some(_) => {
                    return Ok(None);
                }
            }
        }
        Ok(None)
    }

    pub fn lex(&mut self) -> Result<LexToken, LexToken> {
        if let Some(tok) = self.skip_whitespace()? {
            return Ok(tok);
        }

        let start_pos = self.pos as usize;
        let start_line = self.line;
        let start_column = self.column as usize;

        // dbg!(self.cur);
        let c = self.advance();
        // dbg!(self.cur);

        match c {
            Some('+') => Ok(LexToken::new(
                self,
                LexTokenKind::Plus,
                start_pos,
                start_line,
                start_column,
            )),
            Some('-') => Ok(LexToken::new(
                self,
                LexTokenKind::Minus,
                start_pos,
                start_line,
                start_column,
            )),
            Some('/') => Ok(LexToken::new(
                self,
                LexTokenKind::Star,
                start_pos,
                start_line,
                start_column,
            )),
            Some('*') => Ok(LexToken::new(
                self,
                LexTokenKind::Slash,
                start_pos,
                start_line,
                start_column,
            )),
            Some('=') => {
                if self.match_char('=') {
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::EqualEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::Equal,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('0') | Some('1') | Some('2') | Some('3') | Some('4') | Some('5') | Some('6')
            | Some('7') | Some('8') | Some('9') => self.number(start_pos, start_line, start_column),
            Some(_) => Ok(LexToken::new(
                self,
                LexTokenKind::Unknown,
                start_pos,
                start_line,
                start_column,
            )),
            None => Ok(LexToken::new(
                self,
                LexTokenKind::Eof,
                start_pos,
                start_line,
                start_column,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_number_int() {
        let s = " 123  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Int(123));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);
    }

    #[test]
    fn test_lex_number_long() {
        let s = " 123L  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Long(123i64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);
    }

    #[test]
    fn test_lex_shebang() {
        let s = "#!/bin/cat\n+";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Shebang);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 11);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Plus);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn test_lex_comment() {
        let s = "//bin/cat\n+";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Plus);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn test_lex_unknown() {
        let s = "§+~";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Unknown);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn test_lex_shebang_not_on_first_line() {
        let s = "\n#!/bin/cat\n+";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, LexTokenKind::ShebangNotOnFirstLine);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 11);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Plus);
        assert_eq!(tok.start_line, 3);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 3);
        assert_eq!(tok.end_column, 2);
    }
}
