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

    pub fn new(
        lexer: &'a Lexer,
        kind: LexTokenKind,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> LexToken<'a> {
        LexToken {
            kind,
            src: &lexer.src[start_pos..lexer.pos as usize],
            end_line: lexer.line,
            end_column: lexer.column as usize,
            start_line,
            start_column: start_column,
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

        self.pos += 1;
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

    fn number(&mut self) -> Result<isize, String> {
        Ok(0)
    }

    fn skip_whitespace(&mut self) -> Result<(), String> {
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
                        if self.line != 1 {
                            return Err(format!(
                                "Shebang can only be located on the first line: {}{}",
                                self.peek().unwrap(),
                                self.peek_next().unwrap()
                            ));
                        }
                        self.skip_until('\n');
                    }
                    _ => {
                        return Ok(());
                    }
                },
                // TODO: add option to store comments in the ast
                Some('/') => match self.peek_next() {
                    Some('/') => {
                        self.skip_until('\n');
                    }
                    _ => {
                        return Ok(());
                    }
                },
                Some(_) => {
                    return Ok(());
                }
            }
        }
        Ok(())
    }

    pub fn lex(&mut self) -> Option<Result<LexToken, String>> {
        if let Err(err) = self.skip_whitespace() {
            return Some(Err(err));
        }

        let start_pos = self.pos as usize;
        let start_line = self.line;
        let start_column = self.column as usize;

        // dbg!(self.cur);
        let c = self.advance();
        // dbg!(self.cur);

        match c {
            Some('+') => Some(Ok(LexToken::new(
                self,
                LexTokenKind::Plus,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('-') => Some(Ok(LexToken::new(
                self,
                LexTokenKind::Minus,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('/') => Some(Ok(LexToken::new(
                self,
                LexTokenKind::Star,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('*') => Some(Ok(LexToken::new(
                self,
                LexTokenKind::Slash,
                start_pos,
                start_line,
                start_column,
            ))),
            Some('=') => {
                if self.match_char('=') {
                    Some(Ok(LexToken::new(
                        self,
                        LexTokenKind::EqualEqual,
                        start_pos,
                        start_line,
                        start_column,
                    )))
                } else {
                    Some(Ok(LexToken::new(
                        self,
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
