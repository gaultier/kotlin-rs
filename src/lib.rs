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
    UInt(u32),
    ULong(u64),
    Shebang,
    Comment,
    TString,
    Bool(bool),
    Null,
    KeywordAbstract,
    KeywordAnnotation,
    KeywordBy,
    KeywordCatch,
    KeywordCompanion,
    KeywordConstructor,
    KeywordCrossinline,
    KeywordData,
    KeywordDynamic,
    KeywordEnum,
    KeywordExternal,
    KeywordFinal,
    KeywordFinally,
    KeywordImport,
    KeywordInfix,
    KeywordInit,
    KeywordInline,
    KeywordInner,
    KeywordInternal,
    KeywordLateinit,
    KeywordNoinline,
    KeywordOpen,
    KeywordOperator,
    KeywordOut,
    KeywordOverride,
    KeywordPrivate,
    KeywordProtected,
    KeywordPublic,
    KeywordReified,
    KeywordSealed,
    KeywordTailrec,
    KeywordVararg,
    KeywordWhere,
    KeywordGet,
    KeywordSet,
    KeywordField,
    KeywordProperty,
    KeywordReceiver,
    KeywordParam,
    KeywordSetparam,
    KeywordDelegate,
    KeywordFile,
    KeywordExpect,
    KeywordActual,
    KeywordConst,
    KeywordSuspend,
    KeywordTypeof,
    KeywordVal,
    KeywordVar,
    KeywordIf,
    KeywordElse,
    KeywordObject,
    KeywordFun,
    KeywordInterface,
    KeywordClass,
    KeywordPackage,
    KeywordIn,
    KeywordTry,
    KeywordWhen,
    KeywordIs,
    KeywordAs,
    KeywordContinue,
    KeywordReturn,
    KeywordThrow,
    KeywordDo,
    KeywordFor,
    Eof,
    // Errors
    Unknown,
    UnexpectedChar(char),
    ShebangNotOnFirstLine,
    NewlineInString,
    TrailingUnderscoreInNumber,
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

    fn bin_digits(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '_' => {
                    self.advance();
                }
                _ => {
                    return;
                }
            }
        }
    }

    fn hex_digits(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'a' | 'b' | 'c'
                | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | '_' => {
                    self.advance();
                }
                _ => {
                    return;
                }
            }
        }
    }

    fn digits(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_' => {
                    self.advance();
                }
                _ => {
                    return;
                }
            }
        }
    }

    fn bin_number(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, LexToken> {
        // Consume `b|B`
        self.advance();

        self.bin_digits();
        let s = &self.src[start_pos + 2..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);

        let last_digit = self.src[start_pos + 2..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(LexToken::new(
                self,
                LexTokenKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
            ));
        }

        match self.peek() {
            Some('L') => {
                let n = i64::from_str_radix(s, 2).unwrap();
                self.advance();
                Ok(LexToken::new(
                    self,
                    LexTokenKind::Long(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
            Some('U') | Some('u') => match self.peek_next() {
                Some('L') => {
                    let n = u64::from_str_radix(s, 2).unwrap();
                    self.advance();
                    self.advance();
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::ULong(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
                _ => {
                    let n = u32::from_str_radix(s, 2).unwrap();
                    self.advance();
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::UInt(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            },
            _ => {
                let n = i64::from_str_radix(s, 2).unwrap();
                if n < std::i32::MAX as i64 {
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::Int(n as i32),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::Long(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
        }
    }

    fn hex_number(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, LexToken> {
        // Consume `x|X`
        self.advance();

        self.hex_digits();
        let s = &self.src[start_pos + 2..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);

        let last_digit = self.src[start_pos + 2..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(LexToken::new(
                self,
                LexTokenKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
            ));
        }

        match self.peek() {
            Some('L') => {
                let n = i64::from_str_radix(s, 16).unwrap();
                self.advance();
                Ok(LexToken::new(
                    self,
                    LexTokenKind::Long(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
            Some('U') | Some('u') => match self.peek_next() {
                Some('L') => {
                    let n = u64::from_str_radix(s, 16).unwrap();
                    self.advance();
                    self.advance();
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::ULong(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
                _ => {
                    let n = u32::from_str_radix(s, 16).unwrap();
                    self.advance();
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::UInt(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            },
            _ => {
                let n = i64::from_str_radix(s, 16).unwrap();
                if n < std::i32::MAX as i64 {
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::Int(n as i32),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::Long(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
        }
    }

    fn number(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, LexToken> {
        self.digits();
        dbg!(&self.src[start_pos..self.pos as usize]);
        let s = &self.src[start_pos..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);

        let last_digit = self.src[start_pos..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(LexToken::new(
                self,
                LexTokenKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
            ));
        }

        match self.peek() {
            Some('L') => {
                let n: i64 = s.parse().unwrap();
                self.advance();
                Ok(LexToken::new(
                    self,
                    LexTokenKind::Long(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
            Some('U') | Some('u') => match self.peek_next() {
                Some('L') => {
                    let n: u64 = s.parse().unwrap();
                    self.advance();
                    self.advance();
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::ULong(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
                _ => {
                    let n: u32 = s.parse().unwrap();
                    self.advance();
                    Ok(LexToken::new(
                        self,
                        LexTokenKind::UInt(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            },
            _ => {
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
    }

    fn expect(
        &mut self,
        c: char,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<(), LexToken> {
        if self.peek() == Some(c) {
            self.advance();
            Ok(())
        } else {
            Err(LexToken::new(
                self,
                LexTokenKind::UnexpectedChar(c),
                start_pos,
                start_line,
                start_column,
            ))
        }
    }

    fn string(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, LexToken> {
        while let Some(c) = self.peek() {
            match c {
                '"' => {
                    break;
                }
                '\n' => {
                    self.newline();
                    return Err(LexToken::new(
                        self,
                        LexTokenKind::NewlineInString,
                        start_pos,
                        start_line,
                        start_column,
                    ));
                }
                _ => {
                    self.advance();
                }
            }
        }
        self.expect('"', start_pos, start_line, start_column)?;
        Ok(LexToken::new(
            self,
            LexTokenKind::TString,
            start_pos,
            start_line,
            start_column,
        ))
    }

    fn identifier(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<LexToken, LexToken> {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let s = &self.src[start_pos..self.pos as usize];

        match s {
            "true" => Ok(LexToken::new(
                self,
                LexTokenKind::Bool(true),
                start_pos,
                start_line,
                start_column,
            )),
            "false" => Ok(LexToken::new(
                self,
                LexTokenKind::Bool(false),
                start_pos,
                start_line,
                start_column,
            )),
            "null" => Ok(LexToken::new(
                self,
                LexTokenKind::Null,
                start_pos,
                start_line,
                start_column,
            )),
            "abstract" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordAbstract,
                start_pos,
                start_line,
                start_column,
            )),
            "annotation" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordAnnotation,
                start_pos,
                start_line,
                start_column,
            )),
            "by" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordBy,
                start_pos,
                start_line,
                start_column,
            )),
            "catch" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordCatch,
                start_pos,
                start_line,
                start_column,
            )),
            "companion" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordCompanion,
                start_pos,
                start_line,
                start_column,
            )),
            "constructor" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordConstructor,
                start_pos,
                start_line,
                start_column,
            )),
            "crossinline" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordCrossinline,
                start_pos,
                start_line,
                start_column,
            )),
            "data" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordData,
                start_pos,
                start_line,
                start_column,
            )),
            "dynamic" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordDynamic,
                start_pos,
                start_line,
                start_column,
            )),
            "enum" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordEnum,
                start_pos,
                start_line,
                start_column,
            )),
            "external" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordExternal,
                start_pos,
                start_line,
                start_column,
            )),
            "final" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordFinal,
                start_pos,
                start_line,
                start_column,
            )),
            "finally" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordFinally,
                start_pos,
                start_line,
                start_column,
            )),
            "import" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordImport,
                start_pos,
                start_line,
                start_column,
            )),
            "infix" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordInfix,
                start_pos,
                start_line,
                start_column,
            )),
            "init" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordInit,
                start_pos,
                start_line,
                start_column,
            )),
            "inline" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordInline,
                start_pos,
                start_line,
                start_column,
            )),
            "inner" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordInner,
                start_pos,
                start_line,
                start_column,
            )),
            "internal" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordInternal,
                start_pos,
                start_line,
                start_column,
            )),
            "lateinit" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordLateinit,
                start_pos,
                start_line,
                start_column,
            )),
            "noinline" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordNoinline,
                start_pos,
                start_line,
                start_column,
            )),
            "open" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordOpen,
                start_pos,
                start_line,
                start_column,
            )),
            "operator" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordOperator,
                start_pos,
                start_line,
                start_column,
            )),
            "out" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordOut,
                start_pos,
                start_line,
                start_column,
            )),
            "override" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordOverride,
                start_pos,
                start_line,
                start_column,
            )),
            "private" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordPrivate,
                start_pos,
                start_line,
                start_column,
            )),
            "protected" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordProtected,
                start_pos,
                start_line,
                start_column,
            )),
            "public" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordPublic,
                start_pos,
                start_line,
                start_column,
            )),
            "reified" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordReified,
                start_pos,
                start_line,
                start_column,
            )),
            "sealed" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordSealed,
                start_pos,
                start_line,
                start_column,
            )),
            "tailrec" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordTailrec,
                start_pos,
                start_line,
                start_column,
            )),
            "vararg" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordVararg,
                start_pos,
                start_line,
                start_column,
            )),
            "where" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordWhere,
                start_pos,
                start_line,
                start_column,
            )),
            "get" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordGet,
                start_pos,
                start_line,
                start_column,
            )),
            "set" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordSet,
                start_pos,
                start_line,
                start_column,
            )),
            "field" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordField,
                start_pos,
                start_line,
                start_column,
            )),
            "property" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordProperty,
                start_pos,
                start_line,
                start_column,
            )),
            "receiver" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordReceiver,
                start_pos,
                start_line,
                start_column,
            )),
            "param" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordParam,
                start_pos,
                start_line,
                start_column,
            )),
            "setparam" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordSetparam,
                start_pos,
                start_line,
                start_column,
            )),
            "delegate" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordDelegate,
                start_pos,
                start_line,
                start_column,
            )),
            "file" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordFile,
                start_pos,
                start_line,
                start_column,
            )),
            "expect" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordExpect,
                start_pos,
                start_line,
                start_column,
            )),
            "actual" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordActual,
                start_pos,
                start_line,
                start_column,
            )),
            "const" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordConst,
                start_pos,
                start_line,
                start_column,
            )),
            "suspend" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordSuspend,
                start_pos,
                start_line,
                start_column,
            )),
            "package" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordPackage,
                start_pos,
                start_line,
                start_column,
            )),
            "class" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordClass,
                start_pos,
                start_line,
                start_column,
            )),
            "interface" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordInterface,
                start_pos,
                start_line,
                start_column,
            )),
            "fun" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordFun,
                start_pos,
                start_line,
                start_column,
            )),
            "object" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordObject,
                start_pos,
                start_line,
                start_column,
            )),
            "val" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordVal,
                start_pos,
                start_line,
                start_column,
            )),
            "var" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordVar,
                start_pos,
                start_line,
                start_column,
            )),
            "typeof" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordTypeof,
                start_pos,
                start_line,
                start_column,
            )),
            "if" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordIf,
                start_pos,
                start_line,
                start_column,
            )),
            "else" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordElse,
                start_pos,
                start_line,
                start_column,
            )),
            "when" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordWhen,
                start_pos,
                start_line,
                start_column,
            )),
            "try" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordTry,
                start_pos,
                start_line,
                start_column,
            )),
            "for" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordFor,
                start_pos,
                start_line,
                start_column,
            )),
            "do" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordDo,
                start_pos,
                start_line,
                start_column,
            )),
            "throw" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordThrow,
                start_pos,
                start_line,
                start_column,
            )),
            "return" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordReturn,
                start_pos,
                start_line,
                start_column,
            )),
            "continue" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordContinue,
                start_pos,
                start_line,
                start_column,
            )),
            "as" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordAs,
                start_pos,
                start_line,
                start_column,
            )),
            "is" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordIs,
                start_pos,
                start_line,
                start_column,
            )),
            "in" => Ok(LexToken::new(
                self,
                LexTokenKind::KeywordIn,
                start_pos,
                start_line,
                start_column,
            )),
            _ => unimplemented!(),
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
                Some('/') => match self.peek_next() {
                    Some('/') => {
                        let start_pos = self.pos as usize;
                        let start_line = self.line;
                        let start_column = self.column as usize;
                        self.skip_until('\n');
                        return Ok(Some(LexToken::new(
                            self,
                            LexTokenKind::Comment,
                            start_pos,
                            start_line,
                            start_column,
                        )));
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
            Some('0') => match self.peek() {
                Some('x') | Some('X') => self.hex_number(start_pos, start_line, start_column),
                Some('b') | Some('B') => self.bin_number(start_pos, start_line, start_column),
                _ => self.number(start_pos, start_line, start_column),
            },
            Some('1') | Some('2') | Some('3') | Some('4') | Some('5') | Some('6') | Some('7')
            | Some('8') | Some('9') => self.number(start_pos, start_line, start_column),
            Some('"') => self.string(start_pos, start_line, start_column),
            Some(c) if c.is_ascii_alphanumeric() => {
                self.identifier(start_pos, start_line, start_column)
            }
            Some(_) => Err(LexToken::new(
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
    fn test_lex_number_int_with_underscores() {
        let s = " 123_000_000  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Int(123_000_000));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 13);
    }

    #[test]
    fn test_lex_number_int_with_trailing_underscore() {
        let s = " 123_000_000_  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, LexTokenKind::TrailingUnderscoreInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);
    }

    #[test]
    fn test_lex_number_uint() {
        let s = " 123U  456u";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::UInt(123u32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::UInt(456u32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 12);
    }

    #[test]
    fn test_lex_number_ulong() {
        let s = " 123UL  456uL";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::ULong(123u64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::ULong(456u64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 9);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);
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
    fn test_lex_number_bin_number() {
        let s = " 0b101 0B1_00000000_00000000_00000000_00000000";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Int(5));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Long(std::u32::MAX as i64 + 1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 47);
    }

    #[test]
    fn test_lex_number_bin_number_with_suffixes() {
        let s = " 0b101uL 0B1L 0b11U";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::ULong(5));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Long(0b1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::UInt(0b11));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);
    }

    #[test]
    fn test_lex_number_hex_number() {
        let s = " 0x1a1 0XdeadBEEF";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Int(0x1a1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Long(0xdeadbeef));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 18);
    }

    #[test]
    fn test_lex_number_hex_number_with_suffixes() {
        let s = " 0x101uL 0X1L 0x11U";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::ULong(0x101));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Long(0x1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::UInt(0x11));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);
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
        assert_eq!(tok.kind, LexTokenKind::Comment);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 10);

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

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
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

    #[test]
    fn test_lex_empty_string() {
        let s = r##"
            ""
            "##;
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::TString);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 13);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 15);
    }

    #[test]
    fn test_lex_string() {
        let s = r##"
            "abc123老虎老虎"
            "##;
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::TString);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 13);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 25);
        assert_eq!(&s[tok.start_pos..tok.end_pos], "\"abc123老虎老虎\"");
    }

    #[test]
    fn test_lex_unterminated_string() {
        let s = "\"";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, LexTokenKind::UnexpectedChar('"'));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn test_lex_newline_in_string() {
        let s = "\"\n";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, LexTokenKind::NewlineInString);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);
    }

    #[test]
    fn test_lex_bool() {
        let s = " true false";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Bool(true));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Bool(false));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 7);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 12);
    }

    #[test]
    fn test_lex_null() {
        let s = " null ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, LexTokenKind::Null);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);
    }
}
