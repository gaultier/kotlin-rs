use std::convert::TryFrom;
use std::fmt;
use std::option::Option;
use std::result::Result;
use std::str::Chars;
use crate::error::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NumberType {
    Integer,
    Real,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Slash,
    Star,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    Comma,
    At,
    Dollar,
    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
    Dot,
    Ampersand,
    AmpersandAmpersand,
    Pipe,
    PipePipe,
    Bang,
    BangEqual,
    BangEqualEqual,
    Colon,
    ColonColon,
    Semicolon,
    SemicolonSemicolon,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    Percent,
    PercentEqual,
    Lesser,
    LesserEqual,
    Greater,
    GreaterEqual,
    Arrow,
    FatArrow,
    Newline,
    EscapeSequenceNewline,
    EscapeSequenceCarriageReturn,
    EscapeSequenceTab,
    EscapeSequenceBackspace,
    EscapedQuote,
    EscapedDoubleQuote,
    EscapedBackSlash,
    EscapedDollar,
    UnicodeLiteral(char),
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Float(f32),
    Double(f64),
    Shebang,
    Comment,
    TString,
    Bool(bool),
    Null,
    KeywordAbstract,
    KeywordActual,
    KeywordAnnotation,
    KeywordAs,
    KeywordAsSafe,
    KeywordBy,
    KeywordCatch,
    KeywordClass,
    KeywordCompanion,
    KeywordConst,
    KeywordConstructor,
    KeywordContinue,
    KeywordCrossinline,
    KeywordData,
    KeywordDelegate,
    KeywordDo,
    KeywordDynamic,
    KeywordElse,
    KeywordEnum,
    KeywordExpect,
    KeywordExternal,
    KeywordField,
    KeywordFile,
    KeywordFinal,
    KeywordFinally,
    KeywordFor,
    KeywordFun,
    KeywordGet,
    KeywordIf,
    KeywordImport,
    KeywordIn,
    KeywordInfix,
    KeywordInit,
    KeywordInline,
    KeywordInner,
    KeywordInterface,
    KeywordInternal,
    KeywordIs,
    KeywordLateinit,
    KeywordNoinline,
    KeywordObject,
    KeywordOpen,
    KeywordOperator,
    KeywordOut,
    KeywordOverride,
    KeywordPackage,
    KeywordParam,
    KeywordPrivate,
    KeywordProperty,
    KeywordProtected,
    KeywordPublic,
    KeywordReceiver,
    KeywordReified,
    KeywordReturn,
    KeywordSealed,
    KeywordSet,
    KeywordSetparam,
    KeywordSuspend,
    KeywordTailrec,
    KeywordThrow,
    KeywordTry,
    KeywordTypeof,
    KeywordVal,
    KeywordVar,
    KeywordVararg,
    KeywordWhen,
    KeywordWhere,
    Identifier,
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start_pos: usize,
    pub start_line: usize,
    pub start_column: usize,
    pub end_pos: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OwnedToken<'a> {
    pub token: &'a Token,
    pub src: &'a str,
}

impl<'a> fmt::Display for OwnedToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.src[self.token.start_pos..self.token.end_pos])
    }
}

impl Token {
    pub fn is_eof(&self) -> bool {
        match self.kind {
            TokenKind::Eof => true,
            _ => false,
        }
    }

    pub fn num_type(&self) -> NumberType {
        match self {
            Token {
                kind: TokenKind::Float(_),
                ..
            }
            | Token {
                kind: TokenKind::Double(_),
                ..
            } => NumberType::Real,
            _ => NumberType::Integer,
        }
    }

    pub fn to_owned<'a>(&'a self, src: &'a str) -> OwnedToken<'a> {
        OwnedToken { token: &self, src }
    }

    pub fn new(
        lexer: &Lexer,
        kind: TokenKind,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Token {
        Token {
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


#[derive(Debug)]
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

    fn newline(&mut self, start_pos: usize, start_line: usize, start_column: usize) -> Token {
        self.line += 1;
        self.column = 1;

        Token::new(
            self,
            TokenKind::Newline,
            start_pos,
            start_line,
            start_column,
        )
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

    fn digits(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<NumberType, Error> {
        let mut num_type = NumberType::Integer;

        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_' => {
                    self.advance();
                }
                _ => {
                    break;
                }
            }
        }
        if self.peek() == Some('.') {
            self.advance();
            num_type = NumberType::Real;
        }

        while let Some(c) = self.peek() {
            match c {
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_' => {
                    self.advance();
                }
                _ => {
                    break;
                }
            }
        }

        match self.peek() {
            Some('e') | Some('E') => {
                num_type = NumberType::Real;
                self.advance();
                match self.peek() {
                    Some('+') | Some('-') => {
                        self.advance();
                    }
                    Some('0') | Some('1') | Some('2') | Some('3') | Some('4') | Some('5')
                    | Some('6') | Some('7') | Some('8') | Some('9') => {
                        while let Some(c) = self.peek() {
                            match c {
                                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_' => {
                                    self.advance();
                                }
                                _ => {
                                    break;
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorKind::MissingExponentInNumber,
                            start_pos,
                            start_line,
                            start_column,
                            self.pos as usize,
                            self.line as usize,
                            self.column as usize,
                        ))
                    }
                }
            }
            _ => {}
        }
        Ok(num_type)
    }

    fn bin_number(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<Token, Error> {
        // Consume `b|B`
        self.advance();

        self.bin_digits();
        let s = &self.src[start_pos + 2..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);
        if s.len() == 0 {
            return Err(Error::new(
                ErrorKind::MissingDigitsInBinaryNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        }

        let last_digit = self.src[start_pos + 2..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(Error::new(
                ErrorKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        }

        match self.peek() {
            Some('L') => {
                let n = i64::from_str_radix(s, 2).unwrap();
                self.advance();
                Ok(Token::new(
                    self,
                    TokenKind::Long(n),
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
                    Ok(Token::new(
                        self,
                        TokenKind::ULong(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
                _ => {
                    let n = u32::from_str_radix(s, 2).unwrap();
                    self.advance();
                    Ok(Token::new(
                        self,
                        TokenKind::UInt(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            },
            _ => {
                let n = i64::from_str_radix(s, 2).unwrap();
                if n < std::i32::MAX as i64 {
                    Ok(Token::new(
                        self,
                        TokenKind::Int(n as i32),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Long(n),
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
    ) -> Result<Token, Error> {
        // Consume `x|X`
        self.advance();

        self.hex_digits();
        let s = &self.src[start_pos + 2..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);
        if s.len() == 0 {
            return Err(Error::new(
                ErrorKind::MissingDigitsInHexNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        }

        let last_digit = self.src[start_pos + 2..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(Error::new(
                ErrorKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        }

        match self.peek() {
            Some('L') => {
                let n = i64::from_str_radix(s, 16).unwrap();
                self.advance();
                Ok(Token::new(
                    self,
                    TokenKind::Long(n),
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
                    Ok(Token::new(
                        self,
                        TokenKind::ULong(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
                _ => {
                    let n = u32::from_str_radix(s, 16).unwrap();
                    self.advance();
                    Ok(Token::new(
                        self,
                        TokenKind::UInt(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            },
            _ => {
                let n = i64::from_str_radix(s, 16).unwrap();
                if n < std::i32::MAX as i64 {
                    Ok(Token::new(
                        self,
                        TokenKind::Int(n as i32),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Long(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
        }
    }

    fn real(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<Token, Error> {
        self.digits(start_pos, start_line, start_column)?;
        dbg!(&self.src[start_pos..self.pos as usize]);
        let s = &self.src[start_pos..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);

        let last_digit = self.src[start_pos..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(Error::new(
                ErrorKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        } else if last_digit == Some('.') {
            return Err(Error::new(
                ErrorKind::TrailingDotInNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        }

        match self.peek() {
            Some('F') | Some('f') => {
                let n: f32 = s.parse().unwrap();
                self.advance();
                Ok(Token::new(
                    self,
                    TokenKind::Float(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
            _ => {
                let n: f64 = s.parse().unwrap();
                Ok(Token::new(
                    self,
                    TokenKind::Double(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
        }
    }

    fn integer(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<Token, Error> {
        if self.digits(start_pos, start_line, start_column)? == NumberType::Real {
            return self.real(start_pos, start_line, start_column);
        }
        dbg!(&self.src[start_pos..self.pos as usize]);
        let s = &self.src[start_pos..self.pos as usize]
            .to_string()
            .replace("_", "");
        dbg!(&s);

        let last_digit = self.src[start_pos..self.pos as usize].chars().last();
        // Forbid trailing underscore.
        if last_digit == Some('_') {
            return Err(Error::new(
                ErrorKind::TrailingUnderscoreInNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ));
        }

        let res = match self.peek() {
            Some('L') => {
                let n: i64 = s.parse().unwrap();
                self.advance();
                Ok(Token::new(
                    self,
                    TokenKind::Long(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
            Some('F') | Some('f') => self.real(start_pos, start_line, start_column),
            Some('U') | Some('u') => match self.peek_next() {
                Some('L') => {
                    let n: u64 = s.parse().unwrap();
                    self.advance();
                    self.advance();
                    Ok(Token::new(
                        self,
                        TokenKind::ULong(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
                _ => {
                    let n: u32 = s.parse().unwrap();
                    self.advance();
                    Ok(Token::new(
                        self,
                        TokenKind::UInt(n),
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            },
            _ => {
                let n: i32 = s.parse().unwrap();
                Ok(Token::new(
                    self,
                    TokenKind::Int(n),
                    start_pos,
                    start_line,
                    start_column,
                ))
            }
        };

        let first_digit = self.src[start_pos..self.pos as usize].chars().next();
        if res.as_ref().unwrap().num_type() == NumberType::Integer
            && first_digit == Some('0')
            && s.len() > 1
        {
            Err(Error::new(
                ErrorKind::LeadingZeroInNumber,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ))
        } else {
            res
        }
    }

    fn expect(
        &mut self,
        c: char,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<(), Error> {
        if self.peek() == Some(c) {
            self.advance();
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::UnexpectedChar(c),
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            ))
        }
    }

    fn string(
        &mut self,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
    ) -> Result<Token, Error> {
        while let Some(c) = self.peek() {
            match c {
                '"' => {
                    break;
                }
                '\n' => {
                    self.advance();
                    self.newline(start_pos, start_line, start_column);
                    return Err(Error::new(
                        ErrorKind::NewlineInString,
                        start_pos,
                        start_line,
                        start_column,
                        self.pos as usize,
                        self.line as usize,
                        self.column as usize,
                    ));
                }
                _ => {
                    self.advance();
                }
            }
        }
        self.expect('"', start_pos, start_line, start_column)?;
        Ok(Token::new(
            self,
            TokenKind::TString,
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
    ) -> Result<Token, Error> {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let s = &self.src[start_pos..self.pos as usize];

        match s {
            "true" => Ok(Token::new(
                self,
                TokenKind::Bool(true),
                start_pos,
                start_line,
                start_column,
            )),
            "false" => Ok(Token::new(
                self,
                TokenKind::Bool(false),
                start_pos,
                start_line,
                start_column,
            )),
            "null" => Ok(Token::new(
                self,
                TokenKind::Null,
                start_pos,
                start_line,
                start_column,
            )),
            "abstract" => Ok(Token::new(
                self,
                TokenKind::KeywordAbstract,
                start_pos,
                start_line,
                start_column,
            )),
            "annotation" => Ok(Token::new(
                self,
                TokenKind::KeywordAnnotation,
                start_pos,
                start_line,
                start_column,
            )),
            "by" => Ok(Token::new(
                self,
                TokenKind::KeywordBy,
                start_pos,
                start_line,
                start_column,
            )),
            "catch" => Ok(Token::new(
                self,
                TokenKind::KeywordCatch,
                start_pos,
                start_line,
                start_column,
            )),
            "companion" => Ok(Token::new(
                self,
                TokenKind::KeywordCompanion,
                start_pos,
                start_line,
                start_column,
            )),
            "constructor" => Ok(Token::new(
                self,
                TokenKind::KeywordConstructor,
                start_pos,
                start_line,
                start_column,
            )),
            "crossinline" => Ok(Token::new(
                self,
                TokenKind::KeywordCrossinline,
                start_pos,
                start_line,
                start_column,
            )),
            "data" => Ok(Token::new(
                self,
                TokenKind::KeywordData,
                start_pos,
                start_line,
                start_column,
            )),
            "dynamic" => Ok(Token::new(
                self,
                TokenKind::KeywordDynamic,
                start_pos,
                start_line,
                start_column,
            )),
            "enum" => Ok(Token::new(
                self,
                TokenKind::KeywordEnum,
                start_pos,
                start_line,
                start_column,
            )),
            "external" => Ok(Token::new(
                self,
                TokenKind::KeywordExternal,
                start_pos,
                start_line,
                start_column,
            )),
            "final" => Ok(Token::new(
                self,
                TokenKind::KeywordFinal,
                start_pos,
                start_line,
                start_column,
            )),
            "finally" => Ok(Token::new(
                self,
                TokenKind::KeywordFinally,
                start_pos,
                start_line,
                start_column,
            )),
            "import" => Ok(Token::new(
                self,
                TokenKind::KeywordImport,
                start_pos,
                start_line,
                start_column,
            )),
            "infix" => Ok(Token::new(
                self,
                TokenKind::KeywordInfix,
                start_pos,
                start_line,
                start_column,
            )),
            "init" => Ok(Token::new(
                self,
                TokenKind::KeywordInit,
                start_pos,
                start_line,
                start_column,
            )),
            "inline" => Ok(Token::new(
                self,
                TokenKind::KeywordInline,
                start_pos,
                start_line,
                start_column,
            )),
            "inner" => Ok(Token::new(
                self,
                TokenKind::KeywordInner,
                start_pos,
                start_line,
                start_column,
            )),
            "internal" => Ok(Token::new(
                self,
                TokenKind::KeywordInternal,
                start_pos,
                start_line,
                start_column,
            )),
            "lateinit" => Ok(Token::new(
                self,
                TokenKind::KeywordLateinit,
                start_pos,
                start_line,
                start_column,
            )),
            "noinline" => Ok(Token::new(
                self,
                TokenKind::KeywordNoinline,
                start_pos,
                start_line,
                start_column,
            )),
            "open" => Ok(Token::new(
                self,
                TokenKind::KeywordOpen,
                start_pos,
                start_line,
                start_column,
            )),
            "operator" => Ok(Token::new(
                self,
                TokenKind::KeywordOperator,
                start_pos,
                start_line,
                start_column,
            )),
            "out" => Ok(Token::new(
                self,
                TokenKind::KeywordOut,
                start_pos,
                start_line,
                start_column,
            )),
            "override" => Ok(Token::new(
                self,
                TokenKind::KeywordOverride,
                start_pos,
                start_line,
                start_column,
            )),
            "private" => Ok(Token::new(
                self,
                TokenKind::KeywordPrivate,
                start_pos,
                start_line,
                start_column,
            )),
            "protected" => Ok(Token::new(
                self,
                TokenKind::KeywordProtected,
                start_pos,
                start_line,
                start_column,
            )),
            "public" => Ok(Token::new(
                self,
                TokenKind::KeywordPublic,
                start_pos,
                start_line,
                start_column,
            )),
            "reified" => Ok(Token::new(
                self,
                TokenKind::KeywordReified,
                start_pos,
                start_line,
                start_column,
            )),
            "sealed" => Ok(Token::new(
                self,
                TokenKind::KeywordSealed,
                start_pos,
                start_line,
                start_column,
            )),
            "tailrec" => Ok(Token::new(
                self,
                TokenKind::KeywordTailrec,
                start_pos,
                start_line,
                start_column,
            )),
            "vararg" => Ok(Token::new(
                self,
                TokenKind::KeywordVararg,
                start_pos,
                start_line,
                start_column,
            )),
            "where" => Ok(Token::new(
                self,
                TokenKind::KeywordWhere,
                start_pos,
                start_line,
                start_column,
            )),
            "get" => Ok(Token::new(
                self,
                TokenKind::KeywordGet,
                start_pos,
                start_line,
                start_column,
            )),
            "set" => Ok(Token::new(
                self,
                TokenKind::KeywordSet,
                start_pos,
                start_line,
                start_column,
            )),
            "field" => Ok(Token::new(
                self,
                TokenKind::KeywordField,
                start_pos,
                start_line,
                start_column,
            )),
            "property" => Ok(Token::new(
                self,
                TokenKind::KeywordProperty,
                start_pos,
                start_line,
                start_column,
            )),
            "receiver" => Ok(Token::new(
                self,
                TokenKind::KeywordReceiver,
                start_pos,
                start_line,
                start_column,
            )),
            "param" => Ok(Token::new(
                self,
                TokenKind::KeywordParam,
                start_pos,
                start_line,
                start_column,
            )),
            "setparam" => Ok(Token::new(
                self,
                TokenKind::KeywordSetparam,
                start_pos,
                start_line,
                start_column,
            )),
            "delegate" => Ok(Token::new(
                self,
                TokenKind::KeywordDelegate,
                start_pos,
                start_line,
                start_column,
            )),
            "file" => Ok(Token::new(
                self,
                TokenKind::KeywordFile,
                start_pos,
                start_line,
                start_column,
            )),
            "expect" => Ok(Token::new(
                self,
                TokenKind::KeywordExpect,
                start_pos,
                start_line,
                start_column,
            )),
            "actual" => Ok(Token::new(
                self,
                TokenKind::KeywordActual,
                start_pos,
                start_line,
                start_column,
            )),
            "const" => Ok(Token::new(
                self,
                TokenKind::KeywordConst,
                start_pos,
                start_line,
                start_column,
            )),
            "suspend" => Ok(Token::new(
                self,
                TokenKind::KeywordSuspend,
                start_pos,
                start_line,
                start_column,
            )),
            "package" => Ok(Token::new(
                self,
                TokenKind::KeywordPackage,
                start_pos,
                start_line,
                start_column,
            )),
            "class" => Ok(Token::new(
                self,
                TokenKind::KeywordClass,
                start_pos,
                start_line,
                start_column,
            )),
            "interface" => Ok(Token::new(
                self,
                TokenKind::KeywordInterface,
                start_pos,
                start_line,
                start_column,
            )),
            "fun" => Ok(Token::new(
                self,
                TokenKind::KeywordFun,
                start_pos,
                start_line,
                start_column,
            )),
            "object" => Ok(Token::new(
                self,
                TokenKind::KeywordObject,
                start_pos,
                start_line,
                start_column,
            )),
            "val" => Ok(Token::new(
                self,
                TokenKind::KeywordVal,
                start_pos,
                start_line,
                start_column,
            )),
            "var" => Ok(Token::new(
                self,
                TokenKind::KeywordVar,
                start_pos,
                start_line,
                start_column,
            )),
            "typeof" => Ok(Token::new(
                self,
                TokenKind::KeywordTypeof,
                start_pos,
                start_line,
                start_column,
            )),
            "if" => Ok(Token::new(
                self,
                TokenKind::KeywordIf,
                start_pos,
                start_line,
                start_column,
            )),
            "else" => Ok(Token::new(
                self,
                TokenKind::KeywordElse,
                start_pos,
                start_line,
                start_column,
            )),
            "when" => Ok(Token::new(
                self,
                TokenKind::KeywordWhen,
                start_pos,
                start_line,
                start_column,
            )),
            "try" => Ok(Token::new(
                self,
                TokenKind::KeywordTry,
                start_pos,
                start_line,
                start_column,
            )),
            "for" => Ok(Token::new(
                self,
                TokenKind::KeywordFor,
                start_pos,
                start_line,
                start_column,
            )),
            "do" => Ok(Token::new(
                self,
                TokenKind::KeywordDo,
                start_pos,
                start_line,
                start_column,
            )),
            "throw" => Ok(Token::new(
                self,
                TokenKind::KeywordThrow,
                start_pos,
                start_line,
                start_column,
            )),
            "return" => Ok(Token::new(
                self,
                TokenKind::KeywordReturn,
                start_pos,
                start_line,
                start_column,
            )),
            "continue" => Ok(Token::new(
                self,
                TokenKind::KeywordContinue,
                start_pos,
                start_line,
                start_column,
            )),
            "as" => {
                if self.match_char('?') {
                    Ok(Token::new(
                        self,
                        TokenKind::KeywordAsSafe,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::KeywordAs,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            "is" => Ok(Token::new(
                self,
                TokenKind::KeywordIs,
                start_pos,
                start_line,
                start_column,
            )),
            "in" => Ok(Token::new(
                self,
                TokenKind::KeywordIn,
                start_pos,
                start_line,
                start_column,
            )),
            _ => Ok(Token::new(
                self,
                TokenKind::Identifier,
                start_pos,
                start_line,
                start_column,
            )),
        }
    }

    fn skip_whitespace(&mut self) -> Result<Option<Token>, Error> {
        while !self.is_at_end() {
            match self.peek() {
                None | Some(' ') | Some('\t') | Some('\r') => {
                    self.advance();
                }
                Some('#') => match self.peek_next() {
                    Some('!') => {
                        let start_pos = self.pos as usize;
                        let start_line = self.line;
                        let start_column = self.column as usize;
                        if self.line != 1 {
                            self.skip_until('\n');
                            return Err(Error::new(
                                ErrorKind::ShebangNotOnFirstLine,
                                start_pos,
                                start_line,
                                start_column,
                                self.pos as usize,
                                self.line as usize,
                                self.column as usize,
                            ));
                        }
                        self.skip_until('\n');
                        return Ok(Some(Token::new(
                            self,
                            TokenKind::Shebang,
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
                        return Ok(Some(Token::new(
                            self,
                            TokenKind::Comment,
                            start_pos,
                            start_line,
                            start_column,
                        )));
                    }
                    Some('*') => {
                        let start_pos = self.pos as usize;
                        let start_line = self.line;
                        let start_column = self.column as usize;
                        self.advance();
                        self.advance();
                        while let Some(c) = self.peek() {
                            match c {
                                '*' if self.peek_next() == Some('/') => {
                                    self.advance();
                                    self.advance();
                                    break;
                                }
                                '\n' => {
                                    self.advance();
                                    self.newline(start_pos, start_line, start_column);
                                }
                                _ => {
                                    self.advance();
                                }
                            }
                        }
                        return Ok(Some(Token::new(
                            self,
                            TokenKind::Comment,
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

    pub fn lex(&mut self) -> Result<Token, Error> {
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
            Some('\n') => Ok(self.newline(start_pos, start_line, start_column)),
            Some('\\') => {
                if self.match_char('n') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapeSequenceNewline,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('r') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapeSequenceCarriageReturn,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('t') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapeSequenceTab,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('b') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapeSequenceBackspace,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('\'') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapedQuote,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('"') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapedDoubleQuote,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('$') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapedDollar,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('\\') {
                    Ok(Token::new(
                        self,
                        TokenKind::EscapedBackSlash,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('u') {
                    for _ in 1..=4 {
                        match self.peek() {
                            Some(c) if c.is_ascii_hexdigit() => {
                                self.advance();
                            }
                            _ => {
                                return Err(Error::new(
                                    ErrorKind::IncompleteUnicodeLiteral,
                                    start_pos,
                                    start_line,
                                    start_column,
                                    self.pos as usize,
                                    self.line as usize,
                                    self.column as usize,
                                ))
                            }
                        }
                    }

                    let s = &self.src[start_pos + 2..self.pos as usize];
                    dbg!(&s);
                    let bytes = s
                        .chars()
                        .map(|c| c.to_digit(16).unwrap() as u8)
                        .collect::<Vec<u8>>();
                    dbg!(&bytes);
                    let n: u16 = bytes[0] as u16 * 16 * 16 * 16
                        + bytes[1] as u16 * 16 * 16
                        + bytes[2] as u16 * 16
                        + bytes[3] as u16;
                    dbg!(&n);
                    let c = char::try_from(n as u32);
                    dbg!(&c);
                    if let Ok(c) = c {
                        Ok(Token::new(
                            self,
                            TokenKind::UnicodeLiteral(c),
                            start_pos,
                            start_line,
                            start_column,
                        ))
                    } else {
                        Err(Error::new(
                            ErrorKind::InvalidUnicodeLiteral(c.unwrap_err().to_string()),
                            start_pos,
                            start_line,
                            start_column,
                            self.pos as usize,
                            self.line as usize,
                            self.column as usize,
                        ))
                    }
                } else {
                    let c = if self.peek().is_some() {
                        self.advance()
                    } else {
                        // Do not advance past the end to avoid giving a `end_column` past the end
                        None
                    };

                    Err(Error::new(
                        ErrorKind::UnknownEscapeSequence(c),
                        start_pos,
                        start_line,
                        start_column,
                        self.pos as usize,
                        self.line as usize,
                        self.column as usize,
                    ))
                }
            }
            Some('@') => Ok(Token::new(
                self,
                TokenKind::At,
                start_pos,
                start_line,
                start_column,
            )),
            Some('$') => Ok(Token::new(
                self,
                TokenKind::Dollar,
                start_pos,
                start_line,
                start_column,
            )),
            Some('(') => Ok(Token::new(
                self,
                TokenKind::LeftParen,
                start_pos,
                start_line,
                start_column,
            )),
            Some(')') => Ok(Token::new(
                self,
                TokenKind::RightParen,
                start_pos,
                start_line,
                start_column,
            )),
            Some('{') => Ok(Token::new(
                self,
                TokenKind::LeftCurlyBracket,
                start_pos,
                start_line,
                start_column,
            )),
            Some('}') => Ok(Token::new(
                self,
                TokenKind::RightCurlyBracket,
                start_pos,
                start_line,
                start_column,
            )),
            Some('[') => Ok(Token::new(
                self,
                TokenKind::LeftSquareBracket,
                start_pos,
                start_line,
                start_column,
            )),
            Some(']') => Ok(Token::new(
                self,
                TokenKind::RightSquareBracket,
                start_pos,
                start_line,
                start_column,
            )),
            Some('+') => {
                if self.match_char('+') {
                    Ok(Token::new(
                        self,
                        TokenKind::PlusPlus,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::PlusEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Plus,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('<') => {
                if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::LesserEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Lesser,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('>') => {
                if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::GreaterEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Greater,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('!') => {
                if self.match_char('=') {
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::new(
                            self,
                            TokenKind::BangEqualEqual,
                            start_pos,
                            start_line,
                            start_column,
                        ))
                    } else {
                        Ok(Token::new(
                            self,
                            TokenKind::BangEqual,
                            start_pos,
                            start_line,
                            start_column,
                        ))
                    }
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Bang,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('-') => {
                if self.match_char('-') {
                    Ok(Token::new(
                        self,
                        TokenKind::MinusMinus,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::MinusEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else if self.match_char('>') {
                    Ok(Token::new(
                        self,
                        TokenKind::Arrow,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Minus,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('&') => {
                if self.match_char('&') {
                    Ok(Token::new(
                        self,
                        TokenKind::AmpersandAmpersand,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Ampersand,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some(':') => {
                if self.match_char(':') {
                    Ok(Token::new(
                        self,
                        TokenKind::ColonColon,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Colon,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some(';') => {
                if self.match_char(';') {
                    Ok(Token::new(
                        self,
                        TokenKind::SemicolonSemicolon,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Semicolon,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('|') => {
                if self.match_char('|') {
                    Ok(Token::new(
                        self,
                        TokenKind::PipePipe,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Pipe,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('/') => {
                if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::SlashEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Slash,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('*') => {
                if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::StarEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Star,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('=') => {
                if self.match_char('=') {
                    if self.match_char('=') {
                        Ok(Token::new(
                            self,
                            TokenKind::EqualEqualEqual,
                            start_pos,
                            start_line,
                            start_column,
                        ))
                    } else {
                        Ok(Token::new(
                            self,
                            TokenKind::EqualEqual,
                            start_pos,
                            start_line,
                            start_column,
                        ))
                    }
                } else if self.match_char('>') {
                    Ok(Token::new(
                        self,
                        TokenKind::FatArrow,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Equal,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('%') => {
                if self.match_char('=') {
                    Ok(Token::new(
                        self,
                        TokenKind::PercentEqual,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                } else {
                    Ok(Token::new(
                        self,
                        TokenKind::Percent,
                        start_pos,
                        start_line,
                        start_column,
                    ))
                }
            }
            Some('.') if !self.peek().map(|c| c.is_digit(10)).unwrap_or(false) => Ok(Token::new(
                self,
                TokenKind::Dot,
                start_pos,
                start_line,
                start_column,
            )),
            Some('.') => self.real(start_pos, start_line, start_column),
            Some('0') => match self.peek() {
                Some('x') | Some('X') => self.hex_number(start_pos, start_line, start_column),
                Some('b') | Some('B') => self.bin_number(start_pos, start_line, start_column),
                _ => self.integer(start_pos, start_line, start_column),
            },
            Some('1') | Some('2') | Some('3') | Some('4') | Some('5') | Some('6') | Some('7')
            | Some('8') | Some('9') => self.integer(start_pos, start_line, start_column),
            Some('"') => self.string(start_pos, start_line, start_column),
            // TODO: expand to more unicode classes
            Some(c) if c.is_alphanumeric() || c == '_' => {
                self.identifier(start_pos, start_line, start_column)
            }
            Some(_) => Err(Error::new(
                ErrorKind::UnknownChar,
                start_pos,
                start_line,
                start_column,
                self.pos as usize,
                self.line as usize,
                self.column as usize,
            )),
            None => Ok(Token::new(
                self,
                TokenKind::Eof,
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
    fn int() {
        let s = " 123  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(123));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);
    }

    #[test]
    fn int_with_underscores() {
        let s = " 123_000_000  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(123_000_000));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 13);
    }

    #[test]
    fn int_with_trailing_underscore() {
        let s = " 123_000_000_  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::TrailingUnderscoreInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);
    }

    #[test]
    fn int_with_leading_zero() {
        let s = " 0 0123 0456L 0u  ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(0i32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::LeadingZeroInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 4);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 8);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::LeadingZeroInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 9);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(0u32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 17);
    }

    #[test]
    fn uint() {
        let s = " 123U  456u";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(123u32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(456u32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 12);
    }

    #[test]
    fn ulong() {
        let s = " 123UL  456uL";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(123u64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(456u64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 9);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);
    }

    #[test]
    fn long() {
        let s = " 123L  ";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(123i64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);
    }

    #[test]
    fn bin_number() {
        let s = " 0b101 0B1_00000000_00000000_00000000_00000000";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(5));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(std::u32::MAX as i64 + 1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 47);
    }

    #[test]
    fn bin_number_with_suffixes() {
        let s = " 0b101uL 0B1L 0b11U";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(5));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(0b1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(0b11));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);
    }

    #[test]
    fn bin_number_missing_digits() {
        let s = " 0b ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::MissingDigitsInBinaryNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn hex_number_missing_digits() {
        let s = " 0x ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::MissingDigitsInHexNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn hex_number() {
        let s = " 0x1a1 0XdeadBEEF";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(0x1a1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(0xdeadbeef));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 18);
    }

    #[test]
    fn hex_number_with_suffixes() {
        let s = " 0x101uL 0X1L 0x11U";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(0x101));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(0x1));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(0x11));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);
    }

    #[test]
    fn float() {
        let s = " 123f 456F 0f 0.0f .1f 2. ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Float(123f32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Float(456f32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 7);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 11);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Float(0f32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 12);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Float(0f32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 19);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Float(0.1f32));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 20);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 23);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::TrailingDotInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 24);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 26);
    }

    #[test]
    fn double() {
        let s = " 123.0 456.0 0.0 .1 2.";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(123f64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(456f64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 13);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(0f64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 14);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 17);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(0.1f64));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 18);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::TrailingDotInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 21);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 23);
    }

    #[test]
    fn double_with_exp() {
        let s = " 123e2 123E+2 123E-2 123e ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(123e2));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(123e2));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 14);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(123e-2));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 21);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::MissingExponentInNumber);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 22);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 26);
    }

    #[test]
    fn shebang() {
        let s = "#!/bin/cat\n+";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Shebang);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 11);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 11);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Plus);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn comment() {
        let s = "//bin/cat\n+";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Comment);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 10);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Plus);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn unknown() {
        let s = "§+~";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::UnknownChar);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn shebang_not_on_first_line() {
        let s = "\n#!/bin/cat\n+";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::ShebangNotOnFirstLine);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 11);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 11);
        assert_eq!(tok.end_line, 3);
        assert_eq!(tok.end_column, 1);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Plus);
        assert_eq!(tok.start_line, 3);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 3);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn empty_string() {
        let s = r##"
            ""
            "##;
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::TString);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 13);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 15);
    }

    #[test]
    fn string() {
        let s = r##"
            "abc123老虎老虎"
            "##;
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::TString);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 13);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 25);
        assert_eq!(&s[tok.start_pos..tok.end_pos], "\"abc123老虎老虎\"");
    }

    #[test]
    fn unterminated_string() {
        let s = "\"";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::UnexpectedChar('"'));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);
    }

    #[test]
    fn newline_in_string() {
        let s = "\"\n";
        let mut lexer = Lexer::new(&s);
        let tok = lexer.lex();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::NewlineInString);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 1);
    }

    #[test]
    fn bool() {
        let s = " true false";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Bool(true));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Bool(false));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 7);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 12);
    }

    #[test]
    fn null() {
        let s = " null ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Null);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);
    }

    #[test]
    fn keyword() {
        // TODO: many more keywords
        let s = " abstract as as?";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::KeywordAbstract);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 10);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::KeywordAs);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 11);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 13);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::KeywordAsSafe);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 14);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 17);
    }

    #[test]
    fn identifier() {
        // `ƍ` is of category Ll (letter lowercase)
        // `ᴽ` is of category Lm (letter modifier)
        // `א` is of categoy Lo (other letter)
        // `ᾯ` is of category Lt (letter titlecase)
        // `Ʊ` is of category Lu (letter uppercase)
        // `ᛮ` is of category Nl (letter number)
        let s = " _ _a B_ ᛮƍᴽאᾯƱ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);
        assert_eq!(&s[tok.start_pos..tok.end_pos], "_");

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 4);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);
        assert_eq!(&s[tok.start_pos..tok.end_pos], "_a");

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 7);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);
        assert_eq!(&s[tok.start_pos..tok.end_pos], "B_");

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 16);
        assert_eq!(&s[tok.start_pos..tok.end_pos], "ᛮƍᴽאᾯƱ");
    }

    #[test]
    fn comment_multiline() {
        let s = "/* foo \n bar */ + ";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Comment);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 8);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Plus);
        assert_eq!(tok.start_line, 2);
        assert_eq!(tok.start_column, 9);
        assert_eq!(tok.end_line, 2);
        assert_eq!(tok.end_column, 10);
    }

    #[test]
    fn empty_comment_multiline() {
        let s = "/**/+";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Comment);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Plus);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 5);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);
    }

    #[test]
    fn dot() {
        let s = " . .";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Dot);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Dot);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 4);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);
    }

    #[test]
    fn single_char_tokens() {
        let s = "-*/:;@$";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Minus);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Star);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Slash);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Colon);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 4);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Semicolon);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 5);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 6);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::At);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 6);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Dollar);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 7);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 8);
    }

    #[test]
    fn plus() {
        let s = "+++ +=";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::PlusPlus);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Plus);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::PlusEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 5);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);
    }

    #[test]
    fn minus() {
        let s = "--- ->";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::MinusMinus);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Minus);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Arrow);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 5);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);
    }

    #[test]
    fn ampersand() {
        let s = "&&&";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::AmpersandAmpersand);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Ampersand);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn pipe() {
        let s = "|||";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::PipePipe);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Pipe);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn colon() {
        let s = ":::";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ColonColon);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Colon);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn semicolon() {
        let s = ";;;";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::SemicolonSemicolon);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Semicolon);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn star() {
        let s = "**=";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Star);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::StarEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn slash() {
        let s = "/ /=";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Slash);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::SlashEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);
    }

    #[test]
    fn percent() {
        let s = "%%=";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Percent);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::PercentEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn equal() {
        let s = "== = === =>";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EqualEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Equal);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 4);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EqualEqualEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 6);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::FatArrow);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 10);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 12);
    }

    #[test]
    fn bang() {
        let s = "!!=!==";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Bang);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::BangEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::BangEqualEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 4);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);
    }

    #[test]
    fn smaller() {
        let s = "<<=";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Lesser);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::LesserEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn greater() {
        let s = ">>=";
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Greater);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 2);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::GreaterEqual);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 2);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 4);
    }

    #[test]
    fn escape_sequence_literals() {
        let s = r##"\n\r\b\t\'\"\$\\\+\"##;
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapeSequenceNewline);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 3);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapeSequenceCarriageReturn);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 3);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 5);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapeSequenceBackspace);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 5);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapeSequenceTab);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 7);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 9);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapedQuote);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 9);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 11);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapedDoubleQuote);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 11);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 13);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapedDollar);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 13);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 15);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::EscapedBackSlash);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 15);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 17);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::UnknownEscapeSequence(Some('+')));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 17);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 19);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::UnknownEscapeSequence(None));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 19);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);
    }

    #[test]
    fn unicode_literal() {
        let s = r##"\uabcd \u123 \ud800"##;
        let mut lexer = Lexer::new(&s);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UnicodeLiteral('ꯍ'));
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 1);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 7);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::IncompleteUnicodeLiteral);
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 8);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 13);

        let tok = lexer.lex();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(
            tok.kind,
            ErrorKind::InvalidUnicodeLiteral(
                "converted integer out of range for `char`".to_string()
            )
        );
        assert_eq!(tok.start_line, 1);
        assert_eq!(tok.start_column, 14);
        assert_eq!(tok.end_line, 1);
        assert_eq!(tok.end_column, 20);
    }
}
