use crate::cursor::*;
use crate::error::*;
use log::debug;
// use std::convert::TryFrom;
// use std::fmt;
// use std::option::Option;
// use std::result::Result;

#[derive(Debug, Eq, PartialEq, Clone)]
enum NumberBase {
    Binary,
    Octal, // Actuallty invalid but useful for nice error messages
    Hexadecimal,
    Decimal,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum NumberSuffix {
    L,
    U,
    UL,
    F,
    Invalid(char),
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Whitespace,
    LineComment,
    BlockComment { terminated: bool },
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
enum CursorNumberKind {
    Int {
        base: NumberBase,
    },
    Float {
        base: NumberBase,
        empty_exponent: bool,
    },
}

#[derive(Debug, PartialEq, Clone)]
enum CursorTokenKind {
    Number {
        kind: CursorNumberKind,
        suffix: Option<NumberSuffix>,
        suffix_start: usize,
    },
    Plus,
    PlusPlus,
    Minus,
    Star,
    Slash,
    Ampersand,
    Pipe,
    At,
    Dollar,
    Semicolon,
    Bang,
    Lesser,
    Greater,
    Percent,
    Equal,
    Newline,
    Whitespace,
    Colon,
    Comma,
    Dot,
    LineComment,
    BlockComment {
        terminated: bool,
    },
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
struct CursorToken {
    pub kind: CursorTokenKind,
    pub len: usize,
}

impl CursorToken {
    pub fn new(kind: CursorTokenKind, len: usize) -> CursorToken {
        CursorToken { kind, len }
    }
}

/// Parses the first token from the provided input string.
fn first_token(input: &str) -> CursorToken {
    debug_assert!(!input.is_empty());
    Cursor::new(input).advance_token()
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    match c {
        // Usual ASCII suspects
        | '\u{0009}' // \t
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

            => true,
        _ => false,
    }
}

/// True if `c` is valid as a first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    // We also add fast-path for ascii idents
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
    // || (c > '\x7f' && unicode_xid::UnicodeXID::is_xid_start(c))
}

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_continue(c: char) -> bool {
    // This is exactly XID_Continue.
    // We also add fast-path for ascii idents
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_'
    // || (c > '\x7f' && unicode_xid::UnicodeXID::is_xid_continue(c))
}

fn prepare_num_str_for_parsing(s: &str) -> String {
    s.to_string().replace("_", "")
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    fn advance_token(&mut self) -> CursorToken {
        let first_char = self.bump().unwrap();
        let token_kind = match first_char {
            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => CursorTokenKind::Slash,
            },
            '\n' => CursorTokenKind::Newline,
            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            // c if is_id_start(c) => self.ident(),
            '.' if !self.first().is_ascii_digit() => CursorTokenKind::Dot,
            // Numeric literal.
            '0'..='9' | '.' => {
                let kind = self.number(first_char);
                let suffix_start = self.len_consumed();
                let suffix = self.eat_number_suffix();
                CursorTokenKind::Number {
                    kind,
                    suffix,
                    suffix_start,
                }
            }

            // One-symbol tokens.
            ';' => CursorTokenKind::Semicolon,
            ',' => CursorTokenKind::Comma,
            // '(' => CursorTokenKind::OpenParen,
            // ')' => CursorTokenKind::CloseParen,
            // '{' => CursorTokenKind::OpenBrace,
            // '}' => CursorTokenKind::CloseBrace,
            // '[' => CursorTokenKind::OpenBracket,
            // ']' => CursorTokenKind::CloseBracket,
            '@' => CursorTokenKind::At,
            // '#' => CursorTokenKind::Pound,
            // '~' => CursorTokenKind::Tilde,
            // '?' => CursorTokenKind::Question,
            ':' => CursorTokenKind::Colon,
            '$' => CursorTokenKind::Dollar,
            '=' => CursorTokenKind::Equal,
            '!' => CursorTokenKind::Bang,
            '<' => CursorTokenKind::Lesser,
            '>' => CursorTokenKind::Greater,
            '-' => CursorTokenKind::Minus,
            '&' => CursorTokenKind::Ampersand,
            '|' => CursorTokenKind::Pipe,
            '+' if self.match_char('+') => CursorTokenKind::PlusPlus,
            '+' => CursorTokenKind::Plus,
            '*' => CursorTokenKind::Star,
            // '^' => CursorTokenKind::Caret,
            '%' => CursorTokenKind::Percent,

            // Lifetime or character literal.
            // '\'' => self.lifetime_or_char(),

            // String literal.
            // '"' => {
            //     let terminated = self.double_quoted_string();
            //     let suffix_start = self.len_consumed();
            //     if terminated {
            //         self.eat_literal_suffix();
            //     }
            //     let kind = Str { terminated };
            //     Literal { kind, suffix_start }
            // }
            _ => CursorTokenKind::Unknown,
        };
        CursorToken::new(token_kind, self.len_consumed())
    }

    fn line_comment(&mut self) -> CursorTokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '/');
        self.bump();
        self.eat_while(|c| c != '\n');
        CursorTokenKind::LineComment
    }

    fn block_comment(&mut self) -> CursorTokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        self.bump();
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                '\n' => {}
                _ => (),
            }
        }

        CursorTokenKind::BlockComment {
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self) -> CursorTokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        CursorTokenKind::Whitespace
    }

    // fn raw_ident(&mut self) -> TokenKind {
    //     debug_assert!(self.prev() == 'r' && self.first() == '#' && is_id_start(self.second()));
    //     // Eat "#" symbol.
    //     self.bump();
    //     // Eat the identifier part of RawIdent.
    //     self.eat_identifier();
    //     RawIdent
    // }

    // fn ident(&mut self) -> TokenKind {
    //     debug_assert!(is_id_start(self.prev()));
    //     // Start is already eaten, eat the rest of identifier.
    //     self.eat_while(is_id_continue);
    //     Ident
    // }

    fn number(&mut self, first_digit: char) -> CursorNumberKind {
        debug_assert!(self.prev() == '.' || ('0' <= self.prev() && self.prev() <= '9'));
        let mut base = NumberBase::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            let has_digits = match self.first() {
                'b' | 'B' => {
                    base = NumberBase::Binary;
                    self.bump();
                    self.eat_decimal_digits()
                }
                'o' => {
                    base = NumberBase::Octal;
                    self.bump();
                    self.eat_decimal_digits()
                }
                'x' | 'X' => {
                    base = NumberBase::Hexadecimal;
                    self.bump();
                    self.eat_hexadecimal_digits()
                }
                // Not a base prefix.
                '0'..='9' | '_' | '.' | 'e' | 'E' => {
                    self.eat_decimal_digits();
                    true
                }
                // Just a 0.
                _ => return CursorNumberKind::Int { base },
            };
            // NumberBase prefix was provided, but there were no digits
            // after it, e.g. "0x".
            if !has_digits {
                return CursorNumberKind::Int { base };
            }
        } else if first_digit == '.' {
            let mut empty_exponent = false;
            if self.first().is_digit(10) {
                self.eat_decimal_digits();
                match self.first() {
                    'e' | 'E' => {
                        self.bump();
                        empty_exponent = !self.eat_float_exponent();
                    }
                    _ => (),
                }
            }
            return CursorNumberKind::Float {
                base,
                empty_exponent,
            };
        } else {
            // No base prefix, parse number in the usual way.
            self.eat_decimal_digits();
        };

        match self.first() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access or a range pattern
            // (`0..2` and `12.foo()`)
            '.' if self.second() != '.' && !is_id_start(self.second()) => {
                // might have stuff after the ., and if it does, it needs to start
                // with a number
                self.bump();
                let mut empty_exponent = false;
                if self.first().is_digit(10) {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                CursorNumberKind::Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();
                CursorNumberKind::Float {
                    base,
                    empty_exponent,
                }
            }
            _ => CursorNumberKind::Int { base },
        }
    }

    // fn lifetime_or_char(&mut self) -> TokenKind {
    //     debug_assert!(self.prev() == '\'');

    //     let can_be_a_lifetime = if self.second() == '\'' {
    //         // It's surely not a lifetime.
    //         false
    //     } else {
    //         // If the first symbol is valid for identifier, it can be a lifetime.
    //         // Also check if it's a number for a better error reporting (so '0 will
    //         // be reported as invalid lifetime and not as unterminated char literal).
    //         is_id_start(self.first()) || self.first().is_digit(10)
    //     };

    //     if !can_be_a_lifetime {
    //         let terminated = self.single_quoted_string();
    //         let suffix_start = self.len_consumed();
    //         if terminated {
    //             self.eat_literal_suffix();
    //         }
    //         let kind = Char { terminated };
    //         return Literal { kind, suffix_start };
    //     }

    //     // Either a lifetime or a character literal with
    //     // length greater than 1.

    //     let starts_with_number = self.first().is_digit(10);

    //     // Skip the literal contents.
    //     // First symbol can be a number (which isn't a valid identifier start),
    //     // so skip it without any checks.
    //     self.bump();
    //     self.eat_while(is_id_continue);

    //     // Check if after skipping literal contents we've met a closing
    //     // single quote (which means that user attempted to create a
    //     // string with single quotes).
    //     if self.first() == '\'' {
    //         self.bump();
    //         let kind = Char { terminated: true };
    //         return Literal {
    //             kind,
    //             suffix_start: self.len_consumed(),
    //         };
    //     }

    //     return Lifetime { starts_with_number };
    // }

    fn single_quoted_string(&mut self) -> bool {
        debug_assert!(self.prev() == '\'');
        // Check if it's a one-symbol literal.
        if self.second() == '\'' && self.first() != '\\' {
            self.bump();
            self.bump();
            return true;
        }

        // Literal has more than one symbol.

        // Parse until either quotes are terminated or error is detected.
        loop {
            match self.first() {
                // Quotes are terminated, finish parsing.
                '\'' => {
                    self.bump();
                    return true;
                }
                // Probably beginning of the comment, which we don't want to include
                // to the error report.
                '/' => break,
                // Newline without following '\'' means unclosed quote, stop parsing.
                '\n' if self.second() != '\'' => break,
                // End of file, stop parsing.
                EOF_CHAR if self.is_eof() => break,
                // Escaped slash is considered one character, so bump twice.
                '\\' => {
                    self.bump();
                    self.bump();
                }
                // Skip the character.
                _ => {
                    self.bump();
                }
            }
        }
        // String was not terminated.
        false
    }

    /// Eats double-quoted string and returns true
    /// if string is terminated.
    fn double_quoted_string(&mut self) -> bool {
        debug_assert!(self.prev() == '"');
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    // Bump again to skip escaped character.
                    self.bump();
                }
                _ => (),
            }
        }
        // End of file reached.
        false
    }

    /// Eats the double-quoted string and returns a tuple of
    /// (amount of the '#' symbols, raw string started, raw string terminated)
    fn raw_double_quoted_string(&mut self) -> (usize, bool, bool) {
        debug_assert!(self.prev() == 'r');
        let mut started: bool = false;
        let mut finished: bool = false;

        // Count opening '#' symbols.
        let n_hashes = self.eat_while(|c| c == '#');

        // Check that string is started.
        match self.bump() {
            Some('"') => started = true,
            _ => return (n_hashes, started, finished),
        }

        // Skip the string contents and on each '#' character met, check if this is
        // a raw string termination.
        while !finished {
            self.eat_while(|c| c != '"');

            if self.is_eof() {
                return (n_hashes, started, finished);
            }

            // Eat closing double quote.
            self.bump();

            // Check that amount of closing '#' symbols
            // is equal to the amount of opening ones.
            let mut hashes_left = n_hashes;
            let is_closing_hash = |c| {
                if c == '#' && hashes_left != 0 {
                    hashes_left -= 1;
                    true
                } else {
                    false
                }
            };
            finished = self.eat_while(is_closing_hash) == n_hashes;
        }

        (n_hashes, started, finished)
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }

    // Eats the suffix of the literal, e.g. "UL".
    fn eat_number_suffix(&mut self) -> Option<NumberSuffix> {
        match self.first() {
            'u' | 'U' if self.second() == 'L' => {
                self.bump();
                self.bump();
                Some(NumberSuffix::UL)
            }
            'u' | 'U' => {
                self.bump();
                Some(NumberSuffix::U)
            }
            'L' => {
                self.bump();
                Some(NumberSuffix::L)
            }
            'f' | 'F' => {
                self.bump();
                Some(NumberSuffix::F)
            }
            '_' | 'l' => {
                self.bump();
                Some(NumberSuffix::Invalid(self.first()))
            }
            _ => None,
        }
    }

    // Eats the identifier.
    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }
        self.bump();

        self.eat_while(is_id_continue);
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    /// Returns amount of eaten symbols.
    fn eat_while<F>(&mut self, mut predicate: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let mut eaten: usize = 0;
        while predicate(self.first()) && !self.is_eof() {
            eaten += 1;
            self.bump();
        }

        eaten
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.first() == c {
            self.bump();
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    pub(crate) src: String,
    pos: usize,
    // Index of each line, 0 based
    lines: Vec<usize>,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }

    pub fn is_eof(&self) -> bool {
        match self.kind {
            TokenKind::Eof => true,
            _ => false,
        }
    }
}

impl Lexer {
    fn cursor_number_to_token_number(
        &self,
        kind: CursorNumberKind,
        suffix: Option<NumberSuffix>,
        suffix_start: usize,
        span: &Span,
    ) -> Result<TokenKind, Error> {
        // Forbid invalid suffixes
        match suffix {
            Some(NumberSuffix::Invalid(suffix)) => {
                return Err(Error::new(
                    ErrorKind::InvalidNumberSuffix(suffix.to_string()),
                    self.span_location(&span),
                ));
            }
            _ => {}
        }
        let original_str = &self.src[span.start..span.end];

        // Remove prefix e.g `0x` and suffix e.g `UL`
        let num_str = match kind {
            CursorNumberKind::Int {
                base: NumberBase::Decimal,
                ..
            }
            | CursorNumberKind::Float {
                base: NumberBase::Decimal,
                ..
            } => &self.src[span.start..span.start + suffix_start],
            _ => &self.src[span.start + 2..span.start + suffix_start],
        };

        let suffix_str = &original_str[suffix_start..];
        debug!(
            "original_str=`{}` num_str=`{}` suffix={:?} suffix_start={} suffix_str=`{}`",
            original_str, num_str, suffix, suffix_start, suffix_str
        );

        // Remove underscores
        if num_str.ends_with(&"_") {
            return Err(Error::new(
                ErrorKind::TrailingUnderscoreInNumber,
                self.span_location(span),
            ));
        }
        let s = prepare_num_str_for_parsing(&num_str);

        // Forbid empty string
        if s.is_empty() {
            return Err(Error::new(
                ErrorKind::MissingDigitsInNumber,
                self.span_location(&span),
            ));
        }

        match kind {
            // Forbid octal numbers
            CursorNumberKind::Int {
                base: NumberBase::Octal,
                ..
            }
            | CursorNumberKind::Float {
                base: NumberBase::Octal,
                ..
            } => Err(Error::new(
                ErrorKind::OctalNumber,
                self.span_location(&span),
            )),
            // Forbid empty exponent
            CursorNumberKind::Float {
                empty_exponent: true,
                ..
            } => Err(Error::new(
                ErrorKind::MissingExponentInNumber,
                self.span_location(&span),
            )),
            CursorNumberKind::Int {
                base: NumberBase::Hexadecimal,
                ..
            } => {
                // TODO: report error on number too big
                match suffix {
                    Some(NumberSuffix::U) => {
                        Ok(TokenKind::UInt(u32::from_str_radix(&s, 16).unwrap()))
                    }
                    Some(NumberSuffix::UL) => {
                        Ok(TokenKind::ULong(u64::from_str_radix(&s, 16).unwrap()))
                    }
                    Some(NumberSuffix::L) => {
                        Ok(TokenKind::Long(i64::from_str_radix(&s, 16).unwrap()))
                    }
                    _ => {
                        let num = i64::from_str_radix(&s, 16).expect("Could not parse number");
                        if num <= std::i32::MAX as i64 {
                            Ok(TokenKind::Int(num as i32))
                        } else {
                            Ok(TokenKind::Long(num))
                        }
                    }
                }
            }
            CursorNumberKind::Int {
                base: NumberBase::Binary,
                ..
            } => match suffix {
                Some(NumberSuffix::U) => Ok(TokenKind::UInt(u32::from_str_radix(&s, 2).unwrap())),
                Some(NumberSuffix::UL) => Ok(TokenKind::ULong(u64::from_str_radix(&s, 2).unwrap())),
                Some(NumberSuffix::L) => Ok(TokenKind::Long(i64::from_str_radix(&s, 2).unwrap())),
                Some(NumberSuffix::F) => Err(Error::new(
                    ErrorKind::InvalidNumberSuffix(suffix_str.to_string()),
                    self.span_location(&span),
                )),
                _ => {
                    let num = i64::from_str_radix(&s, 2).expect("Could not parse number");
                    if num <= std::i32::MAX as i64 {
                        Ok(TokenKind::Int(num as i32))
                    } else {
                        Ok(TokenKind::Long(num))
                    }
                }
            },
            CursorNumberKind::Int {
                base: NumberBase::Decimal,
                ..
            } => {
                if s.len() > 1 && s.starts_with('0') {
                    return Err(Error::new(
                        ErrorKind::LeadingZeroInNumber,
                        self.span_location(&span),
                    ));
                }

                // TODO: report error on number too big

                match suffix {
                    Some(NumberSuffix::U) => {
                        Ok(TokenKind::UInt(u32::from_str_radix(&s, 10).unwrap()))
                    }
                    Some(NumberSuffix::UL) => {
                        Ok(TokenKind::ULong(u64::from_str_radix(&s, 10).unwrap()))
                    }
                    Some(NumberSuffix::L) => {
                        Ok(TokenKind::Long(i64::from_str_radix(&s, 10).unwrap()))
                    }
                    Some(NumberSuffix::F) => Ok(TokenKind::Float(s.parse().unwrap())),
                    _ => {
                        let num = i64::from_str_radix(&s, 10).expect("Could not parse number");
                        if num <= std::i32::MAX as i64 {
                            Ok(TokenKind::Int(num as i32))
                        } else {
                            Ok(TokenKind::Long(num))
                        }
                    }
                }
            }
            CursorNumberKind::Float {
                base: NumberBase::Decimal,
                ..
            } => match suffix {
                Some(NumberSuffix::U) | Some(NumberSuffix::UL) | Some(NumberSuffix::L) => {
                    Err(Error::new(
                        ErrorKind::InvalidNumberSuffix(suffix_str.to_string()),
                        self.span_location(&span),
                    ))
                }
                Some(NumberSuffix::F) => Ok(TokenKind::Float(s.parse().unwrap())),
                _ => {
                    let num: f64 = s.parse().unwrap();
                    Ok(TokenKind::Double(num))
                }
            },
            CursorNumberKind::Float { .. } => Err(Error::new(
                ErrorKind::TrailingDotInNumber,
                self.span_location(&span),
            )),
        }
    }

    fn cursor_to_lex_token_kind(
        &mut self,
        kind: CursorTokenKind,
        span: &Span,
    ) -> Result<TokenKind, Error> {
        match kind {
            CursorTokenKind::Newline => {
                debug!("newline: pos={}", self.pos);
                self.lines.push(self.pos);
                Ok(TokenKind::Newline)
            }
            CursorTokenKind::Unknown => Err(Error::new(
                ErrorKind::UnknownChar,
                self.span_location(&span),
            )),
            CursorTokenKind::LineComment => Ok(TokenKind::LineComment),
            CursorTokenKind::BlockComment { terminated } => {
                Ok(TokenKind::BlockComment { terminated })
            }
            CursorTokenKind::Plus => Ok(TokenKind::Plus),
            CursorTokenKind::PlusPlus => Ok(TokenKind::PlusPlus),
            CursorTokenKind::Minus => Ok(TokenKind::Minus),
            CursorTokenKind::Star => Ok(TokenKind::Star),
            CursorTokenKind::Slash => Ok(TokenKind::Slash),
            CursorTokenKind::Comma => Ok(TokenKind::Comma),
            CursorTokenKind::Semicolon => Ok(TokenKind::Semicolon),
            CursorTokenKind::Pipe => Ok(TokenKind::Pipe),
            CursorTokenKind::Ampersand => Ok(TokenKind::Ampersand),
            CursorTokenKind::Dollar => Ok(TokenKind::Dollar),
            CursorTokenKind::At => Ok(TokenKind::At),
            CursorTokenKind::Lesser => Ok(TokenKind::Lesser),
            CursorTokenKind::Greater => Ok(TokenKind::Greater),
            CursorTokenKind::Equal => Ok(TokenKind::Equal),
            CursorTokenKind::Whitespace => Ok(TokenKind::Whitespace),
            CursorTokenKind::Bang => Ok(TokenKind::Bang),
            CursorTokenKind::Percent => Ok(TokenKind::Percent),
            CursorTokenKind::Colon => Ok(TokenKind::Colon),
            CursorTokenKind::Dot => Ok(TokenKind::Dot),
            CursorTokenKind::Number {
                kind,
                suffix,
                suffix_start,
            } => self.cursor_number_to_token_number(kind, suffix, suffix_start, &span),
        }
    }

    pub fn new(src: String) -> Lexer {
        Lexer {
            src,
            pos: 0,
            lines: vec![0],
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        if self.pos >= self.src.len() {
            return Ok(Token::new(TokenKind::Eof, Span::new(self.pos, self.pos)));
        }
        let cursor_token = first_token(&self.src[self.pos..]);
        let start = self.pos;
        self.pos += cursor_token.len;

        debug!(
            "next_token: kind={:?} c={:?} start={} pos={}",
            cursor_token.kind,
            &self.src[start..self.pos],
            start,
            self.pos
        );

        let span = Span::new(start, self.pos);
        let kind = self.cursor_to_lex_token_kind(cursor_token.kind, &span)?;

        Ok(Token::new(kind, span))
    }

    pub fn span_location(&self, span: &Span) -> Location {
        let start_line_i = match self.lines.binary_search(&span.start) {
            Ok(l) => l,
            Err(l) => l - 1,
        };
        let end_line_i = match self.lines.binary_search(&span.end) {
            Ok(l) => l,
            Err(l) => l - 1,
        };
        let start_line_pos = self.lines[start_line_i];
        let end_line_pos = self.lines[end_line_i];

        let start_line_s = &self.src[start_line_pos..span.start];
        let mut start_line_chars = start_line_s.chars();
        let mut start_col = 0;
        while let Some(_) = start_line_chars.next() {
            if start_col == span.start {
                break;
            }

            start_col += 1;
        }

        let end_line_s = &self.src[end_line_pos..span.end];
        let mut end_line_chars = end_line_s.chars();
        let mut end_col = 0;
        while let Some(_) = end_line_chars.next() {
            if end_col == span.end {
                break;
            }

            end_col += 1;
        }

        debug!(
            "token: start_col={} end_col={} start={} start_line={} end={} end_line={}",
            start_col, end_col, span.start, start_line_pos, span.end, end_line_pos
        );

        Location {
            start_pos: span.start,
            start_line: start_line_i + 1,
            start_column: start_col + 1,
            end_pos: span.end,
            end_line: end_line_i + 1,
            end_column: end_col + 1,
        }
    }
}

//     fn expect(
//         &mut self,
//         c: char,
//         start_pos: usize,
//         start_line: usize,
//         start_column: usize,
//     ) -> Result<(), Error> {
//         if self.peek() == Some(c) {
//             self.advance();
//             Ok(())
//         } else {
//             Err(Error::new(
//                 ErrorKind::UnexpectedChar(c),
//                 start_pos,
//                 start_line,
//                 start_column,
//                 self.pos as usize,
//                 self.line as usize,
//                 self.column as usize,
//             ))
//         }
//     }

//     fn string(
//         &mut self,
//         start_pos: usize,
//         start_line: usize,
//         start_column: usize,
//     ) -> Result<Token, Error> {
//         while let Some(c) = self.peek() {
//             match c {
//                 '"' => {
//                     break;
//                 }
//                 '\n' => {
//                     self.advance();
//                     self.newline(start_pos, start_line, start_column);
//                     return Err(Error::new(
//                         ErrorKind::NewlineInString,
//                         start_pos,
//                         start_line,
//                         start_column,
//                         self.pos as usize,
//                         self.line as usize,
//                         self.column as usize,
//                     ));
//                 }
//                 _ => {
//                     self.advance();
//                 }
//             }
//         }
//         self.expect('"', start_pos, start_line, start_column)?;
//         Ok(Token::new(
//             self,
//             TokenKind::TString,
//             start_pos,
//             start_line,
//             start_column,
//         ))
//     }

//     fn identifier(
//         &mut self,
//         start_pos: usize,
//         start_line: usize,
//         start_column: usize,
//     ) -> Result<Token, Error> {
//         while let Some(c) = self.peek() {
//             if c.is_alphanumeric() || c == '_' {
//                 self.advance();
//             } else {
//                 break;
//             }
//         }
//         let s = &self.src[start_pos..self.pos as usize];

//         match s {
//             "true" => Ok(Token::new(
//                 self,
//                 TokenKind::Bool(true),
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "false" => Ok(Token::new(
//                 self,
//                 TokenKind::Bool(false),
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "null" => Ok(Token::new(
//                 self,
//                 TokenKind::Null,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "abstract" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordAbstract,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "annotation" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordAnnotation,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "by" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordBy,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "catch" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordCatch,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "companion" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordCompanion,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "constructor" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordConstructor,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "crossinline" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordCrossinline,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "data" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordData,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "dynamic" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordDynamic,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "enum" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordEnum,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "external" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordExternal,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "final" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordFinal,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "finally" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordFinally,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "import" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordImport,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "infix" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordInfix,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "init" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordInit,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "inline" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordInline,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "inner" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordInner,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "internal" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordInternal,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "lateinit" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordLateinit,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "noinline" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordNoinline,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "open" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordOpen,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "operator" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordOperator,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "out" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordOut,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "override" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordOverride,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "private" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordPrivate,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "protected" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordProtected,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "public" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordPublic,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "reified" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordReified,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "sealed" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordSealed,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "tailrec" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordTailrec,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "vararg" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordVararg,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "where" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordWhere,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "get" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordGet,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "set" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordSet,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "field" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordField,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "property" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordProperty,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "receiver" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordReceiver,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "param" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordParam,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "setparam" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordSetparam,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "delegate" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordDelegate,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "file" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordFile,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "expect" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordExpect,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "actual" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordActual,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "const" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordConst,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "suspend" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordSuspend,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "package" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordPackage,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "class" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordClass,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "interface" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordInterface,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "fun" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordFun,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "object" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordObject,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "val" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordVal,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "var" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordVar,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "typeof" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordTypeof,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "if" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordIf,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "else" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordElse,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "when" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordWhen,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "try" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordTry,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "for" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordFor,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "do" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordDo,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "throw" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordThrow,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "return" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordReturn,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "continue" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordContinue,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "as" => {
//                 if self.match_char('?') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::KeywordAsSafe,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::KeywordAs,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             "is" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordIs,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             "in" => Ok(Token::new(
//                 self,
//                 TokenKind::KeywordIn,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             _ => Ok(Token::new(
//                 self,
//                 TokenKind::Identifier,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//         }
//     }

//     fn skip_whitespace(&mut self) -> Result<Option<Token>, Error> {
//         while !self.is_at_end() {
//             match self.peek() {
//                 None | Some(' ') | Some('\t') | Some('\r') => {
//                     self.advance();
//                 }
//                 Some('#') => match self.peek_next() {
//                     Some('!') => {
//                         let start_pos = self.pos as usize;
//                         let start_line = self.line;
//                         let start_column = self.column as usize;
//                         if self.line != 1 {
//                             self.skip_until('\n');
//                             return Err(Error::new(
//                                 ErrorKind::ShebangNotOnFirstLine,
//                                 start_pos,
//                                 start_line,
//                                 start_column,
//                                 self.pos as usize,
//                                 self.line as usize,
//                                 self.column as usize,
//                             ));
//                         }
//                         self.skip_until('\n');
//                         return Ok(Some(Token::new(
//                             self,
//                             TokenKind::Shebang,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         )));
//                     }
//                     _ => {
//                         return Ok(None);
//                     }
//                 },
//                 Some('/') => match self.peek_next() {
//                     Some('/') => {
//                         let start_pos = self.pos as usize;
//                         let start_line = self.line;
//                         let start_column = self.column as usize;
//                         self.skip_until('\n');
//                         return Ok(Some(Token::new(
//                             self,
//                             TokenKind::Comment,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         )));
//                     }
//                     Some('*') => {
//                         let start_pos = self.pos as usize;
//                         let start_line = self.line;
//                         let start_column = self.column as usize;
//                         self.advance();
//                         self.advance();
//                         while let Some(c) = self.peek() {
//                             match c {
//                                 '*' if self.peek_next() == Some('/') => {
//                                     self.advance();
//                                     self.advance();
//                                     break;
//                                 }
//                                 '\n' => {
//                                     self.advance();
//                                     self.newline(start_pos, start_line, start_column);
//                                 }
//                                 _ => {
//                                     self.advance();
//                                 }
//                             }
//                         }
//                         return Ok(Some(Token::new(
//                             self,
//                             TokenKind::Comment,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         )));
//                     }
//                     _ => {
//                         return Ok(None);
//                     }
//                 },
//                 Some(_) => {
//                     return Ok(None);
//                 }
//             }
//         }
//         Ok(None)
//     }

//     pub fn lex(&mut self) -> Result<Token, Error> {
//         if let Some(tok) = self.skip_whitespace()? {
//             return Ok(tok);
//         }

//         let start_pos = self.pos as usize;
//         let start_line = self.line;
//         let start_column = self.column as usize;

//         let c = self.advance();

//         match c {
//             Some('\n') => Ok(self.newline(start_pos, start_line, start_column)),
//             Some('\\') => {
//                 if self.match_char('n') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapeSequenceNewline,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('r') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapeSequenceCarriageReturn,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('t') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapeSequenceTab,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('b') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapeSequenceBackspace,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('\'') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapedQuote,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('"') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapedDoubleQuote,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('$') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapedDollar,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('\\') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::EscapedBackSlash,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('u') {
//                     for _ in 1..=4 {
//                         match self.peek() {
//                             Some(c) if c.is_ascii_hexdigit() => {
//                                 self.advance();
//                             }
//                             _ => {
//                                 return Err(Error::new(
//                                     ErrorKind::IncompleteUnicodeLiteral,
//                                     start_pos,
//                                     start_line,
//                                     start_column,
//                                     self.pos as usize,
//                                     self.line as usize,
//                                     self.column as usize,
//                                 ))
//                             }
//                         }
//                     }

//                     let s = &self.src[start_pos + 2..self.pos as usize];
//                     let bytes = s
//                         .chars()
//                         .map(|c| c.to_digit(16).unwrap() as u8)
//                         .collect::<Vec<u8>>();
//                     let n: u16 = bytes[0] as u16 * 16 * 16 * 16
//                         + bytes[1] as u16 * 16 * 16
//                         + bytes[2] as u16 * 16
//                         + bytes[3] as u16;
//                     let c = char::try_from(n as u32);
//                     if let Ok(c) = c {
//                         Ok(Token::new(
//                             self,
//                             TokenKind::UnicodeLiteral(c),
//                             start_pos,
//                             start_line,
//                             start_column,
//                         ))
//                     } else {
//                         Err(Error::new(
//                             ErrorKind::InvalidUnicodeLiteral(c.unwrap_err().to_string()),
//                             start_pos,
//                             start_line,
//                             start_column,
//                             self.pos as usize,
//                             self.line as usize,
//                             self.column as usize,
//                         ))
//                     }
//                 } else {
//                     let c = if self.peek().is_some() {
//                         self.advance()
//                     } else {
//                         // Do not advance past the end to avoid giving a `end_column` past the end
//                         None
//                     };

//                     Err(Error::new(
//                         ErrorKind::UnknownEscapeSequence(c),
//                         start_pos,
//                         start_line,
//                         start_column,
//                         self.pos as usize,
//                         self.line as usize,
//                         self.column as usize,
//                     ))
//                 }
//             }
//             Some('@') => Ok(Token::new(
//                 self,
//                 TokenKind::At,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('$') => Ok(Token::new(
//                 self,
//                 TokenKind::Dollar,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('(') => Ok(Token::new(
//                 self,
//                 TokenKind::LeftParen,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some(')') => Ok(Token::new(
//                 self,
//                 TokenKind::RightParen,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('{') => Ok(Token::new(
//                 self,
//                 TokenKind::LeftCurlyBracket,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('}') => Ok(Token::new(
//                 self,
//                 TokenKind::RightCurlyBracket,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('[') => Ok(Token::new(
//                 self,
//                 TokenKind::LeftSquareBracket,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some(']') => Ok(Token::new(
//                 self,
//                 TokenKind::RightSquareBracket,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('+') => {
//                 if self.match_char('+') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::PlusPlus,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::PlusEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Plus,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('<') => {
//                 if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::LesserEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Lesser,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('>') => {
//                 if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::GreaterEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Greater,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('!') => {
//                 if self.match_char('=') {
//                     if self.peek() == Some('=') {
//                         self.advance();
//                         Ok(Token::new(
//                             self,
//                             TokenKind::BangEqualEqual,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         ))
//                     } else {
//                         Ok(Token::new(
//                             self,
//                             TokenKind::BangEqual,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         ))
//                     }
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Bang,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('-') => {
//                 if self.match_char('-') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::MinusMinus,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::MinusEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else if self.match_char('>') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Arrow,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Minus,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('&') => {
//                 if self.match_char('&') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::AmpersandAmpersand,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Ampersand,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some(':') => {
//                 if self.match_char(':') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::ColonColon,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Colon,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some(';') => {
//                 if self.match_char(';') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::SemicolonSemicolon,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Semicolon,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('|') => {
//                 if self.match_char('|') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::PipePipe,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Pipe,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('/') => {
//                 if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::SlashEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Slash,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('*') => {
//                 if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::StarEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Star,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('=') => {
//                 if self.match_char('=') {
//                     if self.match_char('=') {
//                         Ok(Token::new(
//                             self,
//                             TokenKind::EqualEqualEqual,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         ))
//                     } else {
//                         Ok(Token::new(
//                             self,
//                             TokenKind::EqualEqual,
//                             start_pos,
//                             start_line,
//                             start_column,
//                         ))
//                     }
//                 } else if self.match_char('>') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::FatArrow,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Equal,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('%') => {
//                 if self.match_char('=') {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::PercentEqual,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 } else {
//                     Ok(Token::new(
//                         self,
//                         TokenKind::Percent,
//                         start_pos,
//                         start_line,
//                         start_column,
//                     ))
//                 }
//             }
//             Some('.') if !self.peek().map(|c| c.is_digit(10)).unwrap_or(false) => Ok(Token::new(
//                 self,
//                 TokenKind::Dot,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//             Some('.') => self.real(start_pos, start_line, start_column),
//             Some('0') => match self.peek() {
//                 Some('x') | Some('X') => self.hex_number(start_pos, start_line, start_column),
//                 Some('b') | Some('B') => self.bin_number(start_pos, start_line, start_column),
//                 _ => self.integer(start_pos, start_line, start_column),
//             },
//             Some('1') | Some('2') | Some('3') | Some('4') | Some('5') | Some('6') | Some('7')
//             | Some('8') | Some('9') => self.integer(start_pos, start_line, start_column),
//             Some('"') => self.string(start_pos, start_line, start_column),
//             // TODO: expand to more unicode classes
//             Some(c) if c.is_alphanumeric() || c == '_' => {
//                 self.identifier(start_pos, start_line, start_column)
//             }
//             Some(_) => Err(Error::new(
//                 ErrorKind::UnknownChar,
//                 start_pos,
//                 start_line,
//                 start_column,
//                 self.pos as usize,
//                 self.line as usize,
//                 self.column as usize,
//             )),
//             None => Ok(Token::new(
//                 self,
//                 TokenKind::Eof,
//                 start_pos,
//                 start_line,
//                 start_column,
//             )),
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_token() {
        let s = String::from("@");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::At);
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 1);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 0);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_pos, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 2);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::Eof);
        assert_eq!(tok.span.start, 1);
        assert_eq!(tok.span.end, 1);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 1);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 2);
        assert_eq!(location.end_pos, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 2);
    }

    #[test]
    fn single_token_2_chars() {
        let s = String::from("++");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::PlusPlus);
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 2);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 0);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_pos, 2);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 3);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::Eof);
        assert_eq!(tok.span.start, 2);
        assert_eq!(tok.span.end, 2);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 2);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 3);
        assert_eq!(location.end_pos, 2);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 3);
    }

    #[test]
    fn token_with_newline() {
        let s = String::from("@\n$");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::At);
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 1);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 0);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_pos, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 2);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.span.start, 1);
        assert_eq!(tok.span.end, 2);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 1);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 2);
        assert_eq!(location.end_pos, 2);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 1);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::Dollar);
        assert_eq!(tok.span.start, 2);
        assert_eq!(tok.span.end, 3);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 2);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_pos, 3);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 2);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::Eof);
        assert_eq!(tok.span.start, 3);
        assert_eq!(tok.span.end, 3);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 3);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 2);
        assert_eq!(location.end_pos, 3);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 2);
    }

    #[test]
    fn unknown() {
        let s = String::from("§");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert!(tok.is_err());
        let err = tok.unwrap_err();
        assert_eq!(err.kind, ErrorKind::UnknownChar);
        assert_eq!(err.location.start_pos, 0);
        assert_eq!(err.location.start_line, 1);
        assert_eq!(err.location.start_column, 1);
        assert_eq!(err.location.end_pos, 2);
        assert_eq!(err.location.end_line, 1);
        assert_eq!(err.location.end_column, 2);

        let tok = lexer.next_token();
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok.kind, TokenKind::Eof);
        assert_eq!(tok.span.start, 2);
        assert_eq!(tok.span.end, 2);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_pos, 2);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 2);
        assert_eq!(location.end_pos, 2);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 2);
    }

    #[test]
    fn int() {
        let s = String::from("123");
        let mut lexer = Lexer::new(s);
        let tok = lexer.next_token();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(123));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn int_with_underscores() {
        let s = String::from("123_000_000");
        let mut lexer = Lexer::new(s);
        let tok = lexer.next_token();

        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(123_000_000));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 11);
    }

    #[test]
    fn int_with_trailing_underscore() {
        let s = String::from("123_000_000_  ");
        let mut lexer = Lexer::new(s);
        let tok = lexer.next_token();

        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::TrailingUnderscoreInNumber);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 12);
    }

    #[test]
    fn int_zero() {
        let s = String::from("0");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(0i32));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 1);
    }

    #[test]
    fn int_with_leading_zero() {
        let s = String::from("0123");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::LeadingZeroInNumber);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 4);
    }

    #[test]
    fn uint_with_leading_zero() {
        let s = String::from("01u");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::LeadingZeroInNumber);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 3);
    }

    #[test]
    fn uint() {
        let s = String::from("10U");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(10));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn ulong() {
        let s = String::from("10uL");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(10));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn long() {
        let s = String::from("10L");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(10));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn float() {
        let s = String::from("10F");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Float(10f32));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn real_number_with_suffix_l() {
        let s = String::from("1.23L");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidNumberSuffix("L".to_string()));
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 5);
    }

    #[test]
    fn real_number_with_suffix_u() {
        let s = String::from("1.23u");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidNumberSuffix("u".to_string()));
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 5);
    }

    #[test]
    fn real_number_with_suffix_ul() {
        let s = String::from("1.23uL");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidNumberSuffix("uL".to_string()));
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 6);
    }

    #[test]
    fn bin_number_with_suffix_f() {
        let s = String::from("0b101F");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidNumberSuffix("F".to_string()));
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 6);
    }

    #[test]
    fn bin_number() {
        let s = String::from("0b101");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(0b101));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 5);
    }

    #[test]
    fn bin_number_2() {
        let s = String::from("0B1_00000000_00000000_00000000_00000000");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(
            tok.kind,
            TokenKind::Long(0b1_00000000_00000000_00000000_00000000)
        );
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 39);
    }

    #[test]
    fn bin_number_with_suffix_l() {
        let s = String::from("0b101L");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(0b101));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 6);
    }

    #[test]
    fn bin_number_with_suffix_ul() {
        let s = String::from("0b101uL");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(0b101));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 7);
    }

    #[test]
    fn bin_number_with_suffix_u() {
        let s = String::from("0b101U");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(0b101));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 6);
    }

    #[test]
    fn bin_number_missing_digits() {
        let s = String::from("0b");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::MissingDigitsInNumber);
        assert_eq!(tok.location.start_line, 1);
        assert_eq!(tok.location.start_column, 1);
        assert_eq!(tok.location.end_line, 1);
        assert_eq!(tok.location.end_column, 3);
    }

    #[test]
    fn hex_number_missing_digits() {
        let s = String::from("0x");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::MissingDigitsInNumber);
        assert_eq!(tok.location.start_line, 1);
        assert_eq!(tok.location.start_column, 1);
        assert_eq!(tok.location.end_line, 1);
        assert_eq!(tok.location.end_column, 3);
    }

    #[test]
    fn hex_number() {
        let s = String::from("0XdeadBEEF");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(0xdeadbeef));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 10);
    }

    #[test]
    fn hex_number_with_suffix_l() {
        let s = String::from("0XdeadL");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Long(0xdead));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 7);
    }

    #[test]
    fn hex_number_with_suffix_u() {
        let s = String::from("0Xdeadu");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::UInt(0xdead));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 7);
    }

    #[test]
    fn hex_number_with_suffix_ul() {
        let s = String::from("0XdeaduL");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::ULong(0xdead));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 8);
    }

    #[test]
    fn double_number() {
        let s = String::from("123.456");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(123.456));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 7);
    }

    #[test]
    fn double_exp() {
        let s = String::from("123.456e2");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(12345.6));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 9);
    }

    #[test]
    fn double_exp_2() {
        let s = String::from("123.456e+2");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(12345.6));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 10);
    }

    #[test]
    fn double_exp_3() {
        let s = String::from("123.456e-2");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(1.23456));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 10);
    }

    #[test]
    fn double_starting_with_dot() {
        let s = String::from(".1");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Double(0.1));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 2);
    }

    #[test]
    fn double_with_empty_exp() {
        let s = String::from(".1e");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::MissingExponentInNumber);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 3);
    }

    #[test]
    fn dot_not_followed_by_digit() {
        let s = String::from(".@");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Dot);
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 1);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::At);
        assert_eq!(tok.span.start, 1);
        assert_eq!(tok.span.end, 2);
    }

    #[test]
    fn line_comment() {
        let s = String::from("// hello\n1");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::LineComment);
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 8);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.span.start, 8);
        assert_eq!(tok.span.end, 9);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(1));
        assert_eq!(tok.span.start, 9);
        assert_eq!(tok.span.end, 10);

        let location = lexer.span_location(&tok.span);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 2);
    }

    #[test]
    fn block_comment() {
        let s = String::from("/* hello\n 1 */2");
        let mut lexer = Lexer::new(s);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::BlockComment { terminated: true });
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 14);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(2));
        assert_eq!(tok.span.start, 14);
        assert_eq!(tok.span.end, 15);

        let location = lexer.span_location(&tok.span);
        // assert_eq!(location.start_line, 2);
        // assert_eq!(location.start_column, 5);
        // assert_eq!(location.end_line, 2);
        // assert_eq!(location.end_column, 6);
    }

    //     #[test]
    //     fn shebang() {
    //         let s = "#!/bin/cat\n+";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Shebang);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 11);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Newline);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 11);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 1);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Plus);
    //         assert_eq!(tok.location.start_line, 2);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 2);
    //     }

    //     #[test]
    //     fn unknown() {
    //         let s = "§+~";
    //         let mut lexer = Lexer::new(&s);
    //         let tok = lexer.lex();

    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::UnknownChar);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);
    //     }

    //     #[test]
    //     fn shebang_not_on_first_line() {
    //         let s = "\n#!/bin/cat\n+";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Newline);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 1);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::ShebangNotOnFirstLine);
    //         assert_eq!(tok.location.start_line, 2);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 11);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Newline);
    //         assert_eq!(tok.location.start_line, 2);
    //         assert_eq!(tok.location.start_column, 11);
    //         assert_eq!(tok.location.end_line, 3);
    //         assert_eq!(tok.location.end_column, 1);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Plus);
    //         assert_eq!(tok.location.start_line, 3);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 3);
    //         assert_eq!(tok.location.end_column, 2);
    //     }

    //     #[test]
    //     fn empty_string() {
    //         let s = r##"
    //             ""
    //             "##;
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Newline);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 1);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::TString);
    //         assert_eq!(tok.location.start_line, 2);
    //         assert_eq!(tok.location.start_column, 13);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 15);
    //     }

    //     #[test]
    //     fn string() {
    //         let s = r##"
    //             "abc123老虎老虎"
    //             "##;
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Newline);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 1);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::TString);
    //         assert_eq!(tok.location.start_line, 2);
    //         assert_eq!(tok.location.start_column, 13);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 25);
    //         assert_eq!(
    //             &s[tok.location.start_pos..tok.location.end_pos],
    //             "\"abc123老虎老虎\""
    //         );
    //     }

    //     #[test]
    //     fn unterminated_string() {
    //         let s = "\"";
    //         let mut lexer = Lexer::new(&s);
    //         let tok = lexer.lex();

    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::UnexpectedChar('"'));
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);
    //     }

    //     #[test]
    //     fn newline_in_string() {
    //         let s = "\"\n";
    //         let mut lexer = Lexer::new(&s);
    //         let tok = lexer.lex();

    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::NewlineInString);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 2);
    //         assert_eq!(tok.location.end_column, 1);
    //     }

    //     #[test]
    //     fn bool() {
    //         let s = " true false";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Bool(true));
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 6);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Bool(false));
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 7);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 12);
    //     }

    //     #[test]
    //     fn null() {
    //         let s = " null ";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Null);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 6);
    //     }

    //     #[test]
    //     fn keyword() {
    //         // TODO: many more keywords
    //         let s = " abstract as as?";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::KeywordAbstract);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 10);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::KeywordAs);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 11);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 13);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::KeywordAsSafe);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 14);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 17);
    //     }

    //     #[test]
    //     fn identifier() {
    //         // `ƍ` is of category Ll (letter lowercase)
    //         // `ᴽ` is of category Lm (letter modifier)
    //         // `א` is of categoy Lo (other letter)
    //         // `ᾯ` is of category Lt (letter titlecase)
    //         // `Ʊ` is of category Lu (letter uppercase)
    //         // `ᛮ` is of category Nl (letter number)
    //         let s = " _ _a B_ ᛮƍᴽאᾯƱ";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Identifier);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);
    //         assert_eq!(&s[tok.location.start_pos..tok.location.end_pos], "_");

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Identifier);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 4);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 6);
    //         assert_eq!(&s[tok.location.start_pos..tok.location.end_pos], "_a");

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Identifier);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 7);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 9);
    //         assert_eq!(&s[tok.location.start_pos..tok.location.end_pos], "B_");

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Identifier);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 10);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 16);
    //         assert_eq!(&s[tok.location.start_pos..tok.location.end_pos], "ᛮƍᴽאᾯƱ");
    //     }

    //     #[test]
    //     fn empty_comment_multiline() {
    //         let s = "/**/+";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Comment);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 5);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Plus);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 5);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 6);
    //     }

    //     #[test]
    //     fn single_char_tokens() {
    //         let s = "-*/:;@$";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Minus);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Star);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Slash);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Colon);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 4);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 5);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Semicolon);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 5);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 6);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::At);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 6);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 7);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Dollar);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 7);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 8);
    //     }

    //     #[test]
    //     fn plus() {
    //         let s = "+++ +=";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::PlusPlus);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Plus);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::PlusEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 5);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 7);
    //     }

    //     #[test]
    //     fn minus() {
    //         let s = "--- ->";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::MinusMinus);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Minus);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Arrow);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 5);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 7);
    //     }

    //     #[test]
    //     fn ampersand() {
    //         let s = "&&&";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::AmpersandAmpersand);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Ampersand);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn pipe() {
    //         let s = "|||";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::PipePipe);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Pipe);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn colon() {
    //         let s = ":::";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::ColonColon);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Colon);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn semicolon() {
    //         let s = ";;;";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::SemicolonSemicolon);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Semicolon);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn star() {
    //         let s = "**=";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Star);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::StarEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn slash() {
    //         let s = "/ /=";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Slash);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::SlashEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 5);
    //     }

    //     #[test]
    //     fn percent() {
    //         let s = "%%=";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Percent);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::PercentEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn equal() {
    //         let s = "== = === =>";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EqualEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Equal);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 4);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 5);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EqualEqualEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 6);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 9);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::FatArrow);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 10);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 12);
    //     }

    //     #[test]
    //     fn bang() {
    //         let s = "!!=!==";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Bang);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::BangEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::BangEqualEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 4);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 7);
    //     }

    //     #[test]
    //     fn smaller() {
    //         let s = "<<=";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Lesser);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::LesserEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn greater() {
    //         let s = ">>=";
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::Greater);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 2);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::GreaterEqual);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 2);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 4);
    //     }

    //     #[test]
    //     fn escape_sequence_literals() {
    //         let s = r##"\n\r\b\t\'\"\$\\\+\"##;
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapeSequenceNewline);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 3);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapeSequenceCarriageReturn);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 3);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 5);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapeSequenceBackspace);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 5);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 7);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapeSequenceTab);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 7);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 9);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapedQuote);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 9);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 11);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapedDoubleQuote);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 11);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 13);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapedDollar);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 13);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 15);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::EscapedBackSlash);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 15);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 17);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::UnknownEscapeSequence(Some('+')));
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 17);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 19);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::UnknownEscapeSequence(None));
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 19);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 20);
    //     }

    //     #[test]
    //     fn unicode_literal() {
    //         let s = r##"\uabcd \u123 \ud800"##;
    //         let mut lexer = Lexer::new(&s);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_ok(), true);
    //         let tok = tok.as_ref().unwrap();
    //         assert_eq!(tok.kind, TokenKind::UnicodeLiteral('ꯍ'));
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 1);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 7);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(tok.kind, ErrorKind::IncompleteUnicodeLiteral);
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 8);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 13);

    //         let tok = lexer.lex();
    //         assert_eq!(tok.as_ref().is_err(), true);
    //         let tok = tok.as_ref().unwrap_err();
    //         assert_eq!(
    //             tok.kind,
    //             ErrorKind::InvalidUnicodeLiteral(
    //                 "converted integer out of range for `char`".to_string()
    //             )
    //         );
    //         assert_eq!(tok.location.start_line, 1);
    //         assert_eq!(tok.location.start_column, 14);
    //         assert_eq!(tok.location.end_line, 1);
    //         assert_eq!(tok.location.end_column, 20);
    //     }
}
