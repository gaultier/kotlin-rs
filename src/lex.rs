use crate::cursor::*;
use crate::error::*;
use crate::session::{Session, Span};
use log::debug;
use std::fmt;

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
    Elvis,
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
    DotDot,
    AmpersandAmpersand,
    PipePipe,
    Bang,
    BangBang,
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
    Pound,
    QuestionMark,
    Newline,
    EscapeSequenceNewline,
    EscapeSequenceCarriageReturn,
    EscapeSequenceTab,
    EscapeSequenceBackspace,
    EscapedQuote,
    EscapedDoubleQuote,
    EscapedBackSlash,
    EscapedDollar,
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Float(f32),
    Double(f64),
    Char(char),
    Shebang,
    Comment,
    TString,
    Boolean(bool),
    Null,
    KeywordAbstract,
    KeywordActual,
    KeywordAnnotation,
    KeywordAs(bool), // Safe? e.g `as?`
    KeywordBreak,
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
    KeywordPrintln,
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
    KeywordWhile,
    Identifier,
    Whitespace,
    LineComment,
    BlockComment { terminated: bool },
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Elvis => write!(f, "?:"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::PlusPlus => write!(f, "++"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::MinusMinus => write!(f, "--"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::EqualEqualEqual => write!(f, "==="),
            TokenKind::Comma => write!(f, ","),
            TokenKind::At => write!(f, "@"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftSquareBracket => write!(f, "["),
            TokenKind::RightSquareBracket => write!(f, "]"),
            TokenKind::LeftCurlyBracket => write!(f, "{{"),
            TokenKind::RightCurlyBracket => write!(f, "}}"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::AmpersandAmpersand => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangBang => write!(f, "!!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::BangEqualEqual => write!(f, "!=="),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::SemicolonSemicolon => write!(f, ";;"),
            TokenKind::PlusEqual => write!(f, "+="),
            TokenKind::MinusEqual => write!(f, "-="),
            TokenKind::StarEqual => write!(f, "*="),
            TokenKind::SlashEqual => write!(f, "/="),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::PercentEqual => write!(f, "%="),
            TokenKind::Lesser => write!(f, "<"),
            TokenKind::LesserEqual => write!(f, "<="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::Pound => write!(f, "#"),
            TokenKind::QuestionMark => write!(f, "?"),
            TokenKind::Newline => write!(f, "newline"),
            TokenKind::EscapeSequenceNewline => write!(f, "\\n"),
            TokenKind::EscapeSequenceCarriageReturn => write!(f, "\\r"),
            TokenKind::EscapeSequenceTab => write!(f, "\\t"),
            TokenKind::EscapeSequenceBackspace => write!(f, "\\"),
            TokenKind::EscapedQuote => write!(f, "\'"),
            TokenKind::EscapedDoubleQuote => write!(f, "\\\""),
            TokenKind::EscapedBackSlash => write!(f, "\\\\"),
            TokenKind::EscapedDollar => write!(f, "\\$"),
            TokenKind::Int(_) => write!(f, ""),
            TokenKind::Long(_) => write!(f, ""),
            TokenKind::UInt(_) => write!(f, ""),
            TokenKind::ULong(_) => write!(f, ""),
            TokenKind::Float(_) => write!(f, ""),
            TokenKind::Double(_) => write!(f, ""),
            TokenKind::Char(_) => write!(f, ""),
            TokenKind::Shebang => write!(f, "#!"),
            TokenKind::Comment => write!(f, "//"),
            TokenKind::TString => write!(f, ""),
            TokenKind::Boolean(_) => write!(f, ""),
            TokenKind::Null => write!(f, "null"),
            TokenKind::KeywordAbstract => write!(f, "abstract"),
            TokenKind::KeywordActual => write!(f, "actual"),
            TokenKind::KeywordAnnotation => write!(f, "annotation"),
            TokenKind::KeywordAs(safe) => write!(f, "as{}", if *safe { "?" } else { "" }),
            TokenKind::KeywordBreak => write!(f, "break"),
            TokenKind::KeywordBy => write!(f, "by"),
            TokenKind::KeywordCatch => write!(f, "catch"),
            TokenKind::KeywordClass => write!(f, "class"),
            TokenKind::KeywordCompanion => write!(f, "companion"),
            TokenKind::KeywordConst => write!(f, "const"),
            TokenKind::KeywordConstructor => write!(f, "constructor"),
            TokenKind::KeywordContinue => write!(f, "continue"),
            TokenKind::KeywordCrossinline => write!(f, "crossinline"),
            TokenKind::KeywordData => write!(f, "data"),
            TokenKind::KeywordDelegate => write!(f, "delegate"),
            TokenKind::KeywordDo => write!(f, "do"),
            TokenKind::KeywordDynamic => write!(f, "dynamic"),
            TokenKind::KeywordElse => write!(f, "else"),
            TokenKind::KeywordEnum => write!(f, "enum"),
            TokenKind::KeywordExpect => write!(f, "expect"),
            TokenKind::KeywordExternal => write!(f, "external"),
            TokenKind::KeywordField => write!(f, "field"),
            TokenKind::KeywordFile => write!(f, "file"),
            TokenKind::KeywordFinal => write!(f, "final"),
            TokenKind::KeywordFinally => write!(f, "finally"),
            TokenKind::KeywordFor => write!(f, "for"),
            TokenKind::KeywordFun => write!(f, "fun"),
            TokenKind::KeywordGet => write!(f, "get"),
            TokenKind::KeywordIf => write!(f, "if"),
            TokenKind::KeywordImport => write!(f, "import"),
            TokenKind::KeywordIn => write!(f, "in"),
            TokenKind::KeywordInfix => write!(f, "infix"),
            TokenKind::KeywordInit => write!(f, "init"),
            TokenKind::KeywordInline => write!(f, "inline"),
            TokenKind::KeywordInner => write!(f, "inner"),
            TokenKind::KeywordInterface => write!(f, "interface"),
            TokenKind::KeywordInternal => write!(f, "internal"),
            TokenKind::KeywordIs => write!(f, "is"),
            TokenKind::KeywordLateinit => write!(f, "lateinit"),
            TokenKind::KeywordNoinline => write!(f, "noinline"),
            TokenKind::KeywordObject => write!(f, "object"),
            TokenKind::KeywordOpen => write!(f, "open"),
            TokenKind::KeywordOperator => write!(f, "operator"),
            TokenKind::KeywordOut => write!(f, "out"),
            TokenKind::KeywordOverride => write!(f, "override"),
            TokenKind::KeywordPackage => write!(f, "package"),
            TokenKind::KeywordParam => write!(f, "param"),
            TokenKind::KeywordPrintln => write!(f, "println"),
            TokenKind::KeywordPrivate => write!(f, "private"),
            TokenKind::KeywordProperty => write!(f, "property"),
            TokenKind::KeywordProtected => write!(f, "protected"),
            TokenKind::KeywordPublic => write!(f, "public"),
            TokenKind::KeywordReceiver => write!(f, "receiver"),
            TokenKind::KeywordReified => write!(f, "reified"),
            TokenKind::KeywordReturn => write!(f, "return"),
            TokenKind::KeywordSealed => write!(f, "sealed"),
            TokenKind::KeywordSet => write!(f, "set"),
            TokenKind::KeywordSetparam => write!(f, "setparam"),
            TokenKind::KeywordSuspend => write!(f, "suspend"),
            TokenKind::KeywordTailrec => write!(f, "tailrec"),
            TokenKind::KeywordThrow => write!(f, "throw"),
            TokenKind::KeywordTry => write!(f, "try"),
            TokenKind::KeywordTypeof => write!(f, "typeof"),
            TokenKind::KeywordVal => write!(f, "val"),
            TokenKind::KeywordVar => write!(f, "var"),
            TokenKind::KeywordVararg => write!(f, "vararg"),
            TokenKind::KeywordWhen => write!(f, "when"),
            TokenKind::KeywordWhere => write!(f, "where"),
            TokenKind::KeywordWhile => write!(f, "while"),
            TokenKind::Identifier => write!(f, "identifier (e.g abc)"),
            TokenKind::Whitespace => write!(f, " "),
            TokenKind::LineComment => write!(f, "// .."),
            TokenKind::BlockComment { .. } => write!(f, "/* .. */"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
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
    Elvis,
    Number {
        kind: CursorNumberKind,
        suffix: Option<NumberSuffix>,
        suffix_start: usize,
    },
    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
    AmpersandAmpersand,
    PipePipe,
    At,
    Dollar,
    Semicolon,
    Bang,
    BangBang,
    Lesser,
    LesserEqual,
    Greater,
    GreaterEqual,
    Percent,
    PercentEqual,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    BangEqual,
    BangEqualEqual,
    Newline,
    Whitespace,
    Colon,
    ColonColon,
    Comma,
    Dot,
    DotDot,
    Arrow,
    FatArrow,
    LineComment,
    BlockComment {
        terminated: bool,
    },
    Char {
        terminated: bool,
    },
    TString {
        terminated: bool,
    },
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Pound,
    Question,
    Identifier,
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
fn first_token(input: &str, lines: &mut Vec<usize>, start_pos: usize) -> CursorToken {
    debug_assert!(!input.is_empty());
    Cursor::new(input).advance_token(lines, start_pos)
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
fn is_whitespace(c: char) -> bool {
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
fn is_id_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
fn is_id_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn prepare_num_str_for_parsing(s: &str) -> String {
    s.to_string().replace("_", "")
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    fn advance_token(&mut self, lines: &mut Vec<usize>, start_pos: usize) -> CursorToken {
        let first_char = self.bump().unwrap();
        let token_kind = match first_char {
            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(lines, start_pos),
                '=' => {
                    self.bump();
                    CursorTokenKind::SlashEqual
                }
                _ => CursorTokenKind::Slash,
            },
            '\r' if self.first() == '\n' => {
                self.bump();
                CursorTokenKind::Newline
            }
            '\n' => CursorTokenKind::Newline,
            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident(),
            '.' if self.match_char('.') => CursorTokenKind::DotDot,
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
            '(' => CursorTokenKind::OpenParen,
            ')' => CursorTokenKind::CloseParen,
            '{' => CursorTokenKind::OpenBrace,
            '}' => CursorTokenKind::CloseBrace,
            '[' => CursorTokenKind::OpenBracket,
            ']' => CursorTokenKind::CloseBracket,
            '@' => CursorTokenKind::At,
            '#' => CursorTokenKind::Pound,
            '?' if self.match_char(':') => CursorTokenKind::Elvis,
            '?' => CursorTokenKind::Question,
            ':' if self.match_char(':') => CursorTokenKind::ColonColon,
            ':' => CursorTokenKind::Colon,
            '$' => CursorTokenKind::Dollar,
            '=' if self.match_char('>') => CursorTokenKind::FatArrow,
            '=' if self.match_char('=') => {
                if self.match_char('=') {
                    CursorTokenKind::EqualEqualEqual
                } else {
                    CursorTokenKind::EqualEqual
                }
            }
            '=' => CursorTokenKind::Equal,
            '!' if self.match_char('=') => {
                if self.match_char('=') {
                    CursorTokenKind::BangEqualEqual
                } else {
                    CursorTokenKind::BangEqual
                }
            }
            '!' if self.match_char('!') => CursorTokenKind::BangBang,
            '!' => CursorTokenKind::Bang,
            '<' if self.match_char('=') => CursorTokenKind::LesserEqual,
            '<' => CursorTokenKind::Lesser,
            '>' if self.match_char('=') => CursorTokenKind::GreaterEqual,
            '>' => CursorTokenKind::Greater,
            '-' if self.match_char('-') => CursorTokenKind::MinusMinus,
            '-' if self.match_char('=') => CursorTokenKind::MinusEqual,
            '-' if self.match_char('>') => CursorTokenKind::Arrow,
            '-' => CursorTokenKind::Minus,
            '&' if self.match_char('&') => CursorTokenKind::AmpersandAmpersand,
            '|' if self.match_char('|') => CursorTokenKind::PipePipe,
            '+' if self.match_char('+') => CursorTokenKind::PlusPlus,
            '+' if self.match_char('=') => CursorTokenKind::PlusEqual,
            '+' => CursorTokenKind::Plus,
            '*' if self.match_char('=') => CursorTokenKind::StarEqual,
            '*' => CursorTokenKind::Star,
            '%' if self.match_char('=') => CursorTokenKind::PercentEqual,
            '%' => CursorTokenKind::Percent,

            // Character literal.
            '\'' => self.char_literal(),

            // String literal.
            '"' => {
                let terminated = self.double_quoted_string();
                CursorTokenKind::TString { terminated }
            }
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

    fn block_comment(&mut self, lines: &mut Vec<usize>, start_pos: usize) -> CursorTokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        let mut eaten: usize = 1;
        self.bump();
        eaten += 1;
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            eaten += 1;
            match c {
                '/' if self.first() == '*' => {
                    eaten += 1;
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    eaten += 1;
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                '\n' => {
                    debug!(
                        "new line inside block comment at start_pos={} eaten={} pos={}",
                        start_pos,
                        eaten,
                        start_pos + eaten
                    );
                    lines.push(start_pos + eaten);
                }
                _ => (),
            }
        }

        CursorTokenKind::BlockComment {
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self) -> CursorTokenKind {
        debug_assert!(is_whitespace(self.prev()));

        while !self.is_eof() {
            match self.first() {
            | '\t'
            | '\u{000B}' // vertical tab
            | '\u{000C}' // form feed
            | ' ' => { self.bump(); }
            | '\r' if self.second() != '\n' =>{ self.bump(); }
            _ => break,
            }
        }

        CursorTokenKind::Whitespace
    }

    fn ident(&mut self) -> CursorTokenKind {
        debug_assert!(is_id_start(self.prev()));
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(is_id_continue);
        CursorTokenKind::Identifier
    }

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

    fn char_literal(&mut self) -> CursorTokenKind {
        debug_assert!(self.prev() == '\'');

        let terminated = self.single_quoted_string();
        CursorTokenKind::Char { terminated }
    }

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
                // Escaped sequences are considered one character, so bump twice.
                '\r' | '\t' | '\u{0008}' | '\\' => {
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
        while !self.is_eof() {
            match self.first() {
                '"' => {
                    self.bump();
                    return true;
                }
                '\\' if self.second() == '\\' || self.second() == '"' => {
                    // Bump again to skip escaped character.
                    self.bump();
                    self.bump();
                }
                '\n' => {
                    return false;
                }
                _ => {
                    self.bump();
                }
            }
        }
        // End of file reached.
        false
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
pub struct Lexer<'a> {
    session: &'a Session<'a>,
    pos: usize,
    // Index of each line, 0 based
    lines: Vec<usize>,
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

    pub fn is_unsignificant_ws(&self) -> bool {
        match self.kind {
            TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment { .. } => true,
            _ => false,
        }
    }
}

impl<'a> Lexer<'a> {
    fn cursor_identifier_to_token_identifier(&self, span: &Span) -> TokenKind {
        let s = &self.session.src[span.start..span.end];

        match s {
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "null" => TokenKind::Null,
            "break" => TokenKind::KeywordBreak,
            "abstract" => TokenKind::KeywordAbstract,
            "annotation" => TokenKind::KeywordAnnotation,
            "by" => TokenKind::KeywordBy,
            "catch" => TokenKind::KeywordCatch,
            "companion" => TokenKind::KeywordCompanion,
            "constructor" => TokenKind::KeywordConstructor,
            "crossinline" => TokenKind::KeywordCrossinline,
            "data" => TokenKind::KeywordData,
            "dynamic" => TokenKind::KeywordDynamic,
            "enum" => TokenKind::KeywordEnum,
            "external" => TokenKind::KeywordExternal,
            "final" => TokenKind::KeywordFinal,
            "finally" => TokenKind::KeywordFinally,
            "import" => TokenKind::KeywordImport,
            "infix" => TokenKind::KeywordInfix,
            "init" => TokenKind::KeywordInit,
            "inline" => TokenKind::KeywordInline,
            "inner" => TokenKind::KeywordInner,
            "internal" => TokenKind::KeywordInternal,
            "lateinit" => TokenKind::KeywordLateinit,
            "noinline" => TokenKind::KeywordNoinline,
            "open" => TokenKind::KeywordOpen,
            "operator" => TokenKind::KeywordOperator,
            "out" => TokenKind::KeywordOut,
            "override" => TokenKind::KeywordOverride,
            "println" => TokenKind::KeywordPrintln,
            "private" => TokenKind::KeywordPrivate,
            "protected" => TokenKind::KeywordProtected,
            "public" => TokenKind::KeywordPublic,
            "reified" => TokenKind::KeywordReified,
            "sealed" => TokenKind::KeywordSealed,
            "tailrec" => TokenKind::KeywordTailrec,
            "vararg" => TokenKind::KeywordVararg,
            "where" => TokenKind::KeywordWhere,
            "while" => TokenKind::KeywordWhile,
            "get" => TokenKind::KeywordGet,
            "set" => TokenKind::KeywordSet,
            "field" => TokenKind::KeywordField,
            "property" => TokenKind::KeywordProperty,
            "receiver" => TokenKind::KeywordReceiver,
            "param" => TokenKind::KeywordParam,
            "setparam" => TokenKind::KeywordSetparam,
            "delegate" => TokenKind::KeywordDelegate,
            "file" => TokenKind::KeywordFile,
            "expect" => TokenKind::KeywordExpect,
            "actual" => TokenKind::KeywordActual,
            "const" => TokenKind::KeywordConst,
            "suspend" => TokenKind::KeywordSuspend,
            "package" => TokenKind::KeywordPackage,
            "class" => TokenKind::KeywordClass,
            "interface" => TokenKind::KeywordInterface,
            "fun" => TokenKind::KeywordFun,
            "object" => TokenKind::KeywordObject,
            "val" => TokenKind::KeywordVal,
            "var" => TokenKind::KeywordVar,
            "typeof" => TokenKind::KeywordTypeof,
            "if" => TokenKind::KeywordIf,
            "else" => TokenKind::KeywordElse,
            "when" => TokenKind::KeywordWhen,
            "try" => TokenKind::KeywordTry,
            "for" => TokenKind::KeywordFor,
            "do" => TokenKind::KeywordDo,
            "throw" => TokenKind::KeywordThrow,
            "return" => TokenKind::KeywordReturn,
            "continue" => TokenKind::KeywordContinue,
            "as" => TokenKind::KeywordAs(false),
            "is" => TokenKind::KeywordIs,
            "in" => TokenKind::KeywordIn,
            _ => TokenKind::Identifier,
        }
    }

    fn cursor_number_to_token_number(
        &self,
        kind: CursorNumberKind,
        suffix: Option<NumberSuffix>,
        suffix_start: usize,
        span: &Span,
    ) -> Result<TokenKind, Error> {
        // Forbid invalid suffixes
        if let Some(NumberSuffix::Invalid(suffix)) = suffix {
            return Err(Error::new(
                ErrorKind::InvalidNumberSuffix(suffix.to_string()),
                self.span_location(&span),
            ));
        }
        let original_str = &self.session.src[span.start..span.end];

        // Remove prefix e.g `0x` and suffix e.g `UL`
        let num_str = match kind {
            CursorNumberKind::Int {
                base: NumberBase::Decimal,
                ..
            }
            | CursorNumberKind::Float {
                base: NumberBase::Decimal,
                ..
            } => &self.session.src[span.start..span.start + suffix_start],
            _ => &self.session.src[span.start + 2..span.start + suffix_start],
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
            CursorTokenKind::Elvis => Ok(TokenKind::Elvis),
            CursorTokenKind::TString { terminated: false } => Err(Error::new(
                ErrorKind::UnterminatedString,
                self.span_location(&span),
            )),
            CursorTokenKind::TString { terminated: true } => Ok(TokenKind::TString),
            CursorTokenKind::Newline => {
                debug!("newline: pos={}", self.pos);
                self.lines.push(self.pos);
                Ok(TokenKind::Newline)
            }
            CursorTokenKind::Unknown => Err(Error::new(
                ErrorKind::UnknownChar,
                self.span_location(&span),
            )),
            CursorTokenKind::Identifier => Ok(self.cursor_identifier_to_token_identifier(&span)),
            CursorTokenKind::LineComment => Ok(TokenKind::LineComment),
            CursorTokenKind::BlockComment { terminated } => {
                Ok(TokenKind::BlockComment { terminated })
            }
            CursorTokenKind::Plus => Ok(TokenKind::Plus),
            CursorTokenKind::PlusPlus => Ok(TokenKind::PlusPlus),
            CursorTokenKind::PlusEqual => Ok(TokenKind::PlusEqual),
            CursorTokenKind::Minus => Ok(TokenKind::Minus),
            CursorTokenKind::MinusMinus => Ok(TokenKind::MinusMinus),
            CursorTokenKind::MinusEqual => Ok(TokenKind::MinusEqual),
            CursorTokenKind::Star => Ok(TokenKind::Star),
            CursorTokenKind::StarEqual => Ok(TokenKind::StarEqual),
            CursorTokenKind::Slash => Ok(TokenKind::Slash),
            CursorTokenKind::SlashEqual => Ok(TokenKind::SlashEqual),
            CursorTokenKind::Arrow => Ok(TokenKind::Arrow),
            CursorTokenKind::FatArrow => Ok(TokenKind::FatArrow),
            CursorTokenKind::Comma => Ok(TokenKind::Comma),
            CursorTokenKind::Semicolon => Ok(TokenKind::Semicolon),
            CursorTokenKind::PipePipe => Ok(TokenKind::PipePipe),
            CursorTokenKind::AmpersandAmpersand => Ok(TokenKind::AmpersandAmpersand),
            CursorTokenKind::Dollar => Ok(TokenKind::Dollar),
            CursorTokenKind::At => Ok(TokenKind::At),
            CursorTokenKind::Lesser => Ok(TokenKind::Lesser),
            CursorTokenKind::LesserEqual => Ok(TokenKind::LesserEqual),
            CursorTokenKind::Greater => Ok(TokenKind::Greater),
            CursorTokenKind::GreaterEqual => Ok(TokenKind::GreaterEqual),
            CursorTokenKind::Equal => Ok(TokenKind::Equal),
            CursorTokenKind::EqualEqual => Ok(TokenKind::EqualEqual),
            CursorTokenKind::EqualEqualEqual => Ok(TokenKind::EqualEqualEqual),
            CursorTokenKind::Whitespace => Ok(TokenKind::Whitespace),
            CursorTokenKind::Bang => Ok(TokenKind::Bang),
            CursorTokenKind::BangBang => Ok(TokenKind::BangBang),
            CursorTokenKind::BangEqual => Ok(TokenKind::BangEqual),
            CursorTokenKind::BangEqualEqual => Ok(TokenKind::BangEqualEqual),
            CursorTokenKind::Percent => Ok(TokenKind::Percent),
            CursorTokenKind::PercentEqual => Ok(TokenKind::PercentEqual),
            CursorTokenKind::Colon => Ok(TokenKind::Colon),
            CursorTokenKind::ColonColon => Ok(TokenKind::ColonColon),
            CursorTokenKind::Dot => Ok(TokenKind::Dot),
            CursorTokenKind::DotDot => Ok(TokenKind::DotDot),
            CursorTokenKind::OpenParen => Ok(TokenKind::LeftParen),
            CursorTokenKind::CloseParen => Ok(TokenKind::RightParen),
            CursorTokenKind::OpenBrace => Ok(TokenKind::LeftCurlyBracket),
            CursorTokenKind::CloseBrace => Ok(TokenKind::RightCurlyBracket),
            CursorTokenKind::OpenBracket => Ok(TokenKind::LeftSquareBracket),
            CursorTokenKind::CloseBracket => Ok(TokenKind::RightSquareBracket),
            CursorTokenKind::Question => Ok(TokenKind::QuestionMark),
            CursorTokenKind::Pound => Ok(TokenKind::Pound),
            CursorTokenKind::Char { terminated: false } => Err(Error::new(
                ErrorKind::UnterminatedChar,
                self.span_location(&span),
            )),
            CursorTokenKind::Char { terminated: true } => {
                // Trim surrounding quotes to get content
                let c_str = &self.session.src[span.start + 1..span.end - 1];
                let c_chars = c_str.chars().collect::<Vec<_>>();
                let slice: &[char] = &c_chars;
                let c: char = match slice {
                    [c] => *c,
                    ['\\', 'r'] => '\r',
                    ['\\', 't'] => '\t',
                    ['\\', 'n'] => '\n',
                    ['\\', 'b'] => '\u{0008}',
                    ['\\', '\''] => '\'',
                    ['\\', '\\'] => '\\',
                    // Unicode literal
                    ['\\', 'u', a, b, c, d]
                        if a.is_ascii_hexdigit()
                            && b.is_ascii_hexdigit()
                            && c.is_ascii_hexdigit()
                            && d.is_ascii_hexdigit() =>
                    {
                        self.char_literal_unicode([*a, *b, *c, *d], &span)?
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorKind::InvalidCharLiteral,
                            self.span_location(&span),
                        ));
                    }
                };

                Ok(TokenKind::Char(c))
            }
            CursorTokenKind::Number {
                kind,
                suffix,
                suffix_start,
            } => self.cursor_number_to_token_number(kind, suffix, suffix_start, &span),
        }
    }

    fn char_literal_unicode(&self, chars: [char; 4], span: &Span) -> Result<char, Error> {
        let [a, b, c, d] = chars;
        let num: u32 = d.to_digit(16).unwrap()
            + c.to_digit(16).unwrap() * 0x10
            + b.to_digit(16).unwrap() * 0x100
            + a.to_digit(16).unwrap() * 0x1000;

        std::char::from_u32(num)
            .ok_or_else(|| Error::new(ErrorKind::InvalidCharLiteral, self.span_location(&span)))
    }

    pub fn new(session: &'a Session<'a>) -> Lexer<'a> {
        Lexer {
            session,
            pos: 0,
            lines: vec![0],
        }
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        if self.pos >= self.session.src.len() {
            return Ok(Token::new(TokenKind::Eof, Span::new(self.pos, self.pos)));
        }
        let cursor_token = first_token(&self.session.src[self.pos..], &mut self.lines, self.pos);
        let start = self.pos;
        self.pos += cursor_token.len;

        debug!(
            "next_token: kind={:?} c={:?} start={} pos={}",
            cursor_token.kind,
            &self.session.src[start..self.pos],
            start,
            self.pos
        );

        let span = Span::new(start, self.pos);
        let kind = self.cursor_to_lex_token_kind(cursor_token.kind, &span)?;

        Ok(Token::new(kind, span))
    }

    pub fn lex(&mut self) -> Result<(Vec<Token>, Session), Error> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token.kind == TokenKind::Eof {
                tokens.push(token);
                tokens.push(token);
                break;
            } else {
                tokens.push(token);
            }
        }

        Ok((
            tokens,
            Session {
                lines: self.lines.clone(),
                src: self.session.src,
                file: self.session.file,
            },
        ))
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

        let start_line_s = &self.session.src[start_line_pos..span.start];
        let start_col = start_line_s
            .char_indices()
            .take_while(|(i, _)| *i != span.start)
            .count();

        let end_line_s = &self.session.src[end_line_pos..span.end];
        let end_col = end_line_s
            .char_indices()
            .take_while(|(i, _)| *i != span.end)
            .count();

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_token() {
        let s = String::from("@");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert!(tok.is_err());
        let err = tok.unwrap_err();
        match tok {
            Err(Error {
                kind: ErrorKind::UnknownChar,
                location:
                    Location {
                        start_pos: 0,
                        start_line: 1,
                        start_column: 1,
                        end_pos: 2,
                        end_line: 1,
                        end_column: 2,
                    },
            }) => (),
            other => panic!(format!("Should be an error: {:?}", other)),
        }

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);
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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);
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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);
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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
    fn hex_number_missing_digits() -> Result<(), String> {
        let s = String::from("0x");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        match tok {
            Err(Error {
                kind: ErrorKind::MissingDigitsInNumber,
                location:
                    Location {
                        start_line: 1,
                        start_column: 1,
                        end_line: 1,
                        end_column: 3,
                        ..
                    },
            }) => Ok(()),
            other => Err(format!("Should be an error: {:?}", other)),
        }
    }

    #[test]
    fn hex_number() {
        let s = String::from("0XdeadBEEF");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

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
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 6);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 7);
    }

    #[test]
    fn windows_newline() {
        let s = String::from("@\r\r\n1");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::At);
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 1);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Whitespace);
        assert_eq!(tok.span.start, 1);
        assert_eq!(tok.span.end, 2);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Newline);
        assert_eq!(tok.span.start, 2);
        assert_eq!(tok.span.end, 4);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Int(1));
        assert_eq!(tok.span.start, 4);
        assert_eq!(tok.span.end, 5);
    }

    #[test]
    fn single_char_literal() {
        let s = String::from("'a'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('a'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn invalid_char_literal() {
        let s = String::from("'ab'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidCharLiteral);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 4);
    }

    #[test]
    fn invalid_char_literal_2() {
        let s = String::from("'\\ab'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidCharLiteral);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 5);
    }

    #[test]
    fn invalid_char_literal_3() {
        let s = String::from("'\\ua'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidCharLiteral);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 5);
    }

    #[test]
    fn invalid_char_literal_4() {
        let s = String::from("'\\uab'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidCharLiteral);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 6);
    }

    #[test]
    fn invalid_char_literal_5() {
        let s = String::from("'\\uabc'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        assert_eq!(tok.kind, ErrorKind::InvalidCharLiteral);
        assert_eq!(tok.location.start_pos, 0);
        assert_eq!(tok.location.end_pos, 7);
    }

    #[test]
    fn invalid_char_literal_6() -> Result<(), String> {
        let s = String::from("'\\Uabcd'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_err(), true);
        let tok = tok.as_ref().unwrap_err();
        match tok {
            Err(Error {
                kind: ErrorKind::InvalidCharLiteral,
                location:
                    Location {
                        start_pos: 0,
                        end_pos: 8,
                        ..
                    },
            }) => Ok(()),
            other => Err(format!("Should be an error: {:?}", other)),
        }
    }

    #[test]
    fn invalid_char_literal_7() -> Result<(), String> {
        let s = String::from("'\\ud800'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        match tok {
            Err(Error {
                kind: ErrorKind::InvalidCharLiteral,
                location:
                    Location {
                        start_pos: 0,
                        end_pos: 8,
                        ..
                    },
            }) => Ok(()),
            other => Err(format!("Should be an error: {:?}", other)),
        }
    }

    #[test]
    fn invalid_char_literal_8() -> Result<(), String> {
        let s = String::from("'\\uwxyz'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        match tok {
            Err(Error {
                kind: ErrorKind::InvalidCharLiteral,
                location:
                    Location {
                        start_pos: 0,
                        end_pos: 8,
                        ..
                    },
            }) => Ok(()),
            other => Err(format!("Should be an error: {:?}", other)),
        }
    }

    #[test]
    fn unicode_char_literal() {
        let s = String::from("'\\uabcd'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\u{abcd}'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 8);
    }

    #[test]
    fn unicode_char_literal_unassigned_unicode() {
        let s = String::from("'\\u2fe0'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\u{2fe0}'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 8);
    }

    #[test]
    fn escaped_sequence_char_literal_r() {
        let s = String::from(r#"'\r'"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\r'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn escaped_sequence_char_literal_n() {
        let s = String::from(r#"'\n'"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\n'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn escaped_sequence_char_literal_t() {
        let s = String::from(r#"'\t'"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\t'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn escaped_sequence_char_literal_b() {
        let s = String::from(r#"'\b'"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\u{0008}'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn escaped_sequence_char_literal_single_quote() {
        let s = String::from(r#"'\''"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\''));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn escaped_sequence_char_literal_double_quote() {
        let s = String::from(r#"'"'"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('"'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn escaped_sequence_char_literal_dollar() {
        let s = String::from("'$'");
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('$'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 3);
    }

    #[test]
    fn escaped_sequence_char_literal_backslash() {
        let s = String::from(r#"'\\'"#);
        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Char('\\'));
        assert_eq!(tok.span.start, 0);
        assert_eq!(tok.span.end, 4);
    }

    #[test]
    fn keyword_true() {
        let s = String::from("true");

        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Boolean(true));
        assert_eq!(&session.src[tok.span.start..tok.span.end], "true");
    }

    #[test]
    fn keyword_false() {
        let s = String::from("false");

        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Boolean(false));
        assert_eq!(&session.src[tok.span.start..tok.span.end], "false");
    }

    #[test]
    fn identifier_ascii() {
        let s = String::from("_foo_bar_");

        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(&session.src[tok.span.start..tok.span.end], "_foo_bar_");
    }

    #[test]
    fn identifier_unicode() {
        // `ƍ` is of category Ll (letter lowercase)
        // `ᴽ` is of category Lm (letter modifier)
        // `א` is of categoy Lo (other letter)
        // `ᾯ` is of category Lt (letter titlecase)
        // `Ʊ` is of category Lu (letter uppercase)
        // `ᛮ` is of category Nl (letter number)
        let s = String::from("ᛮƍᴽאᾯƱ");

        let session = Session::new(&s, None);
        let mut lexer = Lexer::new(&session);

        let tok = lexer.next_token();
        assert_eq!(tok.as_ref().is_ok(), true);
        let tok = tok.as_ref().unwrap();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(&session.src[tok.span.start..tok.span.end], "ᛮƍᴽאᾯƱ");
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
}
