use crate::type_check::Type;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Location {
    pub start_pos: usize,
    pub start_line: usize,
    pub start_column: usize,
    pub end_pos: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    UnknownChar,
    UnexpectedChar(char),
    ShebangNotOnFirstLine,
    NewlineInString,
    TrailingUnderscoreInNumber,
    LeadingZeroInNumber,
    MissingDigitsInBinaryNumber,
    MissingDigitsInHexNumber,
    TrailingDotInNumber,
    MissingExponentInNumber,
    UnknownEscapeSequence(Option<char>),
    IncompleteUnicodeLiteral,
    InvalidUnicodeLiteral(String),
    ExpectedPrimary,
    IncompatibleTypes(Type, Type),
    EmitError(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::UnknownChar => write!(f, "Unknown char"),
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected char: got {}", c),
            ErrorKind::ShebangNotOnFirstLine => write!(f, "Shebang not on the first line"),
            ErrorKind::NewlineInString => write!(f, "Newline in double quoted string"),
            ErrorKind::TrailingUnderscoreInNumber => write!(f, "Trailing underscore in number"),
            ErrorKind::LeadingZeroInNumber => write!(f, "Leading zero in number"),
            ErrorKind::MissingDigitsInBinaryNumber => write!(f, "Missing digits in binary number"),
            ErrorKind::MissingDigitsInHexNumber => write!(f, "Missing digits in hex number"),
            ErrorKind::TrailingDotInNumber => write!(f, "Trailing dot in number"),
            ErrorKind::MissingExponentInNumber => write!(f, "Missing exponent in number"),
            ErrorKind::UnknownEscapeSequence(esc) => {
                write!(f, "Unknown escape sequence: {:?}", esc)
            }
            ErrorKind::IncompleteUnicodeLiteral => write!(f, "Incomplete unicode literal"),
            ErrorKind::InvalidUnicodeLiteral(_s) => write!(f, "Invalid uncide literal"),
            ErrorKind::ExpectedPrimary => write!(f, "Expected primary"),
            ErrorKind::IncompatibleTypes(left, right) => {
                write!(f, "Incompatible types: {} and {}", left, right)
            }
            ErrorKind::EmitError(err) => write!(f, "Emit error: {}", err),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    location: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OwnedError<'a> {
    error: Error,
    pub src: &'a str,
}

impl<'a> fmt::Display for OwnedError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt = format!(
            "{}:{}:{}:    {}",
            self.error.location.start_line,
            self.error.location.start_column,
            self.error.kind,
            &self.src[self.error.location.start_pos..self.error.location.start_pos] // FIXME: show full source code line
        );
        write!(
            f,
            "{}{}\n",
            fmt,
            &self.src[self.error.location.start_pos..self.error.location.end_pos]
        )?;
        for _ in 0..fmt.len() {
            write!(f, " ")?;
        }
        for _ in self.error.location.start_pos..self.error.location.end_pos {
            write!(f, "^")?;
        }
        Ok(())
    }
}

impl Error {
    pub fn new(
        kind: ErrorKind,
        start_pos: usize,
        start_line: usize,
        start_column: usize,
        end_pos: usize,
        end_line: usize,
        end_column: usize,
    ) -> Error {
        Error {
            kind,
            location: Location {
                end_pos,
                end_line,
                end_column,
                start_pos,
                start_line,
                start_column,
            },
        }
    }

    pub fn to_owned<'a>(self, src: &'a str) -> OwnedError<'a> {
        OwnedError { error: self, src }
    }
}
