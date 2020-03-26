use crate::parse::Type;
use log::debug;
use std::fmt;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

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
    UnterminatedString,
    TrailingUnderscoreInNumber,
    LeadingZeroInNumber,
    OctalNumber,
    MissingDigitsInNumber,
    TrailingDotInNumber,
    MissingExponentInNumber,
    UnknownEscapeSequence(Option<char>),
    InvalidCharLiteral,
    ExpectedPrimary,
    IncompatibleTypes(Type, Type),
    EmitError(String),
    InvalidNumberSuffix(String),
    UnterminatedStatement,
    UnterminatedChar,
    InvalidRange(Type),
    UnknownIdentifier(String),
    ExpectedToken,
    CannotReassignVal(String),
    NotACallable(Type),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::UnknownChar => write!(f, "Unknown character"),
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected character: got {}", c),
            ErrorKind::ShebangNotOnFirstLine => write!(f, "Shebang not on the first line"),
            ErrorKind::UnterminatedString => write!(f, "Unterminated string"),
            ErrorKind::TrailingUnderscoreInNumber => write!(f, "Trailing underscore in number"),
            ErrorKind::LeadingZeroInNumber => write!(f, "Leading zero in number"),
            ErrorKind::OctalNumber => write!(f, "Octal number"),
            ErrorKind::MissingDigitsInNumber => write!(f, "Missing digits in number"),
            ErrorKind::TrailingDotInNumber => write!(f, "Trailing dot in number"),
            ErrorKind::MissingExponentInNumber => write!(f, "Missing exponent in number"),
            ErrorKind::UnknownEscapeSequence(esc) => {
                write!(f, "Unknown escape sequence: {:?}", esc)
            }
            ErrorKind::InvalidCharLiteral => write!(f, "Invalid char literal"),
            ErrorKind::ExpectedPrimary => write!(f, "Expected primary"),
            ErrorKind::IncompatibleTypes(left, right) => {
                write!(f, "Incompatible types: {} and {}", left, right)
            }
            ErrorKind::EmitError(err) => write!(f, "Emit error: {}", err),
            ErrorKind::UnterminatedStatement => write!(f, "Unterminated statement"),
            ErrorKind::UnterminatedChar => write!(f, "Unterminated char"),
            ErrorKind::InvalidNumberSuffix(suffix) => {
                write!(f, "Invalid number suffix `{}`", suffix)
            }
            ErrorKind::ExpectedToken => write!(f, "Expected token"),
            ErrorKind::InvalidRange(t) => write!(f, "Invalid range of type {}", t),
            ErrorKind::UnknownIdentifier(identifier) => {
                write!(f, "Unknown identifier {}", identifier)
            }
            ErrorKind::CannotReassignVal(identifier) => write!(
                f,
                "Cannot reassign variable {} declared with `val`",
                identifier
            ),
            ErrorKind::NotACallable(t) => write!(f, "`{}` cannot be called as a function", t),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub location: Location,
}

impl Error {
    pub fn new(kind: ErrorKind, location: Location) -> Error {
        Error { kind, location }
    }

    pub fn eprint(&self, src: &str) {
        debug!(
            "error start_pos={} start_line={} start_col={} end_pos={} end_line={} end_col={}",
            self.location.start_pos,
            self.location.start_line,
            self.location.start_column,
            self.location.end_pos,
            self.location.end_line,
            self.location.end_column
        );
        let mut stderr = StandardStream::stderr(ColorChoice::Always);

        eprintln!(
            "{}:{}:{}",
            self.location.start_line, self.location.start_column, self.kind,
        );

        let line = format!("{}", self.location.start_line);
        let blank = " ".repeat(line.len());

        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
            .unwrap();

        eprintln!("{} |", blank);
        eprint!("{} |", self.location.start_line);
        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::White)))
            .unwrap();

        let last_newline_index = src[0..self.location.start_pos]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);

        let next_newline_index = src[self.location.end_pos..]
            .find('\n')
            .unwrap_or(src.len() - self.location.end_pos)
            + self.location.end_pos;
        eprintln!(" {}", &src[last_newline_index..next_newline_index]);

        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
            .unwrap();

        eprint!("{} |{}", blank, " ".repeat(self.location.start_column));

        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
            .unwrap();

        let src_err = &src[self.location.start_pos..self.location.end_pos];
        eprint!(
            "{} {}",
            "^".repeat(std::cmp::max(1, src_err.chars().count())),
            self.kind
        );

        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
            .unwrap();
        eprintln!();

        eprintln!("{} |", blank);
        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::White)))
            .unwrap();
    }
}
