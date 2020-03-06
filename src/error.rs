// use crate::parse::Type;
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
    // IncompatibleTypes(Type, Type),
    EmitError(String),
    UnterminatedStatement,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::UnknownChar => write!(f, "Unknown character"),
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected character: got {}", c),
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
            ErrorKind::InvalidUnicodeLiteral(_s) => write!(f, "Invalid unicode literal"),
            ErrorKind::ExpectedPrimary => write!(f, "Expected primary"),
            // ErrorKind::IncompatibleTypes(left, right) => {
            //     write!(f, "Incompatible types: {} and {}", left, right)
            // }
            ErrorKind::EmitError(err) => write!(f, "Emit error: {}", err),
            ErrorKind::UnterminatedStatement => write!(f, "Unterminated statement"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub location: Location,
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct OwnedError<'a> {
//     error: Error,
//     pub src: &'a str,
// }

// impl OwnedError<'_> {
//     pub fn eprint(&self) {
//         let mut stderr = StandardStream::stderr(ColorChoice::Always);

//         eprintln!(
//             "{}:{}:{}",
//             self.error.location.start_line, self.error.location.start_column, self.error.kind,
//         );

//         let line = format!("{}", self.error.location.start_line);
//         let blank = " ".repeat(line.len());

//         stderr
//             .set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
//             .unwrap();

//         eprintln!("{} |", blank);
//         eprint!("{} |", self.error.location.start_line);
//         stderr
//             .set_color(ColorSpec::new().set_fg(Some(Color::White)))
//             .unwrap();

//         let last_newline_index = self.src[0..self.error.location.start_pos]
//             .rfind('\n')
//             .map(|i| i + 1)
//             .unwrap_or(0);

//         let next_newline_index = &self.src[self.error.location.end_pos..]
//             .find('\n')
//             .unwrap_or(self.src.len())
//             + self.error.location.end_pos;
//         eprintln!(" {}", &self.src[last_newline_index..next_newline_index]);

//         stderr
//             .set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
//             .unwrap();

//         eprint!(
//             "{} |{}",
//             blank,
//             " ".repeat(self.error.location.start_column)
//         );

//         stderr
//             .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
//             .unwrap();

//         let src_err = &self.src[self.error.location.start_pos..self.error.location.end_pos];
//         eprint!(
//             "{} {}",
//             "^".repeat(src_err.chars().count()),
//             self.error.kind
//         );

//         stderr
//             .set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
//             .unwrap();
//         eprintln!();

//         eprintln!("{} |", blank);
//         stderr
//             .set_color(ColorSpec::new().set_fg(Some(Color::White)))
//             .unwrap();
//     }
// }

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

    //     pub fn to_owned(self, src: &str) -> OwnedError {
    //         OwnedError { error: self, src }
    //     }
}
