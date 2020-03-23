use crate::error::Location;
use log::debug;
use std::path::PathBuf;

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn len(&self) -> usize {
        debug_assert!(self.end >= self.start);
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Session<'a> {
    file: Option<PathBuf>,
    // Index of each line, 0 based
    // FIXME: should be private
    pub(crate) lines: Vec<usize>,
    pub(crate) src: &'a str,
}

impl<'a> Session<'a> {
    pub(crate) fn from_stdin(src: &'a str) -> Session {
        Session {
            file: None,
            lines: Vec::new(),
            src,
        }
    }

    pub(crate) fn from_file(src: &'a str, file: PathBuf) -> Session {
        Session {
            file: Some(file),
            lines: Vec::new(),
            src,
        }
    }

    pub(crate) fn newline(&mut self, pos: usize) {
        self.lines.push(pos);
    }

    pub(crate) fn span_location(&self, span: &Span) -> Location {
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
        let start_col = start_line_s
            .char_indices()
            .take_while(|(i, _)| *i != span.start)
            .count();

        let end_line_s = &self.src[end_line_pos..span.end];
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
