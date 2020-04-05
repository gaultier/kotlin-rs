use crate::error::Location;
use log::debug;
use std::path::PathBuf;

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct Session<'a> {
    src: &'a str,
    file: Option<&'a PathBuf>,
    lines: Vec<usize>,
}

impl<'a> Session<'a> {
    pub fn new(src: &'a str, file: Option<&'a PathBuf>) -> Session<'a> {
        Session {
            src,
            file,
            lines: vec![0],
        }
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

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn from_spans(left: &Span, right: &Span) -> Span {
        Span {
            start: left.start,
            end: right.end,
        }
    }

    pub fn len(&self) -> usize {
        debug_assert!(self.end >= self.start);
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
