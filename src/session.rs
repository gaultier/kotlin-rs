use std::path::PathBuf;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Session<'a> {
    file: Option<PathBuf>,
    // Index of each line, 0 based
    lines: Vec<usize>,
    pub(crate) src: &'a str,
}

impl<'a> Session<'a> {
    fn from_stdin(src: &'a str) -> Session {
        Session {
            file: None,
            lines: Vec::new(),
            src,
        }
    }

    fn from_file(src: &'a str, file: PathBuf) -> Session {
        Session {
            file: Some(file),
            lines: Vec::new(),
            src,
        }
    }

    fn newline(&mut self, pos: usize) {
        self.lines.push(pos);
    }
}
