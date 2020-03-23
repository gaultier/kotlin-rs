use crate::error::*;
use crate::lex::Lexer;
use crate::parse::{Parser, Statements};
use crate::session::Session;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use log::debug;
use std::io;

fn parse(session: &mut Session) -> Result<Statements, Error> {
    let mut lexer = Lexer::new(&mut session);
    let mut parser = Parser::new(&mut lexer);
    Ok(parser.parse()?)
}

pub fn compile<W: io::Write>(src: &str, w: &mut W) -> Result<(), Error> {
    let mut session = Session::from_stdin(src);
    let mut stmts = parse(&mut session)?;
    debug!("lines={:?}", &session.lines);
    let type_checker = TypeChecker::new(&session);
    type_checker.statements(&mut stmts)?;

    let emitter = SexpEmitter::new(&session);
    emitter.statements(&stmts, w)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn addition() {
        let src = "// hello\n 0xab + 24; ";
        let buf = Vec::new();
        let mut writer = io::BufWriter::new(buf);

        assert!(compile(&src, writer.by_ref()).is_ok());
        let s = String::from_utf8(writer.into_inner().unwrap()).unwrap();
        assert_eq!(&s, "(+ 171 24)\n");
    }

    #[test]
    fn multiplication() {
        let src = "// hello\n 0xab * 24; ";
        let buf = Vec::new();
        let mut writer = io::BufWriter::new(buf);

        assert!(compile(&src, writer.by_ref()).is_ok());
        let s = String::from_utf8(writer.into_inner().unwrap()).unwrap();
        assert_eq!(&s, "(* 171 24)\n");
    }
}
