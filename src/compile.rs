use crate::error::*;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::session::Session;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use std::io;

pub fn compile<W: io::Write>(src: &str, w: &mut W) -> Result<(), Error> {
    let mut session = Session::from_stdin(src);
    let mut lexer = Lexer::new(&mut session);
    let mut parser = Parser::new(&session);
    let mut stmts = parser.parse()?;

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
        let contents = String::from("// hello\n 0xab + 24; ");
        let buf = Vec::new();
        let mut writer = io::BufWriter::new(buf);

        assert!(compile(contents, writer.by_ref()).is_ok());
        let s = String::from_utf8(writer.into_inner().unwrap()).unwrap();
        assert_eq!(&s, "(+ 171 24)\n");
    }

    #[test]
    fn multiplication() {
        let contents = String::from("// hello\n 0xab * 24; ");
        let buf = Vec::new();
        let mut writer = io::BufWriter::new(buf);

        assert!(compile(contents, writer.by_ref()).is_ok());
        let s = String::from_utf8(writer.into_inner().unwrap()).unwrap();
        assert_eq!(&s, "(* 171 24)\n");
    }
}
