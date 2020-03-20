use crate::error::*;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use std::io;

pub fn compile<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut stmts = parser.parse()?;

    let type_checker = TypeChecker::new(&lexer);
    type_checker.type_check_stmts(&mut stmts)?;

    let js_emitter = SexpEmitter::new(&lexer);
    js_emitter.stmts(&stmts, w)
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
        assert_eq!(&s, "171+24;\n");
    }

    #[test]
    fn multiplication() {
        let contents = String::from("// hello\n 0xab * 24; ");
        let buf = Vec::new();
        let mut writer = io::BufWriter::new(buf);

        assert!(compile(contents, writer.by_ref()).is_ok());
        let s = String::from_utf8(writer.into_inner().unwrap()).unwrap();
        assert_eq!(&s, "171*24;\n");
    }
}
