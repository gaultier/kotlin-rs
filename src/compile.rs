use crate::error::*;
use crate::gen_js::JsEmitter;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::type_check::TypeChecker;
use std::io;

pub fn compile<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut stmts = parser.parse()?;

    let type_checker = TypeChecker::new(&lexer);
    type_checker.type_check_stmts(&mut stmts)?;

    let js_emitter = JsEmitter::new(&lexer);
    js_emitter.stmts(&stmts, w)
}
