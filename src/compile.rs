use crate::error::*;
use crate::gen_js::gen_js_stmts;
use crate::parse::Parser;
use crate::type_check::type_check_stmts;
use std::io;

pub fn compile<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let cpy = src.clone();
    let mut parser = Parser::new(src);
    let mut stmts = parser.parse()?;

    type_check_stmts(&mut stmts, &cpy)?;
    gen_js_stmts(&stmts, &cpy, w)
}
