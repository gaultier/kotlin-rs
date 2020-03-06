// use crate::error::*;
// use crate::gen_js::gen_js_stmts;
// use crate::parse::Parser;
// use crate::type_check::type_check_stmts;
// use std::io;

// pub fn compile<W: io::Write>(src: &str, w: &mut W) -> Result<(), Error> {
//     let mut parser = Parser::new(&src);
//     let mut stmts = parser.parse()?;
//     type_check_stmts(&mut stmts, &src)?;
//     gen_js_stmts(&stmts, &src, w)
// }
