use crate::error::*;
use crate::gen_js::gen_js;
use crate::parse::*;
use crate::type_check::*;
use std::io;

pub fn compile<W: io::Write>(src: &str, w: &mut W) -> Result<(), Error> {
    let mut parser = Parser::new(&src);
    let ast = parser.parse()?;
    type_check(&ast, &src)?;
    gen_js(&ast, &src, w)
}
