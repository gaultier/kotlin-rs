use crate::error::*;
// use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::Session;

pub(crate) struct JvmEmitter<'a> {
    session: &'a Session<'a>,
    _types: &'a Types,
}

impl<'a> JvmEmitter<'a> {
    pub(crate) fn new(session: &'a Session, _types: &'a Types) -> JvmEmitter<'a> {
        JvmEmitter { session, _types }
    }

    pub(crate) fn statements<W: std::io::Write>(
        &self,
        _block: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.magic_number(w)?;
        Ok(())
    }

    fn magic_number<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&[0xca, 0xfe, 0xba, 0xbe])?;
        Ok(())
    }
}
