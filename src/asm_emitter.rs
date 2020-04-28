use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use crate::session::Session;
use log::debug;

#[derive(Debug)]
pub(crate) struct AsmEmitter<'a> {
    pub(crate) session: &'a Session<'a>,
    pub(crate) types: &'a Types,
    pub(crate) resolution: &'a Resolution,
}

impl<'a> AsmEmitter<'a> {
    pub(crate) fn new(
        session: &'a Session,
        types: &'a Types,
        resolution: &'a Resolution,
    ) -> AsmEmitter<'a> {
        AsmEmitter {
            session,
            types,
            resolution,
        }
    }

    fn fn_prolog<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            push rbp
            mov rbp, rsp
            "##
            .as_bytes(),
        )?;
        Ok(())
    }

    fn fn_epilog<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"pop rbp
            ret
            "##
            .as_bytes(),
        )?;
        Ok(())
    }

    fn fn_main<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            global _main
            _main:"##
                .as_bytes(),
        )?;
        Ok(())
    }

    fn epilog<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            BITS 64
            CPU X64
            DEFAULT REL
            "##
            .as_bytes(),
        )?;
        Ok(())
    }
}
