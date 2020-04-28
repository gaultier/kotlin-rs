use crate::error::*;
// use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use crate::session::Session;
// use log::debug;

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

    fn prolog<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            BITS 64 ; 64 bits
            CPU X64 ; target the x86_64 family of CPUs
            DEFAULT REL ; relative addressing mode

            extern _printf ; might be unused but that is ok
            "##
            .as_bytes(),
        )?;
        Ok(())
    }

    fn data_section<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            section .data
                int_fmt_string: db "%d", 0
                string_fmt_string: db "%s", 0
                char_fmt_string: db "%c", 0
                hello: db "hello, world!", 0
            "##
            .as_bytes(),
        )?;
        Ok(())
    }

    fn text_section<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            section .text
            "##
            .as_bytes(),
        )?;
        Ok(())
    }

    pub(crate) fn main<W: std::io::Write>(
        &self,
        _statements: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.prolog(w)?;
        self.data_section(w)?;

        self.text_section(w)?;
        self.fn_main(w)?;
        self.fn_prolog(w)?;

        // FIXME
        w.write_all(
            &r##"
            xor rax, rax
            lea rdi, [string_fmt_string]
            lea rsi, [hello]
            call _printf

            xor rax, rax
            "##
            .as_bytes(),
        )?;
        self.fn_epilog(w)?;

        w.flush()?;

        Ok(())
    }
}
