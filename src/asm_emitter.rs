use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use crate::session::Session;
// use log::debug;

#[derive(Debug)]
pub(crate) struct AsmEmitter<'a> {
    session: &'a Session<'a>,
    types: &'a Types,
    resolution: &'a Resolution,
    buffer: String,
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
            buffer: String::new(),
        }
    }

    fn fn_prolog(&mut self) {
        self.buffer.push_str(
            r##"
            ; prolog
            push rbp
            mov rbp, rsp
            "##,
        );
    }

    fn fn_epilog(&mut self) {
        self.buffer.push_str(
            r##"
            ; epilog
            pop rbp
            ret
            "##,
        );
    }

    fn fn_main(&mut self) {
        self.buffer.push_str(
            &r##"
            ; entrypoint
            global _main
            _main:"##,
        );
    }

    fn prolog(&mut self) {
        self.buffer.push_str(
            r##"
            BITS 64 ; 64 bits
            CPU X64 ; target the x86_64 family of CPUs
            DEFAULT REL ; relative addressing mode

            extern _printf ; might be unused but that is ok
            "##,
        );
    }

    fn data_section(&mut self) {
        self.buffer.push_str(
            r##"
            section .data
                int_fmt_string: db "%d", 10, 0
                string_fmt_string: db "%s", 10, 0
                char_fmt_string: db "%c", 10, 0
            "##,
        );
    }

    fn text_section(&mut self) {
        self.buffer.push_str(
            r##"
            section .text
            "##,
        );
    }

    pub(crate) fn main<W: std::io::Write>(
        &mut self,
        statements: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.prolog();
        self.data_section();

        self.text_section();
        self.fn_main();
        self.fn_prolog();

        self.statement(statements);

        self.fn_epilog();

        w.write_all(self.buffer.as_bytes())?;
        w.flush()?;

        Ok(())
    }

    fn statement(&mut self, statement: &AstNodeStmt) {
        match statement {
            AstNodeStmt::Expr(expr) => self.expr(expr),
            AstNodeStmt::Block { body, .. } => {
                for stmt in body {
                    self.statement(stmt);
                }
            }
            _ => todo!(),
        }
    }

    fn expr(&mut self, expr: &AstNodeExpr) {
        match expr {
            AstNodeExpr::Literal(tok, _) => self.literal(tok),
            AstNodeExpr::Println(expr, _) => self.println(expr),
            _ => todo!(),
        }
    }

    fn println(&mut self, expr: &AstNodeExpr) {
        self.buffer.push_str(
            r##"
            lea rdi, [int_fmt_string] ; FIXME: hardcoded
            mov rsi, "##,
        );
        self.expr(expr);
        self.buffer.push_str(
            &r##"
            call _printf
            xor rax, rax
            "##,
        );
    }

    fn literal(&mut self, token: &Token) {
        match token.kind {
            TokenKind::Int(n) => self.buffer.push_str(&format!("{}", n)),
            TokenKind::Long(n) => self.buffer.push_str(&format!("{}", n)),
            TokenKind::Boolean(true) => self.buffer.push_str("1"),
            TokenKind::Boolean(false) => self.buffer.push_str("0"),
            TokenKind::Char(c) => self.buffer.push_str(&format!("'{}'", c)),
            _ => todo!(),
        }
    }
}
