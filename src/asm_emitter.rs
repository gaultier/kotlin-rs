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
            &r##"
            push rbp
            mov rbp, rsp
            "##,
        );
    }

    fn fn_epilog(&mut self) {
        self.buffer.push_str(
            &r##"pop rbp
            ret
            "##,
        );
    }

    fn fn_main(&mut self) {
        self.buffer.push_str(
            &r##"
            global _main
            _main:"##,
        );
    }

    fn prolog(&mut self) {
        self.buffer.push_str(
            &r##"
            BITS 64 ; 64 bits
            CPU X64 ; target the x86_64 family of CPUs
            DEFAULT REL ; relative addressing mode

            extern _printf ; might be unused but that is ok
            "##,
        );
    }

    fn data_section(&mut self) {
        self.buffer.push_str(
            &r##"
            section .data
                int_fmt_string: db "%d", 0
                string_fmt_string: db "%s", 0
                char_fmt_string: db "%c", 0
                hello: db "hello, world!", 0
            "##,
        );
    }

    fn text_section(&mut self) {
        self.buffer.push_str(
            &r##"
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

        self.buffer.push_str(
            &r##"
            xor rax, rax
            lea rdi, [int_fmt_string] ; FIXME
            mov rsi,"##,
        );
        self.statement(statements);

        self.buffer.push_str(
            &r##"
            call _printf

            xor rax, rax
            "##,
        );

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
            _ => todo!(),
        }
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
