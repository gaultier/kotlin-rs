use crate::asm_constants::Constants;
use crate::asm_registers::{Register, Registers};
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
    constants: Constants,
    buffer: String,
    int_fmt_string_label: String,
    string_fmt_string_label: String,
    registers: Registers,
}

impl<'a> AsmEmitter<'a> {
    pub(crate) fn new(
        session: &'a Session,
        types: &'a Types,
        resolution: &'a Resolution,
    ) -> AsmEmitter<'a> {
        let mut constants = Constants::new();
        let int_fmt_string_label = constants.find_or_create_string(String::from("%d"));
        let string_fmt_string_label = constants.find_or_create_string(String::from("%s"));

        AsmEmitter {
            session,
            types,
            resolution,
            buffer: String::new(),
            constants,
            int_fmt_string_label,
            string_fmt_string_label,
            registers: Registers::new(),
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

    fn prolog<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
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

    fn data_section<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
        w.write_all(&" section .data\n".as_bytes())?;
        for (constant, label) in self.constants.iter() {
            w.write_all(
                &format!("{}: db \"{}\", 0 ; null terminated\n", label, constant).as_bytes(),
            )?;
        }
        Ok(())
    }

    fn text_section<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"
            section .text
            "##
            .as_bytes(),
        )?;
        Ok(())
    }

    fn zero_register(&mut self, register: Register) {
        self.registers.free(register);
        self.buffer
            .push_str(&format!("xor {}, {}", register, register));
    }

    fn call_function(&mut self, fn_name: &str) {
        self.buffer.push_str(&"call ");
        self.buffer.push_str(fn_name);
    }

    pub(crate) fn main<W: std::io::Write>(
        &mut self,
        statements: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.fn_main();
        self.fn_prolog();
        self.statement(statements);
        self.fn_epilog();

        self.prolog(w)?;
        self.data_section(w)?;
        self.text_section(w)?;

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
        let t = self.types.get(&expr.id()).unwrap();
        let (op, fmt_string_label) = match t {
            Type::Int => ("mov", &self.int_fmt_string_label),
            Type::TString => ("lea", &self.string_fmt_string_label),
            _ => todo!(),
        };

        self.buffer.push_str(&format!(
            r##"
            lea rdi, [{}]
            {} rsi, "##,
            fmt_string_label, op
        ));
        self.expr(expr);

        self.call_function("_printf");

        self.zero_register(Register::Rax);
    }

    fn literal(&mut self, token: &Token) {
        match token.kind {
            TokenKind::Int(n) => self.buffer.push_str(&format!("{}", n)),
            TokenKind::Long(n) => self.buffer.push_str(&format!("{}", n)),
            TokenKind::Boolean(true) => self.buffer.push_str("1"),
            TokenKind::Boolean(false) => self.buffer.push_str("0"),
            TokenKind::Char(c) => self.buffer.push_str(&format!("'{}'", c)),
            TokenKind::TString => {
                let s = String::from(&self.session.src[token.span.start + 1..token.span.end - 1]);
                let label = self.constants.find_or_create_string(s);

                self.buffer.push_str(&format!("[{}]", &label));
            }
            _ => todo!(),
        }
    }
}
