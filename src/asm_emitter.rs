use crate::asm_constants::Constants;
use crate::asm_registers::{
    Register, Registers, REGISTER_ARG_1, REGISTER_ARG_2, REGISTER_RETURN_VALUE,
};
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
    registers: Registers,
}

const PRINTF_FMT_STRING: &str = "%s";
const PRINTF_FMT_INT: &str = "%d";
const PRINTF_FMT_LONG: &str = "%ld";

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
            constants: Constants::new(),
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
            r##"
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
        w.write_all(b" section .data\n")?;
        for (constant, label) in self.constants.iter() {
            w.write_all(
                &format!("{}: db \"{}\", 0 ; null terminated\n", label, constant).as_bytes(),
            )?;
        }
        Ok(())
    }

    fn newline(&mut self) {
        self.buffer.push_str("\n");
    }

    fn text_section<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
        w.write_all(b"section .text\n")?;
        Ok(())
    }

    fn zero_register(&mut self, register: Register) {
        self.registers.free(register);
        self.buffer
            .push_str(&format!("xor {}, {}", register, register));
        self.newline();
    }

    fn call_function(&mut self, fn_name: &str, arg_count: usize) {
        assert_eq!(arg_count, 2);

        self.buffer.push_str("call ");
        self.buffer.push_str(fn_name);
        self.newline();

        self.zero_register(REGISTER_RETURN_VALUE);
        self.registers.free(REGISTER_ARG_1);
        self.registers.free(REGISTER_ARG_2);
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
            AstNodeStmt::Expr(expr) => {
                let register = self.registers.allocate().unwrap();
                self.expr(expr, register);
            }
            AstNodeStmt::Block { body, .. } => {
                for stmt in body {
                    self.statement(stmt);
                }
            }
            _ => todo!(),
        }
    }

    fn expr(&mut self, expr: &AstNodeExpr, register: Register) {
        match expr {
            AstNodeExpr::Literal(tok, _) => self.literal(tok, register),
            AstNodeExpr::Println(expr, _) => self.println(expr, register),
            AstNodeExpr::Unary { .. } => self.unary(expr, register),
            AstNodeExpr::Binary { .. } => self.binary(expr, register),
            AstNodeExpr::Grouping(expr, _) => self.expr(expr, register),
            _ => todo!(),
        }
    }

    // `rax`: dividend, register or next free register: dividend, `rdx`: remainder
    fn div(&mut self, right: &AstNodeExpr, register: Register) {
        // The dividend must be in `rax`, so we copy `register` in rax
        if register != Register::Rax {
            if !self.registers.is_free(Register::Rax) {
                dbg!(&self.registers);
                todo!("Re-arrange registers");
            }

            self.registers.reserve(Register::Rax);
            self.assign_register(Register::Rax);
            self.buffer.push_str(register.as_str());
            self.newline();
        }
        // Required to store the sign in rdx:rax, otherwise we get a FPE
        self.buffer.push_str("cqo");
        self.newline();

        // `div` will overwrite `rdx`.
        if !self.registers.is_free(Register::Rdx) {
            dbg!(&self.registers);
            todo!("Re-arrange registers");
        }

        if register != Register::Rcx {
            if !self.registers.is_free(Register::Rcx) {
                todo!("Re-arrange registers");
            }

            self.registers.reserve(Register::Rcx);
            self.expr(right, Register::Rcx);
            self.newline();
        }

        self.buffer.push_str(&format!("idiv {}", Register::Rcx));
        // `div` destroys the content of `rcx`
        self.registers.free(Register::Rcx);
        self.newline();
    }

    fn binary(&mut self, expr: &AstNodeExpr, register: Register) {
        match expr {
            AstNodeExpr::Binary {
                left,
                op,
                right,
                id,
                ..
            } => {
                let t = self.types.get(id).unwrap();
                let left_t = self.types.get(&left.id()).unwrap();
                let right_t = self.types.get(&right.id()).unwrap();
                if left_t != right_t {
                    unimplemented!("Conversions")
                }

                self.expr(left, register);
                self.newline();

                match (op.kind, t) {
                    (TokenKind::EqualEqual, Type::Boolean) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("cmp {}, {}", register, intermediate_register));
                        self.newline();
                        self.registers.free(intermediate_register);

                        self.buffer
                            .push_str(&format!("sete {}", register.as_byte_str()));
                    }
                    (TokenKind::BangEqual, Type::Boolean) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("cmp {}, {}", register, intermediate_register));
                        self.newline();
                        self.registers.free(intermediate_register);

                        self.buffer
                            .push_str(&format!("setne {}", register.as_byte_str()));
                    }
                    (TokenKind::Plus, Type::Int) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("add {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Minus, Type::Int) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("sub {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Star, Type::Int) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("imul {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Slash, Type::Int) => {
                        self.div(right, register);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rax {
                            self.assign_register(register);
                            self.buffer.push_str(Register::Rax.as_str());
                            self.newline();
                            self.zero_register(Register::Rax);

                            self.registers.free(Register::Rax);
                        }
                    }
                    (TokenKind::Percent, Type::Int) => {
                        self.div(right, register);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rdx {
                            self.assign_register(register);
                            self.buffer.push_str(Register::Rdx.as_str());
                            self.newline();
                            self.zero_register(Register::Rdx);

                            self.registers.free(Register::Rdx);
                        }
                    }
                    (TokenKind::Plus, Type::Long) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("add {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Minus, Type::Long) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("sub {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Star, Type::Long) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.buffer
                            .push_str(&format!("imul {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Slash, Type::Long) => {
                        self.div(right, register);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rax {
                            self.assign_register(register);
                            self.buffer.push_str(Register::Rax.as_str());
                            self.newline();
                            self.zero_register(Register::Rax);

                            self.registers.free(Register::Rax);
                        }
                    }
                    (TokenKind::Percent, Type::Long) => {
                        self.div(right, register);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rdx {
                            self.assign_register(register);
                            self.buffer.push_str(Register::Rdx.as_str());
                            self.newline();
                            self.zero_register(Register::Rdx);

                            self.registers.free(Register::Rdx);
                        }
                    }
                    _ => {
                        dbg!(left, right, op, t);
                        todo!("binary operation")
                    }
                }
                self.newline();
            }
            _ => unreachable!(),
        }
    }

    fn unary(&mut self, expr: &AstNodeExpr, register: Register) {
        match expr {
            AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Minus,
                        ..
                    },
                expr,
                ..
            } => {
                self.expr(expr, register);
                self.newline();

                self.buffer.push_str(&format!("neg {}\n", register));
            }
            AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Plus,
                        ..
                    },
                expr,
                ..
            } => {
                self.expr(expr, register);
                self.newline();
            }
            _ => todo!(),
        }
    }

    fn synthetic_literal_string(&mut self, s: &'static str) -> String {
        self.constants.find_or_create_string(s)
    }

    fn assign_register(&mut self, register: Register) {
        self.registers.reserve(register);
        self.buffer.push_str(&format!("mov {}, ", register));
    }

    // Consume the origin register
    fn transfer_register(&mut self, source: Register, destination: Register) {
        self.assign_register(source);
        self.buffer.push_str(destination.as_str());
        self.registers.free(source);
    }

    fn deref_string_from_label(&mut self, register: Register, fmt_string_label: &str) {
        self.buffer
            .push_str(&format!("lea {}, [{}]\n", register, fmt_string_label));
        self.newline();
    }

    fn println(&mut self, expr: &AstNodeExpr, register: Register) {
        if !self.registers.is_free(REGISTER_ARG_1) {
            todo!("Re-arrange registers");
        }

        let t = self.types.get(&expr.id()).unwrap();
        let fmt_string_label = match t {
            Type::Boolean | Type::Int => self.synthetic_literal_string(PRINTF_FMT_INT),
            Type::TString => self.synthetic_literal_string(PRINTF_FMT_STRING),
            Type::Long => self.synthetic_literal_string(PRINTF_FMT_LONG),
            _ => todo!(),
        };
        self.deref_string_from_label(REGISTER_ARG_1, &fmt_string_label);

        self.expr(expr, register);

        if register != REGISTER_ARG_2 {
            if !self.registers.is_free(REGISTER_ARG_2) {
                todo!("Re-arrange registers");
            }

            self.assign_register(REGISTER_ARG_2);
            self.buffer.push_str(register.as_str());
            self.newline();
            self.zero_register(register);
        }

        self.call_function("_printf", 2);
    }

    fn literal(&mut self, token: &Token, register: Register) {
        match token.kind {
            TokenKind::Int(n) => {
                self.assign_register(register);
                self.buffer.push_str(&format!("{}", n));
                self.newline();
            }
            TokenKind::Long(n) => {
                self.assign_register(register);
                self.buffer.push_str(&format!("{}", n));
                self.newline();
            }
            TokenKind::Boolean(true) => {
                self.assign_register(register);
                self.buffer.push_str("1");
                self.newline();
            }
            TokenKind::Boolean(false) => {
                self.assign_register(register);
                self.buffer.push_str("0");
                self.newline();
            }
            TokenKind::Char(c) => {
                self.assign_register(register);
                self.buffer.push_str(&format!("'{}'", c));
                self.newline();
            }
            TokenKind::TString => {
                let s = String::from(&self.session.src[token.span.start + 1..token.span.end - 1]);
                let label = self.constants.find_or_create_string(&s);

                self.deref_string_from_label(register, &label);
            }
            _ => todo!(),
        }
    }
}
