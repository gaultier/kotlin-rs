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

fn assign_register_op(t: &Type) -> &'static str {
    match t {
        Type::TString => "mov",
        Type::Int => "mov",
        _ => todo!(),
    }
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
                self.expr(expr, Some(register));
            }
            AstNodeStmt::Block { body, .. } => {
                for stmt in body {
                    self.statement(stmt);
                }
            }
            _ => todo!(),
        }
    }

    fn expr(&mut self, expr: &AstNodeExpr, register: Option<Register>) {
        match expr {
            AstNodeExpr::Literal(tok, _) => self.literal(tok, register),
            AstNodeExpr::Println(expr, _) => self.println(expr, register.unwrap()),
            AstNodeExpr::Unary { .. } => self.unary(expr, register.unwrap()),
            AstNodeExpr::Binary { .. } => self.binary(expr, register.unwrap()),
            _ => todo!(),
        }
    }

    // `rax`: dividend, register or next free register: dividend, `rdx`: remainder
    fn div(&mut self, right: &AstNodeExpr, register: Register, t: &Type) {
        // The dividend must be in `rax`, so we copy `register` in rax
        if register != Register::Rax {
            if !self.registers.is_free(Register::Rax) {
                dbg!(&self.registers);
                todo!("Re-arrange registers");
            }

            self.registers.reserve(Register::Rax);
            self.assign_register(Register::Rax, t);
            self.buffer.push_str(&format!("{}", register));
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
            self.expr(right, Some(Register::Rcx));
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

                self.expr(left, Some(register));
                self.newline();

                match (op.kind, t) {
                    (TokenKind::Plus, Type::Int) => {
                        self.buffer.push_str(&format!("add {}, ", register));
                        self.expr(right, None);
                    }
                    (TokenKind::Minus, Type::Int) => {
                        self.buffer.push_str(&format!("sub {}, ", register));
                        self.expr(right, None);
                    }
                    (TokenKind::Star, Type::Int) => {
                        self.buffer.push_str(&format!("imul {}, ", register));
                        self.expr(right, None);
                    }
                    (TokenKind::Slash, Type::Int) => {
                        self.div(right, register, t);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rax {
                            self.assign_register(register, t);
                            self.buffer.push_str(&format!("{}", Register::Rax));
                            self.newline();
                            self.zero_register(Register::Rax);

                            self.registers.free(Register::Rax);
                        }
                    }
                    (TokenKind::Percent, Type::Int) => {
                        self.div(right, register, t);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rdx {
                            self.assign_register(register, t);
                            self.buffer.push_str(&format!("{}", Register::Rdx));
                            self.newline();
                            self.zero_register(Register::Rdx);

                            self.registers.free(Register::Rdx);
                        }
                    }
                    _ => todo!(),
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
                self.expr(expr, Some(register));
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
                self.expr(expr, Some(register));
                self.newline();
            }
            _ => todo!(),
        }
    }

    fn synthetic_literal_string(&mut self, s: &'static str) -> String {
        self.constants.find_or_create_string(s)
    }

    fn assign_register(&mut self, register: Register, t: &Type) {
        self.registers.reserve(register);
        self.buffer
            .push_str(&format!("{} {}, ", assign_register_op(t), register));
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
            Type::Int => self.synthetic_literal_string(PRINTF_FMT_INT),
            Type::TString => self.synthetic_literal_string(PRINTF_FMT_STRING),
            _ => todo!(),
        };
        self.deref_string_from_label(REGISTER_ARG_1, &fmt_string_label);

        self.expr(expr, Some(register));

        if register != REGISTER_ARG_2 {
            if !self.registers.is_free(REGISTER_ARG_2) {
                todo!("Re-arrange registers");
            }

            self.assign_register(REGISTER_ARG_2, t);
            self.buffer.push_str(&format!("{}", register));
            self.newline();
            self.zero_register(register);
        }

        self.call_function("_printf", 2);
    }

    fn literal(&mut self, token: &Token, register: Option<Register>) {
        if let Some(register) = register {
            // String requires `lea`
            if token.kind != TokenKind::TString {
                self.buffer.push_str(&format!("mov {}, ", register));
            }
        }

        match token.kind {
            TokenKind::Int(n) => self.buffer.push_str(&format!("{}", n)),
            TokenKind::Long(n) => self.buffer.push_str(&format!("{}", n)),
            TokenKind::Boolean(true) => self.buffer.push_str("1"),
            TokenKind::Boolean(false) => self.buffer.push_str("0"),
            TokenKind::Char(c) => self.buffer.push_str(&format!("'{}'", c)),
            TokenKind::TString => {
                let s = String::from(&self.session.src[token.span.start + 1..token.span.end - 1]);
                let label = self.constants.find_or_create_string(&s);
                self.deref_string_from_label(register.unwrap(), &label);
            }
            _ => todo!(),
        }

        if register.is_some() {
            self.newline();
        }
    }
}
