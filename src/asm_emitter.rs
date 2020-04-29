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
    int_fmt_string_label: String,
    string_fmt_string_label: String,
    registers: Registers,
}

fn assign_register_op(t: &Type) -> &'static str {
    match t {
        Type::TString => "lea",
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
        self.buffer.push_str("\n");
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
            AstNodeExpr::Literal(tok, _) => self.literal(tok),
            AstNodeExpr::Println(expr, _) => self.println(expr),
            AstNodeExpr::Unary { .. } => self.unary(expr, register),
            _ => todo!(),
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
                self.buffer.push_str("\n");

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
                self.buffer.push_str("\n");
            }
            _ => todo!(),
        }
    }

    fn println(&mut self, expr: &AstNodeExpr) {
        let t = self.types.get(&expr.id()).unwrap();
        let fmt_string_label = match t {
            Type::Int => &self.int_fmt_string_label,
            Type::TString => &self.string_fmt_string_label,
            _ => todo!(),
        };

        self.registers.reserve(REGISTER_ARG_1);
        self.registers.reserve(REGISTER_ARG_2);

        self.buffer.push_str(&format!(
            r##"
            {first_arg_assign_op} {first_fn_arg_reg}, [{fmt_string_label}]
            {second_arg_assign_op} {second_fn_arg_reg}, "##,
            first_arg_assign_op = assign_register_op(&Type::TString),
            first_fn_arg_reg = REGISTER_ARG_1,
            fmt_string_label = fmt_string_label,
            second_fn_arg_reg = REGISTER_ARG_2,
            second_arg_assign_op = assign_register_op(t)
        ));
        self.expr(expr, REGISTER_ARG_2);

        self.call_function("_printf");

        self.zero_register(REGISTER_RETURN_VALUE);
        self.registers.free(REGISTER_ARG_1);
        self.registers.free(REGISTER_ARG_2);
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
