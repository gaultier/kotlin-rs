use crate::asm_constants::Constants;
use crate::asm_registers::{
    Register, Registers, REGISTER_ARG_1, REGISTER_ARG_2, REGISTER_RETURN_VALUE,
};
use crate::error::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use crate::session::Session;
use log::debug;

const LABEL_FLAG_NONE: u8 = 0;
const LABEL_FLAG_ENTRYPOINT: u8 = 1;
const LABEL_FLAG_EXTERN: u8 = 2;

type LabelFlag = u8;

#[derive(Debug)]
struct Label {
    name: String,
    buffer: String,
    flags: LabelFlag,
}

impl Label {
    fn new(name: String, flags: LabelFlag) -> Label {
        Label {
            name,
            buffer: String::new(),
            flags,
        }
    }

    fn write_flag<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        if (self.flags & LABEL_FLAG_EXTERN) != 0 {
            writeln!(w, "extern {}", self.name)?;
        }
        if (self.flags & LABEL_FLAG_ENTRYPOINT) != 0 {
            writeln!(w, "global {}", self.name)?;
        }

        Ok(())
    }

    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        self.write_flag(w)?;
        write!(w, "{}:\n", self.name)?;
        w.write_all(self.buffer.as_bytes())?;
        w.write_all(b"\n")?;

        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct AsmEmitter<'a> {
    session: &'a Session<'a>,
    types: &'a Types,
    resolution: &'a Resolution,
    constants: Constants,
    registers: Registers,
    label_count: usize,
    id_to_register: Vec<(NodeId, Register)>,
    labels: Vec<Label>,
    current_label_index: usize,
}

const PRINTF_FMT_STRING: &str = "\"%s\", 0xa"; // 0xa = \n
const PRINTF_FMT_INT: &str = "\"%d\", 0xa"; // 0xa = \n
const PRINTF_FMT_LONG: &str = "\"%ld\", 0xa"; // 0xa = \n

fn logic_op(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::EqualEqual => "sete",
        TokenKind::Lesser => "setl",
        TokenKind::BangEqual => "setne",
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
            constants: Constants::new(),
            registers: Registers::new(),
            label_count: 0,
            id_to_register: Vec::new(),
            labels: vec![Label::new(String::from("_main"), LABEL_FLAG_ENTRYPOINT)],
            current_label_index: 0,
        }
    }

    fn add_code(&mut self, s: &str) {
        self.labels[self.current_label_index].buffer.push_str(s);
    }

    fn generate_new_label(&mut self) -> String {
        self.label_count += 1;
        return format!(".L{}", self.label_count);
    }

    fn fn_prolog(&mut self) {
        self.add_code(
            r##"%push fn_context ; save the current context
%stacksize flat64 ; tell NASM to use bp
%assign %$localsize 0 ; 0 locals
enter   %$localsize, 0
"##,
        );
        self.newline();
    }

    fn fn_epilog(&mut self) {
        self.add_code("leave");
        self.newline();
        self.add_code("ret");
        self.newline();
        self.add_code("%pop");
        self.newline();
    }

    fn prolog<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
        w.write_all(
            &r##"BITS 64 ; 64 bits
CPU X64 ; target the x86_64 family of CPUs
DEFAULT REL ; relative addressing mode

extern _printf ; might be unused but that is ok
"##
            .as_bytes(),
        )?;
        Ok(())
    }

    fn data_section<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
        w.write_all(b"section .data\n")?;
        for (constant, label) in self.constants.iter() {
            w.write_all(&format!("{}: db {}, 0 ; null terminated\n", label, constant).as_bytes())?;
        }

        self.newline();

        Ok(())
    }

    fn newline(&mut self) {
        self.add_code("\n");
    }

    fn text_section<W: std::io::Write>(&mut self, w: &mut W) -> Result<(), Error> {
        w.write_all(b"section .text\n\n")?;
        Ok(())
    }

    fn zero_register(&mut self, register: Register) {
        self.registers.free(register);
        self.add_code(&format!("xor {}, {}", register, register));
        self.newline();
    }

    fn call_function(&mut self, fn_name: &str, arg_count: usize) {
        assert_eq!(arg_count, 2);

        self.add_code("call ");
        self.add_code(fn_name);
        self.newline();

        self.registers.free(REGISTER_ARG_1);
        self.registers.free(REGISTER_ARG_2);
    }

    pub(crate) fn main<W: std::io::Write>(
        &mut self,
        statements: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.fn_prolog();
        let register = self.registers.allocate().unwrap();
        self.statement(statements, register);
        self.zero_register(REGISTER_RETURN_VALUE);
        self.fn_epilog();

        self.prolog(w)?;
        self.data_section(w)?;
        self.text_section(w)?;

        for label in &self.labels {
            label.write(w)?;
        }
        w.flush()?;

        Ok(())
    }

    fn find_var_register(&mut self, id: NodeId) -> Option<Register> {
        self.id_to_register
            .iter()
            .find(|elem| elem.0 == id)
            .map(|elem| elem.1)
    }

    fn assign_var_to_register(&mut self, id: NodeId, register: Register) {
        assert!(self.find_var_register(id).is_none());

        self.id_to_register.push((id, register));
    }

    fn var_def(&mut self, id: NodeId, value: &AstNodeExpr, register: Register) {
        self.expr(value, register);
        self.newline();
        self.assign_var_to_register(id, register);
    }

    fn while_stmt(&mut self, cond: &AstNodeExpr, body: &AstNodeStmt, register: Register) {
        let loop_label = self.generate_new_label();
        let end_label = self.generate_new_label();
        self.newline();
        self.newline();

        self.add_code(&loop_label);
        self.add_code(": ; loop body");
        self.newline();

        self.expr(cond, register);
        self.add_code(&format!("cmp {}, 1", register));
        self.newline();

        self.add_code(&format!("jne {} ; stop loop", end_label));
        self.newline();
        self.registers.free(register);

        self.statement(body, register);
        self.add_code(&format!("jmp {} ; jump to start of loop body", loop_label));
        self.newline();
        self.newline();

        self.add_code(&end_label);
        self.add_code(": ; loop is finished");
        self.newline();
    }

    fn assign(&mut self, target: &AstNodeExpr, value: &AstNodeExpr, register: Register) {
        self.expr(value, register);

        match target {
            AstNodeExpr::VarRef(_, id) => {
                let node_ref_id = self.resolution.get(&id).unwrap().node_ref_id;
                let var_register = self.find_var_register(node_ref_id).unwrap();
                self.assign_register(var_register);

                self.add_code(register.as_str());
                self.newline();
            }
            _ => todo!(),
        }
    }

    fn statement(&mut self, statement: &AstNodeStmt, register: Register) {
        match statement {
            AstNodeStmt::Expr(expr) => {
                self.expr(expr, register);
            }
            AstNodeStmt::Assign { target, value, .. } => self.assign(target, value, register),
            AstNodeStmt::Block { body, .. } => {
                if let Some((stmt, rest)) = body.split_first() {
                    self.statement(stmt, register);

                    for stmt in rest {
                        let register = self.registers.allocate().unwrap();
                        self.statement(stmt, register);
                    }
                }
            }
            AstNodeStmt::While { cond, body, .. } => self.while_stmt(cond, body, register),
            AstNodeStmt::VarDefinition { id, value, .. } => self.var_def(*id, value, register),
            AstNodeStmt::FnDefinition {
                fn_name,
                args,
                id,
                flags,
                body,
                ..
            } => self.fn_def(fn_name, args, body, *flags, *id),
            _ => todo!(),
        }
    }

    fn add_label(&mut self, label: Label) {
        self.labels.push(label);
        self.current_label_index += 1;
    }

    fn fn_def(
        &mut self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        body: &AstNodeStmt,
        _flags: u16,
        _id: NodeId,
    ) {
        let label_containing_fn_def = self.current_label_index;
        let fn_name_s = match fn_name {
            AstNodeExpr::VarRef(span, _) => &self.session.src[span.start..span.end],
            _ => unreachable!(),
        };
        debug!(
            "fn_def: name={} args={:?} registers=[{}]",
            fn_name_s, args, self.registers
        );

        // FIXME: use function flags
        self.add_label(Label::new(fn_name_s.to_owned(), LABEL_FLAG_NONE));

        // let fn_t = self.types.get(&id).unwrap();

        self.fn_prolog();

        if args.len() == 1 {
            self.registers.reserve(REGISTER_ARG_1);
            self.assign_var_to_register(args[0].id(), REGISTER_ARG_1);
        } else if args.len() == 0 {
            // No-op
        } else {
            todo!("More than one arg")
        }

        let register = self.registers.allocate().unwrap();
        debug!(
            "fn_def before body: name={} args={:?} registers=[{}] register={}",
            fn_name_s, args, self.registers, register
        );

        self.statement(body, register);

        self.fn_epilog();

        self.current_label_index = label_containing_fn_def;
    }

    fn if_expr(
        &mut self,
        cond: &AstNodeExpr,
        if_body: &AstNodeStmt,
        else_body: &AstNodeStmt,
        register: Register,
    ) {
        let else_body_label = self.generate_new_label();
        let merge_bodies_label = self.generate_new_label();

        self.expr(cond, register);
        self.add_code(&format!("cmp {}, 1", register));
        self.newline();

        self.add_code(&format!("jne {} ; else branch", else_body_label));
        self.newline();
        self.registers.free(register);

        self.statement(if_body, register);
        self.add_code(&format!("jmp {} ; end of if", merge_bodies_label));
        self.newline();
        self.newline();

        self.add_code(&else_body_label);
        self.add_code(":");
        self.newline();
        self.statement(else_body, register);
        self.newline();
        self.newline();

        self.add_code(&merge_bodies_label);
        self.add_code(":");
        self.newline();
    }

    fn var_ref(&mut self, id: NodeId, register: Register) {
        self.assign_register(register);
        let node_ref_id = self.resolution.get(&id).unwrap().node_ref_id;
        let var_reg = self.find_var_register(node_ref_id).unwrap();
        self.add_code(var_reg.as_str());
        self.newline();
    }

    fn fn_call(
        &mut self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        _id: NodeId,
        _register: Register,
    ) {
        let fn_name_s = match fn_name {
            AstNodeExpr::VarRef(span, _) => &self.session.src[span.start..span.end],
            _ => unreachable!(),
        };
        debug!("fn_call: name={} args={:?}", fn_name_s, args);

        // FIXME: put arguments in the right registers
        // FIXME: if `register` is `rax`, it will be overriden

        let args_count = args.len();
        if args_count > 1 {
            todo!("More than one function argument")
        }
        if args_count == 1 && !self.registers.is_free(REGISTER_ARG_1) {
            todo!("Re-arrange registers");
        }

        if args_count == 1 {
            let arg = &args[0];

            self.registers.reserve(REGISTER_ARG_1);
            self.assign_var_to_register(arg.id(), REGISTER_ARG_1);
            self.expr(arg, REGISTER_ARG_1);
        }

        self.add_code(&format!("call {}\n", fn_name_s));
    }

    fn expr(&mut self, expr: &AstNodeExpr, register: Register) {
        match expr {
            AstNodeExpr::Literal(tok, _) => self.literal(tok, register),
            AstNodeExpr::VarRef(_, id) => self.var_ref(*id, register),
            AstNodeExpr::Println(expr, _) => self.println(expr, register),
            AstNodeExpr::Unary { .. } => self.unary(expr, register),
            AstNodeExpr::Binary { .. } => self.binary(expr, register),
            AstNodeExpr::Grouping(expr, _) => self.expr(expr, register),
            AstNodeExpr::FnCall {
                fn_name, args, id, ..
            } => self.fn_call(fn_name, args, *id, register),
            AstNodeExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => self.if_expr(cond, if_body, else_body, register),
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
            self.add_code(register.as_str());
            self.newline();
        }
        // Required to store the sign in rdx:rax, otherwise we get a FPE
        self.add_code("cqo");
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

        self.add_code(&format!("idiv {}", Register::Rcx));
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
                    (kind, Type::Boolean) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("cmp {}, {}", register, intermediate_register));
                        self.newline();
                        self.registers.free(intermediate_register);

                        self.add_code(&format!("{} {}", logic_op(&kind), register.as_byte_str()));
                    }
                    (TokenKind::Plus, Type::Int) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("add {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Minus, Type::Int) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("sub {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Star, Type::Int) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("imul {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Slash, Type::Int) => {
                        self.div(right, register);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rax {
                            self.assign_register(register);
                            self.add_code(Register::Rax.as_str());
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
                            self.add_code(Register::Rdx.as_str());
                            self.newline();
                            self.zero_register(Register::Rdx);

                            self.registers.free(Register::Rdx);
                        }
                    }
                    (TokenKind::Plus, Type::Long) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("add {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Minus, Type::Long) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("sub {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Star, Type::Long) => {
                        let intermediate_register = self.registers.allocate().unwrap();
                        self.expr(right, intermediate_register);

                        self.add_code(&format!("imul {}, {}", register, intermediate_register));
                        self.registers.free(intermediate_register);
                    }
                    (TokenKind::Slash, Type::Long) => {
                        self.div(right, register);
                        // We copy the result of the division in the original register if needed
                        if register != Register::Rax {
                            self.assign_register(register);
                            self.add_code(Register::Rax.as_str());
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
                            self.add_code(Register::Rdx.as_str());
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

                self.add_code(&format!("neg {}\n", register));
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
        self.add_code(&format!("mov {}, ", register));
    }

    // Consume the origin register
    fn transfer_register(&mut self, source: Register, destination: Register) {
        self.assign_register(destination);
        self.add_code(source.as_str());
        self.newline();
        self.registers.free(source);

        for (_, reg) in self.id_to_register.iter_mut() {
            if reg == &source {
                *reg = destination;
            }
        }
    }

    fn deref_string_from_label(&mut self, register: Register, fmt_string_label: &str) {
        self.add_code(&format!("lea {}, [{}]\n", register, fmt_string_label));
        self.newline();
    }

    fn println(&mut self, expr: &AstNodeExpr, register: Register) {
        let register = if !self.registers.is_free(REGISTER_ARG_1)
            || !self.registers.is_free(REGISTER_ARG_2)
            || !self.registers.is_free(REGISTER_RETURN_VALUE)
        {
            let copy_arg1_register = self.registers.allocate().unwrap();
            self.transfer_register(REGISTER_ARG_1, copy_arg1_register);
            // TODO: arg2, return value
            copy_arg1_register
        } else {
            register
        };

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
            self.assign_register(REGISTER_ARG_2);
            self.add_code(register.as_str());
            self.newline();
        }

        self.call_function("_printf", 2);
    }

    fn literal(&mut self, token: &Token, register: Register) {
        match token.kind {
            TokenKind::Int(n) => {
                self.assign_register(register);
                self.add_code(&format!("{}", n));
                self.newline();
            }
            TokenKind::Long(n) => {
                self.assign_register(register);
                self.add_code(&format!("{}", n));
                self.newline();
            }
            TokenKind::Boolean(true) => {
                self.assign_register(register);
                self.add_code("1");
                self.newline();
            }
            TokenKind::Boolean(false) => {
                self.assign_register(register);
                self.add_code("0");
                self.newline();
            }
            TokenKind::Char(c) => {
                self.assign_register(register);
                self.add_code(&format!("'{}'", c));
                self.newline();
            }
            TokenKind::TString => {
                let s = String::from(&self.session.src[token.span.start..token.span.end]);
                let label = self.constants.find_or_create_string(&s);

                self.deref_string_from_label(register, &label);
            }
            _ => todo!(),
        }
    }
}
