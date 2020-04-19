use crate::error::*;
use crate::jvm_constants::*;
use crate::jvm_emitter::{Attribute, JvmEmitter};
use crate::jvm_stack_map_frame::StackMapFrame;
use crate::parse::*;
use log::debug;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub(crate) enum JumpTarget {
    If {
        if_location: u16,
        if_target: u16,
        else_location: u16,
        else_target: u16,
    },
    Goto {
        location: u16,
        target: u16,
    },
}

pub(crate) struct IfBuilder {
    if_location: Option<u16>,
    if_target: Option<u16>,
    goto_location: Option<u16>,
    goto_target: Option<u16>,
}

impl IfBuilder {
    pub(crate) fn new() -> IfBuilder {
        IfBuilder {
            if_location: None,
            if_target: None,
            goto_location: None,
            goto_target: None,
        }
    }

    pub(crate) fn cond(
        mut self,
        cond: &AstNodeExpr,
        jvm_emitter: &mut JvmEmitter,
        code_builder: &mut CodeBuilder,
    ) -> Result<Self, Error> {
        jvm_emitter.expr(cond, code_builder)?;
        code_builder.push3(OP_IFEQ, OP_IMPDEP1, OP_IMPDEP2, Type::Nothing)?;
        self.if_location = Some((code_builder.code.len() - 3) as u16);

        Ok(self)
    }

    pub(crate) fn if_body(
        mut self,
        if_body: &AstNodeStmt,
        jvm_emitter: &mut JvmEmitter,
        code_builder: &mut CodeBuilder,
    ) -> Result<Self, Error> {
        jvm_emitter.statement(if_body, code_builder)?;
        code_builder.push3(OP_GOTO, OP_IMPDEP1, OP_IMPDEP2, Type::Nothing)?;

        let end_if_body = (code_builder.code.len() - 1) as u16;
        self.goto_location = Some(end_if_body - 2);

        let start_else_offset: u16 = (3 + 3 + end_if_body - self.goto_location.unwrap()) as u16;
        code_builder.code[self.if_location.unwrap() as usize + 1] =
            start_else_offset.to_be_bytes()[0];
        code_builder.code[self.if_location.unwrap() as usize + 2] =
            start_else_offset.to_be_bytes()[1];

        self.if_target = Some(1 + end_if_body);

        Ok(self)
    }

    pub(crate) fn else_body(
        mut self,
        else_body: &AstNodeStmt,
        jvm_emitter: &mut JvmEmitter,
        code_builder: &mut CodeBuilder,
    ) -> Result<Self, Error> {
        jvm_emitter.statement(else_body, code_builder)?;

        let end = (code_builder.code.len() - 1) as u16;

        let start_rest_offset: u16 = 1 + end - self.goto_location.unwrap();
        code_builder.code[self.goto_location.unwrap() as usize + 1] =
            start_rest_offset.to_be_bytes()[0];
        code_builder.code[self.goto_location.unwrap() as usize + 2] =
            start_rest_offset.to_be_bytes()[1];

        self.goto_target = Some(1 + end);

        Ok(self)
    }

    pub(crate) fn build(self, code_builder: &mut CodeBuilder) -> Self {
        code_builder.jump_target_at(
            self.if_location.unwrap(),
            JumpTarget::If {
                if_location: self.if_location.unwrap(),
                if_target: self.if_target.unwrap(),
                else_location: self.goto_location.unwrap(),
                else_target: self.goto_target.unwrap(),
            },
        );

        self
    }
}

type Local = (NodeId, Type);

#[derive(Debug)]
pub(crate) struct CodeBuilder {
    pub(crate) code: Vec<u8>,
    pub(crate) attributes: Vec<Attribute>,
    pub(crate) stack: Vec<Type>,
    pub(crate) locals: Vec<Local>,
    pub(crate) starting_locals: Vec<Local>,
    pub(crate) stack_max: u16,
    pub(crate) locals_max: u16,
    pub(crate) stack_map_frames: BTreeMap<u16, StackMapFrame>,
    pub(crate) opcode_types: Vec<Type>,
    pub(crate) jump_targets: BTreeMap<u16, JumpTarget>,
}

impl CodeBuilder {
    pub(crate) fn new() -> CodeBuilder {
        CodeBuilder {
            code: Vec::new(),
            attributes: Vec::new(),
            stack: Vec::new(),
            locals: Vec::new(),
            starting_locals: Vec::new(),
            stack_max: 100,
            locals_max: 0,
            stack_map_frames: BTreeMap::new(),
            opcode_types: Vec::new(),
            jump_targets: BTreeMap::new(),
        }
    }

    fn stack_pop(&mut self) -> Result<Type, Error> {
        self.stack
            .pop()
            .ok_or_else(|| Error::new(ErrorKind::JvmStackUnderflow, Location::new()))
    }

    fn stack_pop2(&mut self) -> Result<[Type; 2], Error> {
        let a = self.stack_pop()?;
        let b = self.stack_pop()?;
        Ok([a, b])
    }

    fn stack_push(&mut self, t: Type) -> Result<(), Error> {
        if self.stack.len() == std::u8::MAX as usize {
            return Err(Error::new(ErrorKind::JvmStackOverflow, Location::new()));
        }

        self.stack.push(t);

        self.stack_max = std::cmp::max(self.stack.len() as u16, self.stack_max);
        Ok(())
    }

    pub(crate) fn locals_find_by_id(&self, id: NodeId) -> Option<(u16, Local)> {
        self.locals.iter().enumerate().find_map(|(i, l)| {
            if l.0 == id {
                Some((i as u16, l.clone()))
            } else {
                None
            }
        })
    }

    pub(crate) fn locals_push(&mut self, l: Local) -> Result<u16, Error> {
        if self.locals.len() == std::u16::MAX as usize {
            return Err(Error::new(ErrorKind::JvmLocalsOverflow, Location::new()));
        }

        let i = if l.1 == Type::Long || l.1 == Type::Double {
            self.locals.push(l.clone());
            self.locals.push(l);
            self.locals.len() - 2
        } else {
            self.locals.push(l);
            self.locals.len() - 1
        };

        self.locals_max = std::cmp::max(self.locals.len() as u16, self.locals_max);
        Ok(i as u16)
    }

    fn stack_map_frame_add_full(&mut self, bci_target: u16) {
        debug!("stack_map_frame_add_full: bci_target={}", bci_target);

        // TODO: check overflow
        self.stack_map_frames.insert(
            bci_target,
            StackMapFrame::Full {
                offset: 0, // Will be computed in a final step
                stack: self
                    .stack
                    .iter()
                    .map(|t| t.to_verification_info())
                    .collect::<Vec<_>>(),
                locals: self
                    .locals
                    .iter()
                    .map(|(_, t)| t.to_verification_info())
                    .collect::<Vec<_>>(),
            },
        );
    }

    pub(crate) fn stack_map_frames_compute_delta_offsets(&mut self) -> Vec<StackMapFrame> {
        // Conceptually u16 but the first item needs to be off by one
        let mut last_bci_target: i32 = -1;

        for (bci_target, smp) in self.stack_map_frames.iter_mut() {
            smp.offset((*bci_target as i32 - last_bci_target - 1) as u16);
            last_bci_target = *bci_target as i32;
        }
        self.stack_map_frames.values().cloned().collect::<Vec<_>>()
    }

    pub(crate) fn push1(&mut self, op: u8, t: Type) -> Result<(), Error> {
        self.push(op, None, None, t)
    }

    pub(crate) fn push2(&mut self, op: u8, operand1: u8, t: Type) -> Result<(), Error> {
        self.push(op, Some(operand1), None, t)
    }

    pub(crate) fn push3(
        &mut self,
        op: u8,
        operand1: u8,
        operand2: u8,
        t: Type,
    ) -> Result<(), Error> {
        self.push(op, Some(operand1), Some(operand2), t)
    }

    fn jump_target_at(&mut self, location: u16, jump_target: JumpTarget) {
        self.jump_targets.insert(location, jump_target);
    }

    fn verify(&mut self, jvm_emitter: &JvmEmitter) -> Result<(), Error> {
        debug!("verify: jump_targets={:?}", &self.jump_targets);

        // let needed = self.jump_targets.clone();
        // let cpy_locals = self.starting_locals.clone();

        // for bci in &needed {
        //     let mut i = 0;
        //     self.stack = Vec::new();
        //     self.locals = cpy_locals.clone();

        //     while i < *bci {
        //         let op = self.code[i as usize];
        //         debug!("verify: op={}", op);

        //         match op {
        //             OP_ICONST_M1 | OP_ICONST_0 | OP_ICONST_1 | OP_ICONST_2 | OP_ICONST_3
        //             | OP_ICONST_4 | OP_ICONST_5 => {
        //                 self.stack_push(Type::Int)?;
        //             }
        //             OP_SIPUSH => {
        //                 i += 2;
        //                 self.stack_push(Type::Int)?;
        //             }
        //             OP_BIPUSH => {
        //                 i += 1;
        //                 self.stack_push(Type::Int)?;
        //             }
        //             OP_FCONST_0 | OP_FCONST_1 | OP_FCONST_2 => {
        //                 self.stack_push(Type::Float)?;
        //             }
        //             OP_IADD | OP_IMUL | OP_ISUB | OP_IDIV | OP_IREM | OP_IAND | OP_IOR
        //             | OP_FADD | OP_FMUL | OP_FSUB | OP_FDIV | OP_FREM => {
        //                 self.stack_pop()?;
        //             }
        //             OP_FCMPL => {
        //                 self.stack_pop2()?;
        //                 self.stack_push(Type::Int)?;
        //             }
        //             OP_IFEQ | OP_IFNE | OP_IFGE | OP_IFGT | OP_IFLE | OP_IFLT => {
        //                 let op1 = self.code[i as usize + 1];
        //                 let op2 = self.code[i as usize + 2];
        //                 let offset = u16::from_be_bytes([op1, op2]);

        //                 debug!(
        //                     "verify: if i={} offset={} stack={:?}",
        //                     i, offset, self.stack
        //                 );
        //                 self.stack_pop()?;

        //                 if i + offset == *bci {
        //                     // Finished
        //                     break;
        //                 } else {
        //                     i += 2;
        //                 }
        //             }
        //             OP_IF_ICMPNE | OP_IF_ICMPGE | OP_IF_ICMPLE | OP_IF_ICMPGT | OP_IF_ICMPLT => {
        //                 let op1 = self.code[i as usize + 1];
        //                 let op2 = self.code[i as usize + 2];
        //                 let offset = u16::from_be_bytes([op1, op2]);

        //                 self.stack_pop2()?;

        //                 debug!("verify: if i={} offset={}", i, offset);

        //                 if i + offset == *bci {
        //                     // Finished
        //                     break;
        //                 } else {
        //                     i += 2;
        //                 }
        //             }
        //             OP_LCMP | OP_DCMPL => {
        //                 self.stack_pop2()?;
        //             }
        //             OP_GOTO => {
        //                 let op1 = self.code[i as usize + 1];
        //                 let op2 = self.code[i as usize + 2];

        //                 let offset = i16::from_be_bytes([op1, op2]);
        //                 debug!(
        //                     "verify: goto i={} offset={} op1={} op2={}",
        //                     i, offset, op1, op2
        //                 );

        //                 if i as isize + offset as isize == *bci as isize {
        //                     // Finished
        //                     break;
        //                 } else {
        //                     i += 2;
        //                 }
        //             }
        //             OP_GET_STATIC => {
        //                 let t = &self.opcode_types[i as usize];
        //                 let jvm_constant_pool_index = match t {
        //                     Type::Object {
        //                         jvm_constant_pool_index,
        //                         ..
        //                     } => jvm_constant_pool_index.unwrap(),
        //                     _ => unreachable!(),
        //                 };

        //                 i += 2;
        //                 // FIXME: hardcoded for println
        //                 self.stack_push(Type::Object {
        //                     class: String::from("java/io/PrintStream"),
        //                     jvm_constant_pool_index: Some(jvm_constant_pool_index),
        //                 })?;
        //             }
        //             OP_ISTORE => {
        //                 self.locals_push((0xbeef, Type::Int))?;
        //                 i += 1;
        //                 self.stack_pop()?;
        //             }
        //             OP_FSTORE => {
        //                 self.locals_push((0xbeef, Type::Float))?;
        //                 i += 1;
        //                 self.stack_pop()?;
        //             }
        //             OP_LSTORE => {
        //                 todo!();
        //                 i += 1;
        //                 self.stack_pop2()?;
        //             }
        //             OP_DSTORE => {
        //                 todo!();
        //                 i += 1;
        //                 self.stack_pop2()?;
        //             }
        //             OP_ILOAD => {
        //                 i += 1;
        //                 self.stack_push(Type::Int)?;
        //             }
        //             OP_FLOAD => {
        //                 i += 1;
        //                 self.stack_push(Type::Float)?;
        //             }
        //             OP_LLOAD => {
        //                 i += 1;
        //                 self.stack_push(Type::Long)?;
        //                 self.stack_push(Type::Long)?;
        //             }
        //             OP_INVOKE_VIRTUAL => {
        //                 i += 2;
        //                 self.stack_pop2()?; // FIXME: hardcoded for println
        //             }
        //             OP_INVOKE_STATIC | OP_INVOKE_SPECIAL => {
        //                 let op1 = self.code[i as usize + 1];
        //                 let op2 = self.code[i as usize + 2];
        //                 i += 2;

        //                 let fn_i: u16 = u16::from_be_bytes([op1, op2]);
        //                 // The constant pool is one-indexed
        //                 let fn_id: NodeId =
        //                     *jvm_emitter.constant_pool_index_to_fn_id.get(&fn_i).unwrap();
        //                 let fn_t = jvm_emitter.types.get(&fn_id).unwrap();
        //                 let return_t = fn_t.fn_return_t();
        //                 debug!("verify: op={} return_t={:?}", op, return_t);

        //                 match fn_t {
        //                     Type::Function { return_t, args, .. } => {
        //                         for _ in 0..args.len() {
        //                             self.stack_pop()?; // FIXME: Two words types
        //                         }
        //                         if let Some(return_t) = &**return_t {
        //                             self.stack_push(return_t.clone())?; // FIXME: Two words types
        //                         }
        //                     }
        //                     _ => unreachable!(),
        //                 }
        //             }
        //             OP_RETURN | OP_IRETURN => {}
        //             OP_INEG => {}
        //             OP_LDC | OP_LDC_W => {
        //                 i += 1;
        //                 self.stack_push(Type::Long)?; // FIXME
        //             }
        //             OP_LCONST_0 | OP_LCONST_1 => {
        //                 self.stack_push(Type::Long)?;
        //                 self.stack_push(Type::Long)?;
        //             }
        //             OP_LDC2_W => {
        //                 i += 2;
        //                 self.stack_push(Type::Long)?; // FIXME
        //                 self.stack_push(Type::Long)?; // FIXME
        //             }
        //             OP_LADD | OP_LMUL | OP_LSUB | OP_LDIV => {
        //                 self.stack_pop2()?;
        //             }
        //             OP_DADD | OP_DMUL | OP_DSUB | OP_DDIV => {
        //                 self.stack_pop2()?;
        //             }
        //             _ => {
        //                 dbg!(op);
        //                 unimplemented!()
        //             }
        //         }
        //         i += 1;
        //     }

        //     self.stack_map_frame_add_full(*bci);
        // }
        Ok(())
    }

    fn push(
        &mut self,
        op: u8,
        operand1: Option<u8>,
        operand2: Option<u8>,
        t: Type,
    ) -> Result<(), Error> {
        debug!(
            "push: op={} operand1={:?} operand2={:?} t={:?} stack={:?} locals={:?}",
            op, operand1, operand2, t, self.stack, self.locals
        );

        self.code.push(op);
        self.opcode_types.push(t.clone());

        if let Some(operand1) = operand1 {
            self.code.push(operand1);
            self.opcode_types.push(t.clone());
        }
        if let Some(operand2) = operand2 {
            self.code.push(operand2);
            self.opcode_types.push(t.clone());
        }

        Ok(())
    }

    pub(crate) fn end(&mut self, jvm_emitter: &JvmEmitter) -> Result<Vec<u8>, Error> {
        self.verify(jvm_emitter)?;
        Ok(self.code.clone())
    }
}
