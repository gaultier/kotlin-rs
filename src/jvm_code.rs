use crate::error::*;
use crate::jvm_constants::*;
use crate::jvm_emitter::{Attribute, JvmEmitter};
use crate::jvm_locals::Locals;
use crate::jvm_stack_map_frame::StackMapFrame;
use crate::jvm_state::State;
use crate::parse::*;
use log::debug;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub(crate) enum JumpTarget {
    If {
        if_location: u16,
        if_target: u16,
        goto_location: u16,
        goto_target: u16,
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

    pub(crate) fn simple_expr(
        if_opcode: u8,
        if_body_value: u8,
        else_body_value: u8,
        t: Type,
        jvm_emitter: &mut JvmEmitter,
        code: &mut Code,
    ) -> Result<(), Error> {
        let mut builder = IfBuilder::new();

        code.push3(
            if_opcode,
            OP_IMPDEP1,
            OP_IMPDEP2,
            Type::Nothing,
            &jvm_emitter.constant_pool_index_to_fn_id,
            &jvm_emitter.types,
        )?;
        builder.if_location = Some((code.code.len() - 3) as u16);

        code.push1(if_body_value, t.clone())?;
        code.push3(
            OP_GOTO,
            OP_IMPDEP1,
            OP_IMPDEP2,
            Type::Nothing,
            &jvm_emitter.constant_pool_index_to_fn_id,
            &jvm_emitter.types,
        )?;

        let end_if_body = (code.code.len() - 1) as u16;
        builder.goto_location = Some(end_if_body - 2);

        let start_else_offset: u16 = (1 + end_if_body - builder.if_location.unwrap()) as u16;
        code.code[builder.if_location.unwrap() as usize + 1] = start_else_offset.to_be_bytes()[0];
        code.code[builder.if_location.unwrap() as usize + 2] = start_else_offset.to_be_bytes()[1];

        builder.if_target = Some(1 + end_if_body);

        code.push1(else_body_value, t)?;

        let end = (code.code.len() - 1) as u16;

        let start_rest_offset: u16 = 1 + end - builder.goto_location.unwrap();
        code.code[builder.goto_location.unwrap() as usize + 1] = start_rest_offset.to_be_bytes()[0];
        code.code[builder.goto_location.unwrap() as usize + 2] = start_rest_offset.to_be_bytes()[1];

        builder.goto_target = Some(1 + end);

        code.jump_target_at(
            builder.if_location.unwrap(),
            JumpTarget::If {
                if_location: builder.if_location.unwrap(),
                if_target: builder.if_target.unwrap(),
                goto_location: builder.goto_location.unwrap(),
                goto_target: builder.goto_target.unwrap(),
            },
        );

        Ok(())
    }

    pub(crate) fn if_body(
        mut self,
        if_body: &AstNodeStmt,
        jvm_emitter: &mut JvmEmitter,
        code: &mut Code,
    ) -> Result<Self, Error> {
        code.push3(
            OP_IFEQ,
            OP_IMPDEP1,
            OP_IMPDEP2,
            Type::Nothing,
            &jvm_emitter.constant_pool_index_to_fn_id,
            &jvm_emitter.types,
        )?;
        self.if_location = Some((code.code.len() - 3) as u16);

        jvm_emitter.statement(if_body, code)?;
        code.push3(
            OP_GOTO,
            OP_IMPDEP1,
            OP_IMPDEP2,
            Type::Nothing,
            &jvm_emitter.constant_pool_index_to_fn_id,
            &jvm_emitter.types,
        )?;

        let end_if_body = (code.code.len() - 1) as u16;
        self.goto_location = Some(end_if_body - 2);

        let start_else_offset: u16 = (1 + end_if_body - self.if_location.unwrap()) as u16;
        code.code[self.if_location.unwrap() as usize + 1] = start_else_offset.to_be_bytes()[0];
        code.code[self.if_location.unwrap() as usize + 2] = start_else_offset.to_be_bytes()[1];

        self.if_target = Some(1 + end_if_body);

        debug!(
            "if_builder::if_body: if_location={} goto_location={} if_target={} end_if_body={} offset={}",
            self.if_location.unwrap(),
            self.goto_location.unwrap(),
            self.if_target.unwrap(),
            end_if_body,
            start_else_offset,
        );

        Ok(self)
    }

    pub(crate) fn else_body(
        mut self,
        else_body: &AstNodeStmt,
        jvm_emitter: &mut JvmEmitter,
        code: &mut Code,
    ) -> Result<Self, Error> {
        jvm_emitter.statement(else_body, code)?;

        let end = (code.code.len() - 1) as u16;

        let start_rest_offset: u16 = 1 + end - self.goto_location.unwrap();
        code.code[self.goto_location.unwrap() as usize + 1] = start_rest_offset.to_be_bytes()[0];
        code.code[self.goto_location.unwrap() as usize + 2] = start_rest_offset.to_be_bytes()[1];

        self.goto_target = Some(1 + end);

        Ok(self)
    }

    pub(crate) fn build(self, code: &mut Code) -> Self {
        code.jump_target_at(
            self.if_location.unwrap(),
            JumpTarget::If {
                if_location: self.if_location.unwrap(),
                if_target: self.if_target.unwrap(),
                goto_location: self.goto_location.unwrap(),
                goto_target: self.goto_target.unwrap(),
            },
        );

        self
    }
}

#[derive(Debug)]
pub(crate) struct Code {
    pub(crate) code: Vec<u8>,
    pub(crate) attributes: Vec<Attribute>,
    pub(crate) state: State, // FIXME: branching
    pub(crate) stack_max: u16,
    pub(crate) args_locals: Locals,
    pub(crate) locals_max: u16,
    pub(crate) stack_map_frames: BTreeMap<u16, StackMapFrame>,
    pub(crate) opcode_types: Vec<Type>,
    pub(crate) jump_targets: BTreeMap<u16, JumpTarget>,
}

impl Code {
    pub(crate) fn new() -> Code {
        Code {
            code: Vec::new(),
            attributes: Vec::new(),
            state: State::new(),
            stack_max: 0,
            args_locals: Locals::new(),
            locals_max: 0,
            stack_map_frames: BTreeMap::new(),
            opcode_types: Vec::new(),
            jump_targets: BTreeMap::new(),
        }
    }

    fn stack_map_frame_add_full(&mut self, bci_target: u16) {
        debug!("stack_map_frame_add_full: bci_target={}", bci_target);

        // TODO: check overflow
        self.stack_map_frames.insert(
            bci_target,
            StackMapFrame::Full {
                offset: 0, // Will be computed in a final step
                stack: self.state.stack.to_verification_info(),
                locals: self.state.locals.to_verification_info(),
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
        self.push(op, None, None, t.clone())?;

        match op {
            OP_ICONST_M1 | OP_ICONST_0 | OP_ICONST_1 | OP_ICONST_2 | OP_ICONST_3 | OP_ICONST_4
            | OP_ICONST_5 => {
                self.state.stack.push(Type::Int);
            }
            OP_FCONST_0 | OP_FCONST_1 | OP_FCONST_2 => {
                self.state.stack.push(Type::Float);
            }
            OP_IADD | OP_IMUL | OP_ISUB | OP_IDIV | OP_IREM | OP_IAND | OP_IOR | OP_FADD
            | OP_FMUL | OP_FSUB | OP_FDIV | OP_FREM => {
                self.state.stack.pop();
            }
            OP_FCMPL => {
                self.state.stack.pop2();
                self.state.stack.push(Type::Int);
            }
            OP_RETURN | OP_IRETURN => {}
            OP_INEG => {}
            OP_LCONST_0 | OP_LCONST_1 => {
                self.state.stack.push(Type::Long); // FIXME: top
                self.state.stack.push(Type::Long);
            }
            OP_DCONST_0 | OP_DCONST_1 => {
                self.state.stack.push(Type::Double); // FIXME: top
                self.state.stack.push(Type::Double);
            }
            OP_LADD | OP_LMUL | OP_LSUB | OP_LDIV => {
                self.state.stack.pop2();
            }
            OP_DADD | OP_DMUL | OP_DSUB | OP_DDIV => {
                self.state.stack.pop2();
            }
            OP_LCMP => {
                self.state.stack.pop2();
                self.state.stack.pop2();
                self.state.stack.push(Type::Int);
            }
            OP_DCMPL => {
                self.state.stack.pop2();
                self.state.stack.pop2();
                self.state.stack.push(Type::Int);
            }
            OP_FSTORE_0 | OP_ASTORE_0 | OP_ISTORE_0 => {
                let t = self.state.stack.pop();
                self.state.locals.insert(0, (0, t.clone()));
            }
            OP_FSTORE_1 | OP_ASTORE_1 | OP_ISTORE_1 => {
                let t = self.state.stack.pop();
                self.state.locals.insert(1, (0, t.clone()));
            }
            OP_FLOAD_0 | OP_ALOAD_0 | OP_ILOAD_0 => {
                let (_, t) = &self.state.locals.at(0);

                self.state.stack.push(t.clone());
            }
            OP_FLOAD_1 | OP_ALOAD_1 | OP_ILOAD_1 => {
                let (_, t) = &self.state.locals.at(1);

                self.state.stack.push(t.clone());
            }
            OP_DSTORE_0 | OP_LSTORE_0 => {
                self.state.stack.pop2();
                self.state.locals.insert(0, (0, t.clone()));
            }
            OP_DSTORE_1 | OP_LSTORE_1 => {
                self.state.stack.pop2();
                self.state.locals.insert(1, (0, t.clone()));
            }
            OP_DLOAD_0 | OP_LLOAD_0 => {
                self.state.stack.push(t.clone()); // FIXME: top
                self.state.stack.push(t.clone());
            }
            OP_DLOAD_1 | OP_LLOAD_1 => {
                self.state.stack.push(t.clone()); // FIXME: top
                self.state.stack.push(t.clone());
            }
            _ => {
                dbg!(op);
                unimplemented!()
            }
        }
        Ok(())
    }

    pub(crate) fn push2(&mut self, op: u8, operand1: u8, t: Type) -> Result<(), Error> {
        self.push(op, Some(operand1), None, t.clone())?;

        match op {
            OP_BIPUSH => {
                self.state.stack.push(Type::Int);
            }
            OP_ISTORE => {
                self.state.locals.push((0xbeef, Type::Int));
                self.state.stack.pop();
            }
            OP_FSTORE => {
                self.state.locals.push((0xbeef, Type::Float));
                self.state.stack.pop();
            }
            OP_LSTORE => {
                todo!();
            }
            OP_DSTORE => {
                todo!();
            }
            OP_ILOAD => {
                self.state.stack.push(Type::Int);
            }
            OP_FLOAD => {
                self.state.stack.push(Type::Float);
            }
            OP_LLOAD => {
                self.state.stack.push(Type::Long); // FIXME: top
                self.state.stack.push(Type::Long);
            }
            OP_LDC | OP_LDC_W => {
                self.state.stack.push(t.clone());
            }
            _ => {
                dbg!(op);
                unimplemented!()
            }
        }
        Ok(())
    }

    pub(crate) fn push3(
        &mut self,
        op: u8,
        operand1: u8,
        operand2: u8,
        t: Type,
        constant_pool_index_to_fn_id: &BTreeMap<u16, NodeId>,
        types: &Types,
    ) -> Result<(), Error> {
        self.push(op, Some(operand1), Some(operand2), t)?;

        match op {
            OP_SIPUSH => {
                self.state.stack.push(Type::Int);
            }
            OP_IFEQ | OP_IFNE | OP_IFGE | OP_IFGT | OP_IFLE | OP_IFLT => {}
            OP_IF_ICMPEQ | OP_IF_ICMPNE | OP_IF_ICMPGE | OP_IF_ICMPLE | OP_IF_ICMPGT
            | OP_IF_ICMPLT => {}
            OP_GOTO => {}
            OP_GET_STATIC => {
                let t = &self.opcode_types.last().unwrap();
                let jvm_constant_pool_index = match t {
                    Type::Object {
                        jvm_constant_pool_index,
                        ..
                    } => jvm_constant_pool_index.unwrap(),
                    _ => unreachable!(),
                };

                // FIXME: hardcoded for println
                self.state.stack.push(Type::Object {
                    class: String::from("java/io/PrintStream"),
                    jvm_constant_pool_index: Some(jvm_constant_pool_index),
                });
            }
            OP_INVOKE_VIRTUAL => {
                self.state.stack.pop2(); // FIXME: hardcoded for println
            }
            OP_INVOKE_STATIC | OP_INVOKE_SPECIAL => {
                let fn_i: u16 = u16::from_be_bytes([operand1, operand2]);
                // The constant pool is one-indexed
                let fn_id: NodeId = *constant_pool_index_to_fn_id.get(&fn_i).unwrap();
                let fn_t = types.get(&fn_id).unwrap();
                let return_t = fn_t.fn_return_t();
                debug!("verify: op={} return_t={:?}", op, return_t);

                match fn_t {
                    Type::Function { return_t, args, .. } => {
                        for _ in 0..args.len() {
                            self.state.stack.pop(); // FIXME: Two words types
                        }
                        if let Some(return_t) = &**return_t {
                            self.state.stack.push(return_t.clone()); // FIXME: Two words types
                        }
                    }
                    _ => unreachable!(),
                }
            }
            OP_LDC_W => {
                let t = self.opcode_types.last().unwrap();
                self.state.stack.push(t.clone());
                self.state.stack.push(t.clone());
            }
            OP_LDC2_W => {
                self.state.stack.push(Type::Long); // FIXME
                self.state.stack.push(Type::Long); // FIXME
            }
            _ => {
                dbg!(op);
                unimplemented!()
            }
        }
        Ok(())
    }

    fn jump_target_at(&mut self, location: u16, jump_target: JumpTarget) {
        self.jump_targets.insert(location, jump_target);
    }

    fn generate_stack_map_frames(&mut self, jvm_emitter: &JvmEmitter) -> Result<(), Error> {
        // debug!("verify: jump_targets={:?}", &self.jump_targets);

        // let mut init_stack: Stack = Stack::new();
        // let mut init_locals: Locals = locals.clone();

        // let mut i = 0;

        // let stack_max = stack.count_max();
        // let locals_max = locals.count_max();
        // Ok((stack_max, locals_max))
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
            op, operand1, operand2, t, self.state.stack, self.state.locals
        );

        self.code.push(op);
        self.opcode_types.push(t.clone());

        if let Some(operand1) = operand1 {
            self.code.push(operand1);
            self.opcode_types.push(t.clone());
        }
        if let Some(operand2) = operand2 {
            self.code.push(operand2);
            self.opcode_types.push(t);
        }

        Ok(())
    }

    pub(crate) fn end(&mut self, jvm_emitter: &JvmEmitter) -> Result<Vec<u8>, Error> {
        // let (stack_max, locals_max) =
        //     self.generate_stack_map_frames(jvm_emitter, Stack::new(), self.args_locals.clone())?;
        self.stack_max = 100; // FIXME
        self.locals_max = 100; // FIXME
        Ok(self.code.clone())
    }

    pub(crate) fn spill1(&mut self) -> Result<(), Error> {
        let t = self.state.stack.iter().last().unwrap().clone();
        match t {
            Type::Char | Type::Boolean | Type::Int => self.push1(OP_ISTORE_1, t.clone()),
            Type::TString => self.push1(OP_ASTORE_1, t),
            Type::Float => self.push1(OP_FSTORE_1, t),
            Type::Long => self.push1(OP_LSTORE_1, Type::Long),
            Type::Double => self.push1(OP_DSTORE_1, Type::Double),
            _ => {
                dbg!(t);
                unimplemented!()
            }
        }
    }

    pub(crate) fn unspill1(&mut self) -> Result<(), Error> {
        let (_, t) = self.state.locals.at(1);
        match t {
            Type::Char | Type::Boolean | Type::Int => self.push1(OP_ILOAD_1, t.clone()),
            Type::TString => self.push1(OP_ALOAD_1, t.clone()),
            Type::Float => self.push1(OP_FLOAD_1, t.clone()),
            Type::Long => self.push1(OP_LLOAD_1, t.clone()),
            Type::Double => self.push1(OP_DLOAD_1, t.clone()),
            _ => {
                dbg!(t);
                unimplemented!()
            }
        }
    }
}
