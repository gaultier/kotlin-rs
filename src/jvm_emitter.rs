use crate::error::*;
use crate::jvm_code_builder::{CodeBuilder, IfBuilder};
use crate::jvm_constants::*;
use crate::jvm_stack_map_frame::StackMapFrame;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::resolver::Resolution;
use crate::session::Session;
use log::debug;
use std::collections::BTreeMap;
use std::path::Path;

#[derive(Debug, Clone)]
pub(crate) enum Constant {
    Utf8(String),
    ClassInfo(u16),
    MethodRef(u16, u16),
    FieldRef(u16, u16),
    NameAndType(u16, u16),
    CString(u16),
    Int(i32),
    Float(f32),
    Long(i64),
    LongHigh(i32),
    LongLow(i32),
    Double(f64),
    DoubleHigh(u32),
    DoubleLow(u32),
}

#[derive(Debug)]
pub(crate) struct JvmEmitter<'a> {
    pub(crate) session: &'a Session<'a>,
    pub(crate) types: &'a Types,
    pub(crate) resolution: &'a Resolution,
    pub(crate) constants: Vec<Constant>,
    pub(crate) methods: Vec<Function>,
    pub(crate) attributes: Vec<Attribute>,
    pub(crate) source_file_constant: u16,
    pub(crate) source_file_name_constant: u16,
    pub(crate) ctor_str: u16,
    pub(crate) code_str: u16,
    pub(crate) line_table_str: u16,
    pub(crate) obj_str: u16,
    pub(crate) main_str: u16,
    pub(crate) main_descriptor_str: u16,
    pub(crate) super_class: u16,
    pub(crate) this_class: u16,
    pub(crate) obj_ctor_descriptor: u16,
    pub(crate) obj_method_ref: u16,
    pub(crate) out_fieldref: u16,
    pub(crate) println_str: u16,
    pub(crate) class_printstream: u16,
    pub(crate) stack_map_table_str: u16,
    fn_id_to_constant_pool_index: BTreeMap<NodeId, u16>,
    constant_pool_index_to_fn_id: BTreeMap<u16, NodeId>,
    class_name: String,
    class_main_args: u16,
}

#[derive(Debug)]
pub(crate) struct Class {
    pub(crate) ctor: Function,
    pub(crate) methods: Vec<Function>,
}

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) access_flags: u16,
    pub(crate) name: u16,
    pub(crate) descriptor: u16,
    pub(crate) attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub(crate) enum Attribute {
    SourceFile {
        name: u16,
        source_file: u16,
    },
    Code {
        name: u16,
        max_stack: u16,
        max_locals: u16,
        code: Vec<u8>,
        exception_table: Vec<Exception>,
        attributes: Vec<Attribute>,
    },
    LineNumberTable {
        name: u16,
        line_number_tables: Vec<LineNumberTable>,
    },
    StackMapTable {
        name: u16,
        entries: Vec<StackMapFrame>,
    },
}

#[derive(Debug)]
pub(crate) struct LineNumberTable {
    pub(crate) start_pc: u16,
    pub(crate) line_number: u16,
}

#[derive(Debug)]
pub(crate) struct Exception {
    pub(crate) start_pc: u16,
    pub(crate) end_pc: u16,
    pub(crate) handler_pc: u16,
    pub(crate) catch_type: u16,
}

fn binary_op(kind: &TokenKind, t: &Type) -> u8 {
    match (kind, t) {
        (TokenKind::Plus, Type::Int) => OP_IADD,
        (TokenKind::Star, Type::Int) => OP_IMUL,
        (TokenKind::Minus, Type::Int) => OP_ISUB,
        (TokenKind::Slash, Type::Int) => OP_IDIV,
        (TokenKind::Percent, Type::Int) => OP_IREM,

        (TokenKind::Plus, Type::Long) => OP_LADD,
        (TokenKind::Star, Type::Long) => OP_LMUL,
        (TokenKind::Minus, Type::Long) => OP_LSUB,
        (TokenKind::Slash, Type::Long) => OP_LDIV,
        (TokenKind::Percent, Type::Long) => OP_LREM,

        (TokenKind::Plus, Type::Float) => OP_FADD,
        (TokenKind::Star, Type::Float) => OP_FMUL,
        (TokenKind::Minus, Type::Float) => OP_FSUB,
        (TokenKind::Slash, Type::Float) => OP_FDIV,
        (TokenKind::Percent, Type::Float) => OP_FREM,

        (TokenKind::Plus, Type::Double) => OP_DADD,
        (TokenKind::Star, Type::Double) => OP_DMUL,
        (TokenKind::Minus, Type::Double) => OP_DSUB,
        (TokenKind::Slash, Type::Double) => OP_DDIV,
        (TokenKind::Percent, Type::Double) => OP_DREM,
        _ => {
            dbg!(kind);
            unreachable!()
        }
    }
}

fn add_constant(constants: &mut Vec<Constant>, constant: &Constant) -> Result<u16, Error> {
    // Since `constants` is one-indexed, the max index is `std::u16::MAX+1` but since we
    // can only index it with u16 the real max index is `std::u16::MAX`

    match constant {
        Constant::Long(n) if (constants.len() + 1) < std::u16::MAX as usize => {
            constants.push(Constant::LongHigh((*n >> 32) as i32));
            constants.push(Constant::LongLow((*n & 0xff_ff_ff_ff) as i32));
            Ok((constants.len()) as u16)
        }
        Constant::Double(n) if (constants.len() + 1) < std::u16::MAX as usize => {
            let bytes = n.to_be_bytes();
            constants.push(Constant::DoubleHigh(
                ((bytes[0] as u32) << 24)
                    + ((bytes[1] as u32) << 16)
                    + ((bytes[2] as u32) << 8)
                    + (bytes[3] as u32),
            ));
            constants.push(Constant::DoubleLow(
                ((bytes[4] as u32) << 24)
                    + ((bytes[5] as u32) << 16)
                    + ((bytes[6] as u32) << 8)
                    + (bytes[7] as u32),
            ));
            Ok((constants.len()) as u16)
        }
        _ if constants.len() < std::u16::MAX as usize => {
            constants.push(constant.clone());
            Ok(constants.len() as u16)
        }
        _ => Err(Error::new(
            ErrorKind::MaxConstantsReached(constants.len() as u16),
            Location::new(),
        )),
    }
}

fn add_and_push_constant(
    constants: &mut Vec<Constant>,
    constant: &Constant,
    code_builder: &mut CodeBuilder,
) -> Result<(), Error> {
    let i = add_constant(constants, &constant)?;
    debug!("added constant: constant={:?} i={}", &constant, i);

    match constant {
        Constant::Double(_) => {
            let bytes = ((i - 1) as u16).to_be_bytes();
            code_builder.push3(OP_LDC2_W, bytes[0], bytes[1], Type::Double)
        }
        Constant::Long(_) => {
            let bytes = ((i - 1) as u16).to_be_bytes();
            code_builder.push3(OP_LDC2_W, bytes[0], bytes[1], Type::Long)
        }
        Constant::Int(_) if i <= std::u8::MAX as u16 => {
            code_builder.push2(OP_LDC, i as u8, Type::Int)
        }
        Constant::Float(_) if i <= std::u8::MAX as u16 => {
            code_builder.push2(OP_LDC, i as u8, Type::Float)
        }
        _ => {
            let bytes = ((i - 1) as u16).to_be_bytes();
            code_builder.push3(OP_LDC_W, bytes[0], bytes[1], Type::Long) // FIXME
        }
    }
}

impl Type {
    fn to_jvm_string(&self) -> String {
        match self {
            Type::Boolean => String::from("Z"),
            Type::Int => String::from("I"),
            Type::Long => String::from("J"),
            Type::Float => String::from("F"),
            Type::Double => String::from("D"),
            Type::Char => String::from("C"),
            Type::Unit => String::from("V"),
            Type::TString => String::from("Ljava/lang/String;"),
            Type::Function { args, return_t } => {
                let args = args
                    .iter()
                    .map(|a| a.to_jvm_string())
                    .collect::<Vec<_>>()
                    .concat();
                format!(
                    "({}){}",
                    args,
                    return_t.clone().unwrap_or(Type::Unit).to_jvm_string()
                )
            }
            Type::Object { class, .. } => class.clone(),
            _ => unimplemented!(),
        }
    }
}

impl<'a> JvmEmitter<'a> {
    pub(crate) fn new(
        session: &'a Session,
        types: &'a Types,
        resolution: &'a Resolution,
        file_name: &Path,
        class_name: &str,
    ) -> JvmEmitter<'a> {
        let mut constants = Vec::new();

        let stack_map_table_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("StackMapTable")),
        )
        .unwrap();

        let obj_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("java/lang/Object")),
        )
        .unwrap();
        let obj_ctor_descriptor =
            add_constant(&mut constants, &Constant::Utf8(String::from("()V"))).unwrap();

        let ctor_str =
            add_constant(&mut constants, &Constant::Utf8(String::from(CTOR_STR))).unwrap();

        let obj_name_type = add_constant(
            &mut constants,
            &Constant::NameAndType(ctor_str, obj_ctor_descriptor),
        )
        .unwrap();

        let super_class = add_constant(&mut constants, &Constant::ClassInfo(obj_str)).unwrap();

        let obj_method_ref = add_constant(
            &mut constants,
            &Constant::MethodRef(super_class, obj_name_type),
        )
        .unwrap();
        let this_class_name =
            add_constant(&mut constants, &Constant::Utf8(class_name.to_string())).unwrap();

        let this_class =
            add_constant(&mut constants, &Constant::ClassInfo(this_class_name)).unwrap();

        let code_str = add_constant(&mut constants, &Constant::Utf8(String::from("Code"))).unwrap();

        let line_table_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("LineNumberTable")),
        )
        .unwrap();

        let main_str = add_constant(&mut constants, &Constant::Utf8(String::from("main"))).unwrap();

        let main_descriptor_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("([Ljava/lang/String;)V")),
        )
        .unwrap();

        let source_file_constant =
            add_constant(&mut constants, &Constant::Utf8(String::from("SourceFile"))).unwrap();
        let source_file_name_constant = add_constant(
            &mut constants,
            &Constant::Utf8(file_name.to_string_lossy().to_string()),
        )
        .unwrap();

        let class_system_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("java/lang/System")),
        )
        .unwrap();
        let class_system =
            add_constant(&mut constants, &Constant::ClassInfo(class_system_str)).unwrap();

        let out_str = add_constant(&mut constants, &Constant::Utf8(String::from("out"))).unwrap();

        let println_str =
            add_constant(&mut constants, &Constant::Utf8(String::from("println"))).unwrap();

        let printstream_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("java/io/PrintStream")),
        )
        .unwrap();

        let printstream_str_type = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("Ljava/io/PrintStream;")),
        )
        .unwrap();
        let out_name_type = add_constant(
            &mut constants,
            &Constant::NameAndType(out_str, printstream_str_type),
        )
        .unwrap();

        let out_fieldref = add_constant(
            &mut constants,
            &Constant::FieldRef(class_system, out_name_type),
        )
        .unwrap();

        let class_printstream =
            add_constant(&mut constants, &Constant::ClassInfo(printstream_str)).unwrap();

        let main_args_str = add_constant(
            &mut constants,
            &Constant::Utf8(String::from("[Ljava/lang/String;")),
        )
        .unwrap();
        let class_main_args =
            add_constant(&mut constants, &Constant::ClassInfo(main_args_str)).unwrap();

        JvmEmitter {
            session,
            types,
            resolution,
            constants,
            source_file_constant,
            source_file_name_constant,
            code_str,
            line_table_str,
            ctor_str,
            obj_str,
            main_str,
            main_descriptor_str,
            super_class,
            this_class,
            obj_ctor_descriptor,
            obj_method_ref,
            out_fieldref,
            println_str,
            class_printstream,
            stack_map_table_str,
            methods: Vec::new(),
            attributes: Vec::new(),
            fn_id_to_constant_pool_index: BTreeMap::new(),
            constant_pool_index_to_fn_id: BTreeMap::new(),
            class_name: class_name.to_string(),
            class_main_args,
        }
    }

    pub(crate) fn main<W: std::io::Write>(
        &mut self,
        block: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.methods = vec![Function {
            access_flags: 0,
            name: self.ctor_str,
            descriptor: self.obj_ctor_descriptor,
            attributes: vec![Attribute::Code {
                name: self.code_str,
                max_stack: 1,
                max_locals: 1,
                code: vec![
                    OP_ALOAD_0,
                    OP_INVOKE_SPECIAL,
                    self.obj_method_ref.to_be_bytes()[0],
                    self.obj_method_ref.to_be_bytes()[1],
                    OP_RETURN,
                ],
                exception_table: vec![],
                attributes: vec![Attribute::LineNumberTable {
                    // FIXME: dummy for now
                    name: self.line_table_str,
                    line_number_tables: vec![LineNumberTable {
                        start_pc: 0,
                        line_number: 1,
                    }],
                }],
            }],
        }];

        let mut f = Function {
            access_flags: METHOD_ACC_PUBLIC | METHOD_ACC_STATIC,
            name: self.main_str,
            descriptor: self.main_descriptor_str,
            attributes: Vec::new(),
        };

        let mut code_builder = CodeBuilder::new();
        code_builder.locals_push((
            0xdeadbeef,
            // FIXME: hardcoded
            Type::Object {
                class: String::from("[Ljava/lang/String;"),
                jvm_constant_pool_index: Some(self.class_main_args),
            },
        ))?;
        code_builder.starting_locals = code_builder.locals.clone();

        self.statement(block, &mut code_builder)?;
        code_builder.code.push(OP_RETURN);
        let code = code_builder.end(&self)?;

        // FIXME: dummy for now
        let line_table = Attribute::LineNumberTable {
            name: self.line_table_str,
            line_number_tables: vec![LineNumberTable {
                start_pc: 0,
                line_number: 2,
            }],
        };

        let stack_map_table = Attribute::StackMapTable {
            name: self.stack_map_table_str,
            entries: code_builder.stack_map_frames_compute_delta_offsets(),
        };

        let attribute_code = Attribute::Code {
            name: self.code_str,
            max_stack: code_builder.stack_max,
            max_locals: code_builder.locals_max,
            code,
            exception_table: Vec::new(),
            attributes: vec![line_table, stack_map_table],
        };

        f.attributes.push(attribute_code);

        self.methods.push(f);

        self.attributes = vec![Attribute::SourceFile {
            name: self.source_file_constant,
            source_file: self.source_file_name_constant,
        }];

        self.write(w)
    }

    fn while_stmt(
        &mut self,
        cond: &AstNodeExpr,
        body: &AstNodeStmt,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        let before_cond = code_builder.code.len();
        self.expr(cond, code_builder)?;

        code_builder.push3(OP_IFEQ, OP_IMPDEP1, OP_IMPDEP2, Type::Int)?;
        let end_if_jump = code_builder.code.len() - 1;

        self.statement(body, code_builder)?;
        code_builder.push3(OP_GOTO, OP_IMPDEP1, OP_IMPDEP2, Type::Int)?;
        let end_body = code_builder.code.len() - 1;

        let backwards_offset: i16 = 3 - 1 + -((end_body - before_cond) as i16);
        let bytes = backwards_offset.to_be_bytes();
        code_builder.code[end_body - 1] = bytes[0];
        code_builder.code[end_body] = bytes[1];

        let forwards_offset: i16 = 3 + (end_body - end_if_jump) as i16;
        debug!(
            "while: backwards_offset={} forwards_offset={}",
            backwards_offset, forwards_offset
        );
        let bytes = forwards_offset.to_be_bytes();
        code_builder.code[end_if_jump - 1] = bytes[0];
        code_builder.code[end_if_jump] = bytes[1];
        Ok(())
    }

    fn var_def(
        &mut self,
        id: NodeId,
        value: &AstNodeExpr,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        let t = self.types.get(&id).unwrap();

        self.expr(value, code_builder)?;
        let i = code_builder.locals_push((id, t.clone()))?;
        debug!("var_def: id={} i={} t={}", id, i, t);

        let op = match t {
            Type::Int => OP_ISTORE,
            Type::Float => OP_FSTORE,
            Type::Double => OP_DSTORE,
            Type::Long => OP_LSTORE,
            _ => todo!(),
        };

        code_builder.push2(op, i as u8, t.clone())
    }

    fn assign(
        &mut self,
        target: &AstNodeExpr,
        value: &AstNodeExpr,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        self.expr(value, code_builder)?;

        match target {
            AstNodeExpr::VarRef(_, id) => {
                let ref_id = self.resolution.get(&id).unwrap().node_ref_id;
                let (i, _) = code_builder.locals_find_by_id(ref_id).unwrap();
                let t = self.types.get(&id).unwrap();
                debug!("assign: id={} i={} t={} ref_id={}", id, i, t, ref_id);

                let op = match t {
                    Type::Int => OP_ISTORE,
                    Type::Float => OP_FSTORE,
                    Type::Double => OP_DSTORE,
                    Type::Long => OP_LSTORE,
                    _ => todo!(),
                };

                code_builder.push2(op, i as u8, t.clone())
            }
            _ => todo!(),
        }
    }

    fn var_ref(&mut self, id: NodeId, code_builder: &mut CodeBuilder) -> Result<(), Error> {
        let t = self.types.get(&id).unwrap();

        let ref_id = self.resolution.get(&id).unwrap().node_ref_id;
        let (i, _) = code_builder.locals_find_by_id(ref_id).unwrap();
        debug!("var_ref: id={} i={} t={} ref_id={}", id, i, t, ref_id);

        let op = match t {
            Type::Int => OP_ILOAD,
            Type::Float => OP_FLOAD,
            Type::Double => OP_DLOAD,
            Type::Long => OP_LLOAD,
            _ => todo!(),
        };
        code_builder.push2(op, i as u8, t.clone())
    }

    fn fn_def(
        &mut self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        id: NodeId,
        flags: u16,
        body: &AstNodeStmt,
    ) -> Result<(), Error> {
        let fn_name_s = match fn_name {
            AstNodeExpr::VarRef(span, _) => self.session.src[span.start..span.end].to_string(),
            _ => unreachable!(),
        };

        let name = add_constant(&mut self.constants, &Constant::Utf8(fn_name_s))?;

        let fn_t = self.types.get(&id).unwrap();
        let t_i = add_constant(&mut self.constants, &Constant::Utf8(fn_t.to_jvm_string()))?;
        let name_and_type = add_constant(&mut self.constants, &Constant::NameAndType(name, t_i))?;
        let method_ref = add_constant(
            &mut self.constants,
            &Constant::MethodRef(self.this_class, name_and_type),
        )?;
        self.fn_id_to_constant_pool_index.insert(id, method_ref);
        self.constant_pool_index_to_fn_id.insert(method_ref, id);

        let mut f = Function {
            access_flags: METHOD_ACC_STATIC, // FIXME: convert `flags`
            name,
            descriptor: t_i,
            attributes: Vec::new(),
        };

        let mut code_builder = CodeBuilder::new();
        for arg in args {
            let arg_id = arg.id();
            let arg_t = self.types.get(&arg_id).unwrap();
            code_builder.locals_push((arg_id, arg_t.clone()))?;
        }
        code_builder.starting_locals = code_builder.locals.clone();

        self.statement(body, &mut code_builder)?;
        let code = code_builder.end(&self)?;

        // FIXME: dummy for now
        let line_table = Attribute::LineNumberTable {
            name: self.line_table_str,
            line_number_tables: vec![LineNumberTable {
                start_pc: 0,
                line_number: 2,
            }],
        };

        let stack_map_table = Attribute::StackMapTable {
            name: self.stack_map_table_str,
            entries: code_builder.stack_map_frames_compute_delta_offsets(),
        };

        let attribute_code = Attribute::Code {
            name: self.code_str,
            max_stack: code_builder.stack_max,
            max_locals: code_builder.locals_max,
            code,
            exception_table: Vec::new(),
            attributes: vec![line_table, stack_map_table],
        };

        f.attributes.push(attribute_code);

        self.methods.push(f);
        Ok(())
    }

    pub(crate) fn statement(
        &mut self,
        statement: &AstNodeStmt,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        match statement {
            AstNodeStmt::Expr(e) => self.expr(e, code_builder),
            AstNodeStmt::Assign {
                op: TokenKind::Equal,
                target,
                value,
                ..
            } => self.assign(target, value, code_builder),
            // The MIR should have transformed other assignements e.g `+=` to simpler forms at this
            // point
            AstNodeStmt::Assign { .. } => unreachable!(),
            AstNodeStmt::VarDefinition { value, id, .. } => self.var_def(*id, value, code_builder),
            AstNodeStmt::Block { body, .. } => {
                for stmt in body {
                    self.statement(stmt, code_builder)?;
                }
                Ok(())
            }
            AstNodeStmt::While { cond, body, .. } => self.while_stmt(cond, body, code_builder),
            AstNodeStmt::FnDefinition {
                fn_name,
                args,
                id,
                flags,
                body,
                ..
            } => self.fn_def(fn_name, args, *id, *flags, body),
            _ => unimplemented!(),
        }
    }

    fn if_expr(
        &mut self,
        cond: &AstNodeExpr,
        if_body: &AstNodeStmt,
        else_body: &AstNodeStmt,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        self.expr(cond, code_builder)?;

        IfBuilder::new()
            .if_body(if_body, self, code_builder)?
            .else_body(else_body, self, code_builder)?
            .build(code_builder);

        Ok(())
    }

    fn println(&mut self, expr: &AstNodeExpr, code_builder: &mut CodeBuilder) -> Result<(), Error> {
        let expr_t = self.types.get(&expr.id()).unwrap();

        let println_str_type = add_constant(
            &mut self.constants,
            &Constant::Utf8(
                Type::Function {
                    args: vec![expr_t.clone()],
                    return_t: Box::new(Some(Type::Unit)),
                }
                .to_jvm_string(),
            ),
        )?;

        let println_name_type = add_constant(
            &mut self.constants,
            &Constant::NameAndType(self.println_str, println_str_type),
        )?;
        let println_methodref = add_constant(
            &mut self.constants,
            &Constant::MethodRef(self.class_printstream, println_name_type),
        )?;

        code_builder.push3(
            OP_GET_STATIC,
            self.out_fieldref.to_be_bytes()[0],
            self.out_fieldref.to_be_bytes()[1],
            // FIXME: for println
            Type::Object {
                class: String::from("java/io/PrintStream"),
                jvm_constant_pool_index: Some(self.class_printstream),
            },
        )?;
        self.expr(expr, code_builder)?;

        code_builder.push3(
            OP_INVOKE_VIRTUAL,
            println_methodref.to_be_bytes()[0],
            println_methodref.to_be_bytes()[1],
            Type::Unit, // FIXME: for println
        )
    }

    fn fn_call(
        &mut self,
        fn_name: &AstNodeExpr,
        args: &[AstNodeExpr],
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        let fn_id = self.resolution.get(&fn_name.id()).unwrap().node_ref_id;
        let i: u16 = *self.fn_id_to_constant_pool_index.get(&fn_id).unwrap();

        for arg in args {
            self.expr(arg, code_builder)?;
        }

        code_builder.push3(
            OP_INVOKE_STATIC,
            i.to_be_bytes()[0],
            i.to_be_bytes()[1],
            Type::Any, // FIXME
        )
    }

    fn return_expr(
        &mut self,
        expr: &Option<Box<AstNodeExpr>>,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        if let Some(expr) = expr {
            self.expr(expr, code_builder)?;
            let t = self.types.get(&expr.id()).unwrap();
            assert_eq!(t, &Type::Int);
            code_builder.push1(OP_IRETURN, Type::Int)
        } else {
            code_builder.push1(OP_RETURN, Type::Unit)
        }
    }

    pub(crate) fn expr(
        &mut self,
        expr: &AstNodeExpr,
        code_builder: &mut CodeBuilder,
    ) -> Result<(), Error> {
        match expr {
            AstNodeExpr::Literal(l, _) => self.literal(l, code_builder),
            AstNodeExpr::Unary { .. } => self.unary(expr, code_builder),
            AstNodeExpr::Binary { .. } => self.binary(expr, code_builder),
            AstNodeExpr::Grouping(e, _) => self.expr(e, code_builder),
            AstNodeExpr::Println(e, _) => self.println(e, code_builder),
            AstNodeExpr::VarRef(_, id) => self.var_ref(*id, code_builder),
            AstNodeExpr::Jump {
                kind: crate::parse::JumpKind::Return,
                expr,
                ..
            } => self.return_expr(expr, code_builder),
            AstNodeExpr::FnCall { fn_name, args, .. } => self.fn_call(fn_name, args, code_builder),
            AstNodeExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => self.if_expr(cond, if_body, else_body, code_builder),
            _ => unimplemented!(),
        }
    }

    fn binary(&mut self, expr: &AstNodeExpr, code_builder: &mut CodeBuilder) -> Result<(), Error> {
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

                self.expr(left, code_builder)?;

                self.expr(right, code_builder)?;

                match (op.kind, left_t, right_t) {
                    (TokenKind::PipePipe, _, _) => {
                        code_builder.push1(OP_IOR, Type::Int)?;
                    }
                    (TokenKind::AmpersandAmpersand, _, _) => {
                        code_builder.push1(OP_IAND, Type::Int)?;
                    }
                    (TokenKind::EqualEqual, Type::Long, Type::Long) => {
                        code_builder.push1(OP_LCMP, Type::Int)?;
                        code_builder.push3(OP_IFNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::EqualEqual, Type::Float, Type::Float) => {
                        code_builder.push1(OP_FCMPL, Type::Int)?;
                        code_builder.push3(OP_IFNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::EqualEqual, Type::Double, Type::Double) => {
                        code_builder.push1(OP_DCMPL, Type::Int)?;
                        code_builder.push3(OP_IFNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::EqualEqual, _, _) if left_t == right_t => {
                        code_builder.push3(OP_IF_ICMPNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }

                    (TokenKind::BangEqual, Type::Long, Type::Long) => {
                        code_builder.push1(OP_LCMP, Type::Int)?;
                        code_builder.push3(OP_IFNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                    }
                    (TokenKind::BangEqual, Type::Float, Type::Float) => {
                        code_builder.push1(OP_FCMPL, Type::Int)?;
                        code_builder.push3(OP_IFNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                    }
                    (TokenKind::BangEqual, Type::Double, Type::Double) => {
                        code_builder.push1(OP_DCMPL, Type::Int)?;
                        code_builder.push3(OP_IFNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                    }
                    (TokenKind::BangEqual, _, _) if left_t == right_t => {
                        code_builder.push3(OP_IF_ICMPNE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                    }

                    (TokenKind::Lesser, Type::Float, Type::Float) => {
                        code_builder.push1(OP_FCMPL, Type::Int)?;
                        code_builder.push3(OP_IFGE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::Lesser, Type::Double, Type::Double) => {
                        code_builder.push1(OP_DCMPL, Type::Int)?;
                        code_builder.push3(OP_IFGE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::Lesser, Type::Long, Type::Long) => {
                        code_builder.push1(OP_LCMP, Type::Int)?;
                        code_builder.push3(OP_IFGE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::Lesser, _, _) => {
                        code_builder.push3(OP_IF_ICMPGE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }

                    (TokenKind::Greater, Type::Float, Type::Float) => {
                        code_builder.push1(OP_FCMPL, Type::Int)?;
                        code_builder.push3(OP_IFLE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::Greater, Type::Double, Type::Double) => {
                        code_builder.push1(OP_DCMPL, Type::Int)?;
                        code_builder.push3(OP_IFLE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::Greater, Type::Long, Type::Long) => {
                        code_builder.push1(OP_LCMP, Type::Int)?;
                        code_builder.push3(OP_IFLE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::Greater, _, _) => {
                        code_builder.push3(OP_IF_ICMPLE, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }

                    (TokenKind::LesserEqual, Type::Float, Type::Float) => {
                        code_builder.push1(OP_FCMPL, Type::Int)?;
                        code_builder.push3(OP_IFGT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::LesserEqual, Type::Double, Type::Double) => {
                        code_builder.push1(OP_DCMPL, Type::Int)?;
                        code_builder.push3(OP_IFGT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::LesserEqual, Type::Long, Type::Long) => {
                        code_builder.push1(OP_LCMP, Type::Int)?;
                        code_builder.push3(OP_IFGT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::LesserEqual, _, _) => {
                        code_builder.push3(OP_IF_ICMPGT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }

                    (TokenKind::GreaterEqual, Type::Float, Type::Float) => {
                        code_builder.push1(OP_FCMPL, Type::Int)?;
                        code_builder.push3(OP_IFLT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::GreaterEqual, Type::Double, Type::Double) => {
                        code_builder.push1(OP_DCMPL, Type::Int)?;
                        code_builder.push3(OP_IFLT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::GreaterEqual, Type::Long, Type::Long) => {
                        code_builder.push1(OP_LCMP, Type::Int)?;
                        code_builder.push3(OP_IFLT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }
                    (TokenKind::GreaterEqual, _, _) => {
                        code_builder.push3(OP_IF_ICMPLT, 0x00, 0x07, Type::Int)?;
                        code_builder.push1(OP_ICONST_1, Type::Int)?;
                        code_builder.push3(OP_GOTO, 0x00, 0x04, Type::Nothing)?;
                        code_builder.push1(OP_ICONST_0, Type::Int)?;
                    }

                    (_, _, _) if left_t != right_t => {
                        dbg!(t);
                        unimplemented!("Conversion in equality check")
                    }
                    _ => {
                        code_builder.push1(binary_op(&op.kind, t), t.clone())?;
                    }
                }
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn unary(&mut self, expr: &AstNodeExpr, code_builder: &mut CodeBuilder) -> Result<(), Error> {
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
                self.expr(expr, code_builder)?;
                code_builder.push1(OP_INEG, Type::Int)
            }
            AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Plus,
                        ..
                    },
                expr,
                ..
            } => self.expr(expr, code_builder),
            _ => unimplemented!(),
        }
    }

    fn literal(&mut self, literal: &Token, code_builder: &mut CodeBuilder) -> Result<(), Error> {
        match literal.kind {
            TokenKind::Int(-1) => code_builder.push1(OP_ICONST_M1, Type::Int),
            TokenKind::Boolean(false) | TokenKind::Int(0) => {
                code_builder.push1(OP_ICONST_0, Type::Int)
            }
            TokenKind::Boolean(true) | TokenKind::Int(1) => {
                code_builder.push1(OP_ICONST_1, Type::Int)
            }
            TokenKind::Int(2) => code_builder.push1(OP_ICONST_2, Type::Int),
            TokenKind::Int(3) => code_builder.push1(OP_ICONST_3, Type::Int),
            TokenKind::Int(4) => code_builder.push1(OP_ICONST_4, Type::Int),
            TokenKind::Int(5) => code_builder.push1(OP_ICONST_5, Type::Int),
            TokenKind::Int(n) if n <= std::i8::MAX as i32 => {
                code_builder.push2(OP_BIPUSH, n as u8, Type::Int)
            }
            TokenKind::Int(n) if n <= std::i16::MAX as i32 => {
                let bytes = (n as u16).to_be_bytes();
                code_builder.push3(OP_SIPUSH, bytes[0], bytes[1], Type::Int)
            }
            TokenKind::Int(n) => {
                add_and_push_constant(&mut self.constants, &Constant::Int(n), code_builder)
            }
            TokenKind::Char(c) if c as u16 as i32 <= std::i8::MAX as i32 => {
                code_builder.push2(OP_BIPUSH, c as u8, Type::Char)
            }
            TokenKind::Char(c) if c as u16 as i32 <= std::i16::MAX as i32 => {
                let bytes = (c as u16).to_be_bytes();
                code_builder.push3(OP_SIPUSH, bytes[0], bytes[1], Type::Char)
            }
            TokenKind::Long(0) => code_builder.push1(OP_LCONST_0, Type::Int),
            TokenKind::Long(1) => code_builder.push1(OP_LCONST_1, Type::Int),
            TokenKind::Long(n) => {
                add_and_push_constant(&mut self.constants, &Constant::Long(n), code_builder)
            }
            TokenKind::Double(n) if n.to_bits() == 0f64.to_bits() => {
                code_builder.push1(OP_DCONST_0, Type::Int)
            }
            TokenKind::Double(n) if n.to_bits() == 1f64.to_bits() => {
                code_builder.push1(OP_DCONST_1, Type::Int)
            }
            TokenKind::Double(n) => {
                add_and_push_constant(&mut self.constants, &Constant::Double(n), code_builder)
            }
            TokenKind::Float(n) if n.to_bits() == 0f32.to_bits() => {
                code_builder.push1(OP_FCONST_0, Type::Float)
            }
            TokenKind::Float(n) if n.to_bits() == 1f32.to_bits() => {
                code_builder.push1(OP_FCONST_1, Type::Float)
            }
            TokenKind::Float(n) if n.to_bits() == 2f32.to_bits() => {
                code_builder.push1(OP_FCONST_2, Type::Float)
            }
            TokenKind::Float(n) => {
                add_and_push_constant(&mut self.constants, &Constant::Float(n), code_builder)
            }
            TokenKind::TString => {
                let s = String::from(&self.session.src[literal.span.start..literal.span.end]);
                let i = add_constant(&mut self.constants, &Constant::Utf8(s))?;
                add_and_push_constant(&mut self.constants, &Constant::CString(i), code_builder)
            }
            _ => {
                dbg!(literal);
                unimplemented!()
            }
        }
    }
}
