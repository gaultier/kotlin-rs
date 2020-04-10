use crate::error::*;
use crate::jvm_constants::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::Session;
use log::debug;

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
}

#[derive(Debug)]
pub(crate) struct JvmEmitter<'a> {
    pub(crate) session: &'a Session<'a>,
    pub(crate) types: &'a Types,
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
pub(crate) enum VerificationTypeInfo {
    Int,
}

#[derive(Debug)]
pub(crate) enum StackMapFrame {
    SameFrame {
        offset: u8,
    },
    SameLocalsOneStackItemFrame {
        offset: u8,
        stack: VerificationTypeInfo,
    },
    FullFrame {
        offset: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    }, // More to come
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

fn add_constant(constants: &mut Vec<Constant>, constant: &Constant) -> Result<u16, Error> {
    // Since `constants` is one-indexed, the max index is `std::u16::MAX+1` but since we
    // can only index it with u16 the real max index is `std::u16::MAX`

    match constant {
        Constant::Long(n) if (constants.len() + 1) < std::u16::MAX as usize => {
            constants.push(Constant::LongHigh((*n >> 32) as i32));
            constants.push(Constant::LongLow((*n & 0xff_ff_ff_ff) as i32));
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
) -> Result<Vec<u8>, Error> {
    let i = add_constant(constants, &constant)?;
    debug!("added constant: constant={:?} i={}", &constant, i);

    match constant {
        Constant::Long(_) => Ok(vec![
            OP_LDC2_W,
            ((i - 1) >> 8) as u8,
            ((i - 1) & 0xff) as u8,
        ]),
        _ if i <= std::u8::MAX as u16 => Ok(vec![OP_LDC, i as u8]),
        _ => Ok(vec![OP_LDC_W, (i >> 8) as u8, (i & 0xff) as u8]),
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
            _ => unimplemented!(),
        }
    }
}

fn binary_op(kind: &TokenKind, t: &Type) -> u8 {
    match (kind, t) {
        (TokenKind::Plus, Type::Int) => OP_IADD,
        (TokenKind::Star, Type::Int) => OP_IMUL,
        (TokenKind::Minus, Type::Int) => OP_ISUB,
        (TokenKind::Slash, Type::Int) => OP_IDIV,
        (TokenKind::Plus, Type::Long) => OP_LADD,
        (TokenKind::Star, Type::Long) => OP_LMUL,
        (TokenKind::Minus, Type::Long) => OP_LSUB,
        (TokenKind::Slash, Type::Long) => OP_LDIV,
        (TokenKind::Plus, Type::Float) => OP_FADD,
        (TokenKind::Star, Type::Float) => OP_FMUL,
        (TokenKind::Minus, Type::Float) => OP_FSUB,
        (TokenKind::Slash, Type::Float) => OP_FDIV,
        // TokenKind::Percent => "%",
        // TokenKind::DotDot => "range",
        // TokenKind::EqualEqual => "==",
        // TokenKind::EqualEqualEqual => "===",
        // TokenKind::PipePipe => "or",
        // TokenKind::AmpersandAmpersand => "and",
        // TokenKind::Lesser => "<",
        // TokenKind::LesserEqual => "<=",
        // TokenKind::Greater => ">",
        // TokenKind::GreaterEqual => ">=",
        _ => {
            dbg!(kind);
            unreachable!()
        }
    }
}

impl<'a> JvmEmitter<'a> {
    pub(crate) fn new(session: &'a Session, types: &'a Types) -> JvmEmitter<'a> {
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
            add_constant(&mut constants, &Constant::Utf8(String::from("Foo"))).unwrap();

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
        let source_file_name_constant =
            add_constant(&mut constants, &Constant::Utf8(String::from("Foo.java"))).unwrap();

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

        JvmEmitter {
            session,
            types,
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
                    // FIXME
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

        let mut code = self.statement(block)?;
        code.push(OP_RETURN);

        let line_table = Attribute::LineNumberTable {
            name: self.line_table_str,
            line_number_tables: vec![LineNumberTable {
                start_pc: 0,
                line_number: 2,
            }],
        };

        let stack_map_table = Attribute::StackMapTable {
            name: self.stack_map_table_str,
            entries: vec![
                StackMapFrame::SameFrame { offset: 9 }, // FIXME
                StackMapFrame::SameLocalsOneStackItemFrame {
                    offset: 2, // FIXME
                    stack: VerificationTypeInfo::Int,
                },
            ],
        };

        let attribute_code = Attribute::Code {
            name: self.code_str,
            max_stack: 100, // FIXME
            max_locals: 2,  // FIXME
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

    fn statement(&mut self, statement: &AstNodeStmt) -> Result<Vec<u8>, Error> {
        match statement {
            AstNodeStmt::Expr(e) => self.expr(e),
            AstNodeStmt::Block { body, .. } => Ok(body
                .iter()
                .map(|stmt| self.statement(stmt))
                .collect::<Result<Vec<_>, Error>>()?
                .concat()),
            _ => unimplemented!(),
        }
    }

    fn if_expr(
        &mut self,
        cond: &AstNodeExpr,
        if_body: &AstNodeStmt,
        else_body: &AstNodeStmt,
    ) -> Result<Vec<u8>, Error> {
        let mut v = self.expr(cond)?;
        v.push(OP_IFEQ);
        v.push(OP_IMPDEP1); // Will be backpatched
        v.push(OP_IMPDEP2); // Will be backpatched
        let end_cond = v.len() - 1;

        v.append(&mut self.statement(if_body)?);
        v.push(OP_GOTO);
        v.push(OP_IMPDEP1); // Will be backpatched
        v.push(OP_IMPDEP2); // Will be backpatched
        let end_if_body = v.len() - 1;

        v.append(&mut self.statement(else_body)?);

        let end = v.len() - 1;

        let start_else_offset: u16 = (3 + end_if_body - end_cond) as u16;
        v[end_cond - 1] = start_else_offset.to_be_bytes()[0];
        v[end_cond] = start_else_offset.to_be_bytes()[1];

        let start_rest_offset: u16 = (3 + end - end_if_body) as u16;
        v[end_if_body - 1] = start_rest_offset.to_be_bytes()[0];
        v[end_if_body] = start_rest_offset.to_be_bytes()[1];

        // self.register_jump(end_if_body +1);
        // self.register_jump(end +1);

        debug!(
            "if_expr: end_cond={} end_if_body={} end={} start_else_offset={} start_rest_offset={}",
            end_cond, end_if_body, end, start_else_offset, start_rest_offset
        );

        Ok(v)
    }

    fn println(&mut self, expr: &AstNodeExpr) -> Result<Vec<u8>, Error> {
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

        let mut v = vec![];
        v.append(&mut self.expr(expr)?);
        // Workaround due to byte code validation: we empty the stack using the register 0, and we
        // do the opposite once we have loaded the println reference
        v.append(&mut vec![
            OP_ISTORE_0, // FIXME
            OP_GET_STATIC,
            self.out_fieldref.to_be_bytes()[0],
            self.out_fieldref.to_be_bytes()[1],
            OP_ILOAD_0, // FIXME
            OP_INVOKE_VIRTUAL,
            println_methodref.to_be_bytes()[0],
            println_methodref.to_be_bytes()[1],
        ]);
        Ok(v)
    }

    fn expr(&mut self, expr: &AstNodeExpr) -> Result<Vec<u8>, Error> {
        match expr {
            AstNodeExpr::Literal(l, _) => self.literal(l),
            AstNodeExpr::Unary { .. } => self.unary(expr),
            AstNodeExpr::Binary { .. } => self.binary(expr),
            AstNodeExpr::Grouping(e, _) => self.expr(e),
            AstNodeExpr::Println(e, _) => self.println(e),
            AstNodeExpr::IfExpr {
                cond,
                if_body,
                else_body,
                ..
            } => self.if_expr(cond, if_body, else_body),
            _ => unimplemented!(),
        }
    }

    fn binary(&mut self, expr: &AstNodeExpr) -> Result<Vec<u8>, Error> {
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

                let mut v = self.expr(left)?;

                v.append(&mut self.expr(right)?);

                v.push(binary_op(&op.kind, t));
                Ok(v)
            }
            _ => unimplemented!(),
        }
    }

    fn unary(&mut self, expr: &AstNodeExpr) -> Result<Vec<u8>, Error> {
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
                let mut v = self.expr(expr)?;
                v.push(OP_INEG);
                Ok(v)
            }
            AstNodeExpr::Unary {
                token:
                    Token {
                        kind: TokenKind::Plus,
                        ..
                    },
                expr,
                ..
            } => self.expr(expr),
            _ => unimplemented!(),
        }
    }

    fn literal(&mut self, literal: &Token) -> Result<Vec<u8>, Error> {
        match literal.kind {
            TokenKind::Int(-1) => Ok(vec![OP_ICONST_M1]),
            TokenKind::Boolean(false) | TokenKind::Int(0) => Ok(vec![OP_ICONST_0]),
            TokenKind::Boolean(true) | TokenKind::Int(1) => Ok(vec![OP_ICONST_1]),
            TokenKind::Int(2) => Ok(vec![OP_ICONST_2]),
            TokenKind::Int(3) => Ok(vec![OP_ICONST_3]),
            TokenKind::Int(4) => Ok(vec![OP_ICONST_4]),
            TokenKind::Int(5) => Ok(vec![OP_ICONST_5]),
            TokenKind::Int(n) if n <= std::i8::MAX as i32 => Ok(vec![OP_BIPUSH, n as u8]),
            TokenKind::Int(n) if n <= std::i16::MAX as i32 => {
                let v = vec![OP_SIPUSH, (n >> 8) as u8, (n & 0xff) as u8];
                Ok(v)
            }
            TokenKind::Int(n) if n <= std::i32::MAX => {
                add_and_push_constant(&mut self.constants, &Constant::Int(n))
            }
            TokenKind::Long(0) => Ok(vec![OP_LCONST_0]),
            TokenKind::Long(1) => Ok(vec![OP_LCONST_1]),
            TokenKind::Long(n) => add_and_push_constant(&mut self.constants, &Constant::Long(n)),
            TokenKind::Float(n) if n.to_bits() == 0f32.to_bits() => Ok(vec![OP_FCONST_0]),
            TokenKind::Float(n) if n.to_bits() == 1f32.to_bits() => Ok(vec![OP_FCONST_1]),
            TokenKind::Float(n) if n.to_bits() == 2f32.to_bits() => Ok(vec![OP_FCONST_2]),
            TokenKind::Float(n) => add_and_push_constant(&mut self.constants, &Constant::Float(n)),
            TokenKind::TString => {
                let s = String::from(&self.session.src[literal.span.start..literal.span.end]);
                let i = add_constant(&mut self.constants, &Constant::Utf8(s))?;
                add_and_push_constant(&mut self.constants, &Constant::CString(i))
            }
            _ => {
                dbg!(literal);
                unimplemented!()
            }
        }
    }
}
