use crate::error::*;
use crate::jvm_constants::*;
use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::Session;
use log::debug;

#[derive(Debug)]
enum Constant {
    Utf8(String),
    ClassInfo(u16),
    MethodRef(u16, u16),
    FieldRef(u16, u16),
    NameAndType(u16, u16),
    CString(u16),
    Int(i32),
}

#[derive(Debug)]
pub(crate) struct JvmEmitter<'a> {
    session: &'a Session<'a>,
    _types: &'a Types,
    constants: Vec<Constant>,
    source_file_constant: u16,
    source_file_name_constant: u16,
    ctor_str: u16,
    code_str: u16,
    line_table_str: u16,
    obj_str: u16,
    main_str: u16,
    main_descriptor_str: u16,
    super_class: u16,
    this_class: u16,
    obj_ctor_descriptor: u16,
    obj_method_ref: u16,
    out_fieldref: u16,
    println_methodref: u16,
    hello_str_string: u16,
}

#[derive(Debug)]
struct Class {
    ctor: Function,
    methods: Vec<Function>,
}

#[derive(Debug)]
struct Function {
    access_flags: u16,
    name: u16,
    descriptor: u16,
    attributes: Vec<Attribute>,
}

#[derive(Debug)]
enum Attribute {
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
}

#[derive(Debug)]
struct LineNumberTable {
    start_pc: u16,
    line_number: u16,
}

#[derive(Debug)]
struct Exception {
    start_pc: u16,
    end_pc: u16,
    handler_pc: u16,
    catch_type: u16,
}

fn u16_to_u8s(b: u16) -> [u8; 2] {
    [(b >> 8) as u8, (b & 0x00_ff) as u8]
}

fn u32_to_u8s(b: u32) -> [u8; 4] {
    [
        (b >> 24) as u8,
        (b >> 16) as u8,
        (b >> 8) as u8,
        (b & 0x00_00_00_ff) as u8,
    ]
}

impl LineNumberTable {
    fn size(&self) -> u32 {
        2 // start_pc
        + 2 // line_number
    }
}

impl Exception {
    fn size(&self) -> u32 {
        2 // start_pc
            + 2  // end_pc
            + 2 // handler_pc
            + 2 // catch_type
    }
}

impl Attribute {
    fn size(&self) -> u32 {
        match self {
            Attribute::SourceFile { .. } => 2 + 4 + 2, // source_file
            Attribute::LineNumberTable {
                line_number_tables, ..
            } => {
                2 + 4 +
                2 // line_number_tables len
            + line_number_tables.iter().map(|l| l.size()).sum::<u32>()
            }
            Attribute::Code {
                code,
                exception_table,
                attributes,
                ..
            } => {
                2 + 4 +
                2 // max_stacks
                    + 2 // max_locals
                    + 4 // code len
                    + code.len() as u32
                    + 2 // exception_table len
                    + exception_table.iter().map(|e| e.size()).sum::<u32>()
                    + 2 // attributes len
                    + attributes.iter().map(|a| a.size()).sum::<u32>()
            }
        }
    }
}

fn add_constant(constants: &mut Vec<Constant>, constant: Constant) -> Result<u16, Error> {
    // Since `constants` is one-indexed, the max index is `std::u16::MAX+1` but since we
    // can only index it with u16 the real max index is `std::u16::MAX`
    if constants.len() < std::u16::MAX as usize {
        constants.push(constant);
        Ok(constants.len() as u16)
    } else {
        Err(Error::new(
            ErrorKind::MaxConstantsReached(constants.len() as u16),
            Location::new(),
        ))
    }
}

fn add_and_push_constant(
    constants: &mut Vec<Constant>,
    constant: Constant,
) -> Result<Vec<u8>, Error> {
    let i = add_constant(constants, constant)?;

    if i <= std::u8::MAX as u16 {
        Ok(vec![OP_LDC, i as u8])
    } else {
        Ok(vec![OP_LDC_W, (i >> 8) as u8, (i & 0xff) as u8])
    }
}

fn binary_op(kind: &TokenKind) -> u8 {
    match kind {
        TokenKind::Plus => OP_IADD,
        TokenKind::Star => OP_IMUL,
        TokenKind::Minus => OP_ISUB,
        TokenKind::Slash => OP_IDIV,
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
    pub(crate) fn new(session: &'a Session, _types: &'a Types) -> JvmEmitter<'a> {
        let mut constants = Vec::new();

        let obj_str = add_constant(
            &mut constants,
            Constant::Utf8(String::from("java/lang/Object")),
        )
        .unwrap();
        let obj_ctor_descriptor =
            add_constant(&mut constants, Constant::Utf8(String::from("()V"))).unwrap();

        let ctor_str =
            add_constant(&mut constants, Constant::Utf8(String::from(CTOR_STR))).unwrap();

        let obj_name_type = add_constant(
            &mut constants,
            Constant::NameAndType(ctor_str, obj_ctor_descriptor),
        )
        .unwrap();

        let super_class = add_constant(&mut constants, Constant::ClassInfo(obj_str)).unwrap();

        let obj_method_ref = add_constant(
            &mut constants,
            Constant::MethodRef(super_class, obj_name_type),
        )
        .unwrap();
        let this_class_name =
            add_constant(&mut constants, Constant::Utf8(String::from("Foo"))).unwrap();

        let this_class =
            add_constant(&mut constants, Constant::ClassInfo(this_class_name)).unwrap();

        let code_str = add_constant(&mut constants, Constant::Utf8(String::from("Code"))).unwrap();

        let line_table_str = add_constant(
            &mut constants,
            Constant::Utf8(String::from("LineNumberTable")),
        )
        .unwrap();

        let main_str = add_constant(&mut constants, Constant::Utf8(String::from("main"))).unwrap();

        let main_descriptor_str = add_constant(
            &mut constants,
            Constant::Utf8(String::from("([Ljava/lang/String;)V")),
        )
        .unwrap();

        let source_file_constant =
            add_constant(&mut constants, Constant::Utf8(String::from("SourceFile"))).unwrap();
        let source_file_name_constant =
            add_constant(&mut constants, Constant::Utf8(String::from("Foo.java"))).unwrap();

        let class_system_str = add_constant(
            &mut constants,
            Constant::Utf8(String::from("java/lang/System")),
        )
        .unwrap();
        let class_system =
            add_constant(&mut constants, Constant::ClassInfo(class_system_str)).unwrap();

        let out_str = add_constant(&mut constants, Constant::Utf8(String::from("out"))).unwrap();

        let hello_str =
            add_constant(&mut constants, Constant::Utf8(String::from("Hello!"))).unwrap();

        let hello_str_string = add_constant(&mut constants, Constant::CString(hello_str)).unwrap();

        let println_str =
            add_constant(&mut constants, Constant::Utf8(String::from("println"))).unwrap();

        let printstream_str = add_constant(
            &mut constants,
            Constant::Utf8(String::from("java/io/PrintStream")),
        )
        .unwrap();

        let printstream_str_type = add_constant(
            &mut constants,
            Constant::Utf8(String::from("Ljava/io/PrintStream;")),
        )
        .unwrap();
        let out_name_type = add_constant(
            &mut constants,
            Constant::NameAndType(out_str, printstream_str_type),
        )
        .unwrap();

        let out_fieldref = add_constant(
            &mut constants,
            Constant::FieldRef(class_system, out_name_type),
        )
        .unwrap();

        let class_printstream =
            add_constant(&mut constants, Constant::ClassInfo(printstream_str)).unwrap();

        let println_str_type = add_constant(
            &mut constants,
            Constant::Utf8(String::from("(I)V")), // FIXME
        )
        .unwrap();

        let println_name_type = add_constant(
            &mut constants,
            Constant::NameAndType(println_str, println_str_type),
        )
        .unwrap();
        let println_methodref = add_constant(
            &mut constants,
            Constant::MethodRef(class_printstream, println_name_type),
        )
        .unwrap();

        JvmEmitter {
            session,
            _types,
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
            println_methodref,
            hello_str_string,
        }
    }

    pub(crate) fn statements<W: std::io::Write>(
        &mut self,
        block: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        let methods = vec![
            Function {
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
                        u16_to_u8s(self.obj_method_ref)[0],
                        u16_to_u8s(self.obj_method_ref)[1],
                        OP_RETURN,
                    ],
                    exception_table: vec![],
                    attributes: vec![Attribute::LineNumberTable {
                        name: self.line_table_str,
                        line_number_tables: vec![LineNumberTable {
                            start_pc: 0,
                            line_number: 1,
                        }],
                    }],
                }],
            },
            Function {
                access_flags: METHOD_ACC_PUBLIC | METHOD_ACC_STATIC,
                name: self.main_str,
                descriptor: self.main_descriptor_str,
                attributes: vec![Attribute::Code {
                    name: self.code_str,
                    max_stack: 100, // FIXME
                    max_locals: 1,
                    code: self.statement(block)?,
                    exception_table: vec![],
                    attributes: vec![Attribute::LineNumberTable {
                        name: self.line_table_str,
                        line_number_tables: vec![LineNumberTable {
                            start_pc: 0,
                            line_number: 2,
                        }],
                    }],
                }],
            },
        ];

        let attributes = vec![Attribute::SourceFile {
            name: self.source_file_constant,
            source_file: self.source_file_name_constant,
        }];

        self.prologue(w)?;
        self.constant_pool(w)?;
        self.access_flags(w)?;
        self.this_class(w)?;
        self.super_class(w)?;
        self.interfaces(w)?;
        self.fields(w)?;
        self.methods(&methods, w)?;
        self.attributes(&attributes, w)?;

        Ok(())
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

    fn expr(&mut self, expr: &AstNodeExpr) -> Result<Vec<u8>, Error> {
        match expr {
            AstNodeExpr::Literal(l, _) => self.literal(l),
            AstNodeExpr::Unary { .. } => self.unary(expr),
            AstNodeExpr::Binary { .. } => self.binary(expr),
            AstNodeExpr::Grouping(e, _) => self.expr(e),
            AstNodeExpr::Println(e, _) => {
                let mut v = vec![
                    OP_GET_STATIC,
                    u16_to_u8s(self.out_fieldref)[0],
                    u16_to_u8s(self.out_fieldref)[1],
                ];
                v.append(&mut self.expr(e)?);
                v.append(&mut vec![
                    OP_INVOKE_VIRTUAL,
                    u16_to_u8s(self.println_methodref)[0],
                    u16_to_u8s(self.println_methodref)[1],
                    OP_RETURN,
                ]);
                Ok(v)
            }
            _ => unimplemented!(),
        }
    }

    fn binary(&mut self, expr: &AstNodeExpr) -> Result<Vec<u8>, Error> {
        match expr {
            AstNodeExpr::Binary {
                left, op, right, ..
            } => {
                let mut v = self.expr(left)?;
                v.append(&mut self.expr(right)?);
                v.push(binary_op(&op.kind));
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
            _ => unimplemented!(),
        }
    }

    fn literal(&mut self, literal: &Token) -> Result<Vec<u8>, Error> {
        match literal.kind {
            TokenKind::Int(-1) => Ok(vec![OP_ICONST_M1]),
            TokenKind::Int(0) => Ok(vec![OP_ICONST_0]),
            TokenKind::Int(1) => Ok(vec![OP_ICONST_1]),
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
                add_and_push_constant(&mut self.constants, Constant::Int(n))
            }
            _ => unimplemented!(),
        }
    }

    fn magic_number<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&[0xca, 0xfe, 0xba, 0xbe])?;
        Ok(())
    }

    fn version<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // Minor
        w.write(&[0x00, 0x00])?;
        // Major: 0x39 = 57 => java 13
        w.write(&[0x00, 0x39])?;
        Ok(())
    }

    fn prologue<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        self.magic_number(w)?;
        self.version(w)?;
        Ok(())
    }

    fn constant_pool<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // + 1 because it is one-indexed
        let len = &u16_to_u8s(self.constants.len() as u16 + 1);
        debug!("constant pool size={:#04X} {:#04X}", len[0], len[1]);
        w.write(len)?;

        for constant in &self.constants {
            self.constant(constant, w)?;
        }

        Ok(())
    }

    fn access_flags<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO: figure out why
        w.write(&u16_to_u8s(CLASS_ACC_SUPER))?;
        Ok(())
    }

    fn this_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&u16_to_u8s(self.this_class))?;
        Ok(())
    }

    fn super_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&u16_to_u8s(self.super_class))?;
        Ok(())
    }

    fn interfaces<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO
        w.write(&[0x00, 0x00])?;
        Ok(())
    }

    fn fields<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO
        w.write(&[0x00, 0x00])?;
        Ok(())
    }

    fn methods<W: std::io::Write>(&self, methods: &[Function], w: &mut W) -> Result<(), Error> {
        let len = &u16_to_u8s(methods.len() as u16);
        debug!("methods size={:#04X} {:#04X}", len[0], len[1]);
        w.write(len)?;

        for m in methods {
            self.method(m, w)?;
        }

        Ok(())
    }

    fn method<W: std::io::Write>(&self, method: &Function, w: &mut W) -> Result<(), Error> {
        debug!("method={:?}", method);

        w.write(&u16_to_u8s(method.access_flags))?;
        w.write(&u16_to_u8s(method.name))?;
        w.write(&u16_to_u8s(method.descriptor))?;

        self.attributes(&method.attributes, w)?;

        Ok(())
    }

    fn attribute<W: std::io::Write>(&self, attribute: &Attribute, w: &mut W) -> Result<(), Error> {
        debug!("attribute={:?}", attribute);

        match attribute {
            Attribute::Code {
                name,
                max_stack,
                max_locals,
                code,
                exception_table,
                attributes,
            } => {
                w.write(&u16_to_u8s(*name))?;

                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                debug!("code size={}", size);
                w.write(&u32_to_u8s(size))?;

                w.write(&u16_to_u8s(*max_stack))?;

                w.write(&u16_to_u8s(*max_locals))?;

                w.write(&u32_to_u8s((code.len()) as u32))?;
                w.write(&code)?;

                w.write(&u16_to_u8s(exception_table.len() as u16))?;
                // TODO
                assert!(exception_table.is_empty());
                // w.write(&exception_table)?;

                self.attributes(attributes, w)?;
            }
            Attribute::SourceFile { name, source_file } => {
                w.write(&u16_to_u8s(*name))?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write(&u32_to_u8s(size))?;
                w.write(&u16_to_u8s(*source_file))?;
            }
            Attribute::LineNumberTable {
                name,
                line_number_tables,
            } => {
                w.write(&u16_to_u8s(*name))?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write(&u32_to_u8s(size))?;

                self.line_number_tables(line_number_tables, w)?;
            }
        }

        Ok(())
    }

    fn attributes<W: std::io::Write>(
        &self,
        attributes: &[Attribute],
        w: &mut W,
    ) -> Result<(), Error> {
        let len = &u16_to_u8s(attributes.len() as u16);
        debug!("attributes size={:#04X} {:#04X}", len[0], len[1]);
        w.write(len)?;

        for a in attributes {
            self.attribute(a, w)?;
        }

        Ok(())
    }

    fn constant<W: std::io::Write>(&self, constant: &Constant, w: &mut W) -> Result<(), Error> {
        debug!("constant={:?}", constant);

        match constant {
            Constant::Utf8(s) => {
                w.write(&[CONSTANT_UTF8])?;
                w.write(&u16_to_u8s(s.len() as u16))?;
                w.write(&s.as_bytes())?;
            }
            Constant::ClassInfo(index) => {
                w.write(&[CONSTANT_CLASS])?;
                w.write(&u16_to_u8s(*index))?;
            }
            Constant::NameAndType(name, type_descriptor) => {
                w.write(&[CONSTANT_NAME_AND_TYPE])?;
                w.write(&u16_to_u8s(*name))?;
                w.write(&u16_to_u8s(*type_descriptor))?;
            }
            Constant::MethodRef(class, name_and_type) => {
                w.write(&[CONSTANT_METHODREF])?;
                w.write(&u16_to_u8s(*class))?;
                w.write(&u16_to_u8s(*name_and_type))?;
            }
            Constant::FieldRef(class, name_and_type) => {
                w.write(&[CONSTANT_FIELDREF])?;
                w.write(&u16_to_u8s(*class))?;
                w.write(&u16_to_u8s(*name_and_type))?;
            }
            Constant::CString(index) => {
                w.write(&[CONSTANT_STRING])?;
                w.write(&u16_to_u8s(*index))?;
            }
            Constant::Int(n) => {
                w.write(&[CONSTANT_INTEGER])?;
                debug!("const int: n={} v={:?}", n, &u32_to_u8s(*n as u32));
                w.write(&u32_to_u8s(*n as u32))?;
            }
        }
        Ok(())
    }

    fn line_number_tables<W: std::io::Write>(
        &self,
        line_number_tables: &[LineNumberTable],
        w: &mut W,
    ) -> Result<(), Error> {
        debug!("line_number_tables={:?}", line_number_tables);
        w.write(&u16_to_u8s(line_number_tables.len() as u16))?;

        for l in line_number_tables {
            w.write(&u16_to_u8s(l.start_pc))?;
            w.write(&u16_to_u8s(l.line_number))?;
        }
        Ok(())
    }
}
