use crate::error::*;
// use crate::lex::{Token, TokenKind};
use crate::jvm_constants::*;
use crate::parse::*;
use crate::session::Session;
use log::debug;

#[derive(Debug)]
enum Constant {
    Utf8(String),
    ClassInfo(u16),
    MethodRef(u16, u16),
    NameAndType(u16, u16),
}

#[derive(Debug)]
pub(crate) struct JvmEmitter<'a> {
    session: &'a Session<'a>,
    _types: &'a Types,
    constants: Vec<Constant>,
    source_file_constant_index: u16,
    source_file_name_constant_index: u16,
    ctor_str_index: u16,
    code_str_index: u16,
    line_table_str_index: u16,
    obj_str_index: u16,
    main_str_index: u16,
    main_descriptor_str_index: u16,
    super_class_index: u16,
    this_class_index: u16,
}

#[derive(Debug)]
struct Class {
    ctor: Function,
    methods: Vec<Function>,
}

#[derive(Debug)]
struct Function {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<Attribute>,
}

#[derive(Debug)]
enum Attribute {
    SourceFile {
        name_index: u16,
        source_file_index: u16,
    },
    Code {
        name_index: u16,
        max_stack: u16,
        max_locals: u16,
        code: Vec<u8>,
        exception_table: Vec<Exception>,
        attributes: Vec<Attribute>,
    },
    LineNumberTable {
        name_index: u16,
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
    [(b & 0xff_00) as u8, (b & 0x00_ff) as u8]
}

fn u32_to_u8s(b: u32) -> [u8; 4] {
    [
        (b & 0xff_00_00_00) as u8,
        (b & 0x00_ff_00_00) as u8,
        (b & 0x00_00_ff_00) as u8,
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
            Attribute::SourceFile { .. } => 2 + 4 + 2, // source_file_index
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

impl<'a> JvmEmitter<'a> {
    pub(crate) fn new(session: &'a Session, _types: &'a Types) -> JvmEmitter<'a> {
        let mut constants = Vec::new();
        let obj_method_ref_index = add_constant(&mut constants, Constant::MethodRef(2, 3)).unwrap(); // 1
        let super_class_index = add_constant(&mut constants, Constant::ClassInfo(4)).unwrap(); // 2
        let obj_name_type_index =
            add_constant(&mut constants, Constant::NameAndType(5, 6)).unwrap(); // 3
        let obj_str_index = add_constant(
            &mut constants,
            Constant::Utf8(String::from("java/lang/Object")),
        )
        .unwrap(); // 4

        let ctor_str_index =
            add_constant(&mut constants, Constant::Utf8(String::from(CTOR_STR))).unwrap();

        let obj_ctor_descriptor_index =
            add_constant(&mut constants, Constant::Utf8(String::from("()V"))).unwrap(); // 6

        let this_class_index = add_constant(&mut constants, Constant::ClassInfo(8)).unwrap(); // 7

        let class_name_index =
            add_constant(&mut constants, Constant::Utf8(String::from("Foo"))).unwrap(); // 8

        let code_str_index =
            add_constant(&mut constants, Constant::Utf8(String::from("Code"))).unwrap();

        let line_table_str_index = add_constant(
            &mut constants,
            Constant::Utf8(String::from("LineNumberTable")),
        )
        .unwrap();

        let main_str_index =
            add_constant(&mut constants, Constant::Utf8(String::from("main"))).unwrap();

        let main_descriptor_str_index = add_constant(
            &mut constants,
            Constant::Utf8(String::from("([Ljava/lang/String;)V")),
        )
        .unwrap();

        let source_file_constant_index =
            add_constant(&mut constants, Constant::Utf8(String::from("SourceFile"))).unwrap();
        let source_file_name_constant_index =
            add_constant(&mut constants, Constant::Utf8(String::from("Foo.java"))).unwrap();

        JvmEmitter {
            session,
            _types,
            constants,
            source_file_constant_index,
            source_file_name_constant_index,
            code_str_index,
            line_table_str_index,
            ctor_str_index,
            obj_str_index,
            main_str_index,
            main_descriptor_str_index,
            super_class_index,
            this_class_index,
        }
    }

    pub(crate) fn statements<W: std::io::Write>(
        &self,
        _block: &AstNodeStmt,
        w: &mut W,
    ) -> Result<(), Error> {
        self.prologue(w)?;
        self.constant_pool(w)?;
        self.access_flags(w)?;
        self.this_class(w)?;
        self.super_class(w)?;
        self.interfaces(w)?;
        self.fields(w)?;

        let methods = vec![
            Function {
                access_flags: 0,
                name_index: 5,
                descriptor_index: 6,
                attributes: vec![Attribute::Code {
                    name_index: self.code_str_index,
                    max_stack: 1,
                    max_locals: 1,
                    code: vec![OP_ALOAD_0, OP_INVOKE_SPECIAL, 0x00, 0x01, OP_RETURN],
                    exception_table: vec![],
                    attributes: vec![Attribute::LineNumberTable {
                        name_index: self.line_table_str_index,
                        line_number_tables: vec![LineNumberTable {
                            start_pc: 0,
                            line_number: 1,
                        }],
                    }],
                }],
            },
            Function {
                access_flags: METHOD_ACC_PUBLIC | METHOD_ACC_STATIC,
                name_index: self.main_str_index,
                descriptor_index: self.main_descriptor_str_index,
                attributes: vec![Attribute::Code {
                    name_index: self.code_str_index,
                    max_stack: 0,
                    max_locals: 1,
                    code: vec![OP_RETURN],
                    exception_table: vec![],
                    attributes: vec![Attribute::LineNumberTable {
                        name_index: self.line_table_str_index,
                        line_number_tables: vec![LineNumberTable {
                            start_pc: 0,
                            line_number: 2,
                        }],
                    }],
                }],
            },
        ];
        self.methods(&methods, w)?;

        let attributes = vec![Attribute::SourceFile {
            name_index: self.source_file_constant_index,
            source_file_index: self.source_file_name_constant_index,
        }];
        self.attributes(&attributes, w)?;

        Ok(())
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
        // FIXME
        w.write(&u16_to_u8s(CLASS_ACC_SUPER))?;
        Ok(())
    }

    fn this_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&u16_to_u8s(self.this_class_index))?;
        Ok(())
    }

    fn super_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&u16_to_u8s(self.super_class_index))?;
        Ok(())
    }

    fn interfaces<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x00])?;
        Ok(())
    }

    fn fields<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
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
        w.write(&u16_to_u8s(method.name_index))?;
        w.write(&u16_to_u8s(method.descriptor_index))?;

        self.attributes(&method.attributes, w)?;

        Ok(())
    }

    fn attribute<W: std::io::Write>(&self, attribute: &Attribute, w: &mut W) -> Result<(), Error> {
        debug!("attribute={:?}", attribute);

        match attribute {
            Attribute::Code {
                name_index,
                max_stack,
                max_locals,
                code,
                exception_table,
                attributes,
            } => {
                w.write(&u16_to_u8s(*name_index))?;

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
            Attribute::SourceFile {
                name_index,
                source_file_index,
            } => {
                w.write(&u16_to_u8s(*name_index))?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write(&u32_to_u8s(size))?;
                w.write(&u16_to_u8s(*source_file_index))?;
            }
            Attribute::LineNumberTable {
                name_index,
                line_number_tables,
            } => {
                w.write(&u16_to_u8s(*name_index))?;
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
