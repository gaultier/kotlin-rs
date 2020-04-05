use crate::error::*;
// use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::Session;
use log::debug;

const CTOR_STR: &'static str = "<init>";

const ACC_SUPER: u16 = 0x0020;

const CONSTANT_CLASS: u8 = 7;
const _CONSTANT_FIELDREF: u8 = 9;
const CONSTANT_METHODREF: u8 = 10;
const _CONSTANT_INTERFACE_METHODREF: u8 = 11;
const _CONSTANT_STRING: u8 = 8;
const _CONSTANT_INTEGER: u8 = 3;
const _CONSTANT_FLOAT: u8 = 4;
const _CONSTANT_LONG: u8 = 5;
const _CONSTANT_DOUBLE: u8 = 6;
const CONSTANT_NAME_AND_TYPE: u8 = 12;
const CONSTANT_UTF8: u8 = 1;
const _CONSTANT_METHOD_HANDLE: u8 = 15;
const _CONSTANT_METHOD_TYPE: u8 = 16;
const _CONSTANT_INVOKE_DYNAMIC: u8 = 18;

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
struct Attribute {
    name_index: u16,
    info: Vec<u8>,
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

impl<'a> JvmEmitter<'a> {
    pub(crate) fn new(session: &'a Session, _types: &'a Types) -> JvmEmitter<'a> {
        JvmEmitter {
            session,
            _types,
            constants: vec![
                Constant::MethodRef(2, 3),                               // 1
                Constant::ClassInfo(4),                                  // 2
                Constant::NameAndType(5, 6),                             // 3
                Constant::Utf8(String::from("java/lang/Object")),        // 4
                Constant::Utf8(String::from(CTOR_STR)),                  // 5
                Constant::Utf8(String::from("()V")),                     // 6
                Constant::ClassInfo(8),                                  // 7
                Constant::Utf8(String::from("Foo")),                     // 8
                Constant::Utf8(String::from("Code")),                    // 9
                Constant::Utf8(String::from("LineNumberTable")),         // 10
                Constant::Utf8(String::from("main")),                    // 11
                Constant::Utf8(String::from("([Ljava/lang/String;])V")), // 12
                Constant::Utf8(String::from("SourceFile")),              // 13
                Constant::Utf8(String::from("Foo.java")),                // 14
            ],
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
                attributes: vec![Attribute {
                    name_index: 9, // Code
                    info: vec![],  // TODO
                }],
            },
            Function {
                access_flags: 0,
                name_index: 1,
                descriptor_index: 3,
                attributes: vec![],
            },
        ];
        self.methods(&methods, w)?;
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
        let len = &u16_to_u8s(self.constants.len() as u16);
        debug!("constant pool size={:#04X} {:#04X}", len[0], len[1]);
        w.write(len)?;

        for constant in &self.constants {
            self.constant(constant, w)?;
        }

        Ok(())
    }

    fn access_flags<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&u16_to_u8s(ACC_SUPER))?;
        Ok(())
    }

    fn this_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x07])?;
        Ok(())
    }

    fn super_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x02])?;
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
        w.write(&u16_to_u8s(method.attributes.len() as u16))?;

        for attribute in &method.attributes {
            self.attribute(attribute, w)?;
        }
        Ok(())
    }

    fn attribute<W: std::io::Write>(&self, attribute: &Attribute, w: &mut W) -> Result<(), Error> {
        debug!("attribute={:?}", attribute);

        w.write(&u16_to_u8s(attribute.name_index))?;
        w.write(&u32_to_u8s(attribute.info.len() as u32))?;
        w.write(&attribute.info)?;
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
}
