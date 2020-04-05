use crate::error::*;
// use crate::lex::{Token, TokenKind};
use crate::parse::*;
use crate::session::Session;
use log::debug;

const ACC_SUPER: u16 = 0x0002;

const CONSTANT_CLASS: u8 = 7;
const CONSTANT_FIELDREF: u8 = 9;
const CONSTANT_METHODREF: u8 = 10;
const CONSTANT_INTERFACE_METHODREF: u8 = 11;
const CONSTANT_STRING: u8 = 8;
const CONSTANT_INTEGER: u8 = 3;
const CONSTANT_FLOAT: u8 = 4;
const CONSTANT_LONG: u8 = 5;
const CONSTANT_DOUBLE: u8 = 6;
const CONSTANT_NAME_AND_TYPE: u8 = 12;
const CONSTANT_UTF8: u8 = 1;
const CONSTANT_METHOD_HANDLE: u8 = 15;
const CONSTANT_METHOD_TYPE: u8 = 16;
const CONSTANT_INVOKE_DYNAMIC: u8 = 18;

#[derive(Debug)]
enum Constant {
    Utf8(String),
}

#[derive(Debug)]
pub(crate) struct JvmEmitter<'a> {
    session: &'a Session<'a>,
    _types: &'a Types,
    constants: Vec<Constant>,
}

fn u16_to_u8s(b: u16) -> [u8; 2] {
    [(b & 0xff00) as u8, (b & 0x00ff) as u8]
}

impl<'a> JvmEmitter<'a> {
    pub(crate) fn new(session: &'a Session, _types: &'a Types) -> JvmEmitter<'a> {
        JvmEmitter {
            session,
            _types,
            constants: vec![Constant::Utf8(String::from("java/lang/Object"))],
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
        self.methods(w)?;
        self.attributes(w)?;
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
        w.write(&[(ACC_SUPER & 0xff00) as u8, (ACC_SUPER & 0x00ff) as u8])?;
        Ok(())
    }

    fn this_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x00])?;
        Ok(())
    }

    fn super_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x00])?;
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

    fn methods<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x00])?;
        Ok(())
    }

    fn attributes<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // FIXME
        w.write(&[0x00, 0x00])?;
        Ok(())
    }

    fn constant<W: std::io::Write>(&self, constant: &Constant, w: &mut W) -> Result<(), Error> {
        debug!("constant={:?}", constant);

        match constant {
            Constant::Utf8(s) => {
                w.write(&[CONSTANT_UTF8])?;
                w.write(&s.as_bytes())?;
            }
        }
        Ok(())
    }
}
