use crate::error::*;
use crate::jvm_constants::*;
use crate::jvm_emitter::*;
use log::debug;

impl LineNumberTable {
    fn size(&self) -> u32 {
        2 // start_pc
        + 2 // line_number
    }
}

impl VerificationTypeInfo {
    fn size(&self) -> u32 {
        match self {
            VerificationTypeInfo::Int => 1,
        }
    }
}

impl StackMapFrame {
    fn size(&self) -> u32 {
        match self {
            StackMapFrame::SameFrame { .. } => 1,
            StackMapFrame::SameLocalsOneStackItemFrame { stack, .. } => 1 + stack.size(),
            StackMapFrame::FullFrame { locals, stack, .. } => {
                1 + 2
                    + 2
                    + locals.iter().map(|l| l.size()).sum::<u32>()
                    + 2
                    + stack.iter().map(|l| l.size()).sum::<u32>()
            }
        }
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
            Attribute::StackMapTable { entries, .. } => {
                2 + 4 + 2 + entries.iter().map(|l| l.size()).sum::<u32>()
            }
        }
    }
}

impl<'a> JvmEmitter<'a> {
    pub(crate) fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        self.prologue(w)?;
        self.constant_pool(w)?;
        self.access_flags(w)?;
        self.this_class(w)?;
        self.super_class(w)?;
        self.interfaces(w)?;
        self.fields(w)?;
        self.methods(w)?;
        self.attributes(&self.attributes, w)?;

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
        let len = &(self.constants.len() as u16 + 1).to_be_bytes();
        debug!("constant pool size={:#04X} {:#04X}", len[0], len[1]);
        w.write(len)?;

        for constant in &self.constants {
            self.constant(constant, w)?;
        }

        Ok(())
    }

    fn access_flags<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO: figure out why
        w.write(&CLASS_ACC_SUPER.to_be_bytes())?;
        Ok(())
    }

    fn this_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&self.this_class.to_be_bytes())?;
        Ok(())
    }

    fn super_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write(&self.super_class.to_be_bytes())?;
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

    fn methods<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        let len = &(self.methods.len() as u16).to_be_bytes();
        debug!("methods size={:#04X} {:#04X}", len[0], len[1]);
        w.write(len)?;

        for m in &self.methods {
            self.method(m, w)?;
        }

        Ok(())
    }

    fn method<W: std::io::Write>(&self, method: &Function, w: &mut W) -> Result<(), Error> {
        debug!("method={:?}", method);

        w.write(&method.access_flags.to_be_bytes())?;
        w.write(&method.name.to_be_bytes())?;
        w.write(&method.descriptor.to_be_bytes())?;

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
                w.write(&name.to_be_bytes())?;

                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                debug!("code: size={} code={:?}", size, code);
                w.write(&size.to_be_bytes())?;

                w.write(&max_stack.to_be_bytes())?;

                w.write(&max_locals.to_be_bytes())?;

                w.write(&(code.len() as u32).to_be_bytes())?;
                w.write(&code)?;

                w.write(&(exception_table.len() as u16).to_be_bytes())?;
                // TODO
                assert!(exception_table.is_empty());
                // w.write(&exception_table)?;

                self.attributes(attributes, w)?;
            }
            Attribute::SourceFile { name, source_file } => {
                w.write(&name.to_be_bytes())?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write(&size.to_be_bytes())?;
                w.write(&source_file.to_be_bytes())?;
            }
            Attribute::LineNumberTable {
                name,
                line_number_tables,
            } => {
                w.write(&name.to_be_bytes())?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write(&size.to_be_bytes())?;

                self.line_number_tables(line_number_tables, w)?;
            }
            Attribute::StackMapTable { name, entries } => {
                w.write(&name.to_be_bytes())?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                debug!("attribute stack_map_table: size={}", size);
                w.write(&size.to_be_bytes())?;
                self.stack_map_frames(entries, w)?;
            }
        }

        Ok(())
    }

    fn attributes<W: std::io::Write>(
        &self,
        attributes: &[Attribute],
        w: &mut W,
    ) -> Result<(), Error> {
        let len = &(attributes.len() as u16).to_be_bytes();
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
                w.write(&(s.len() as u16).to_be_bytes())?;
                w.write(&s.as_bytes())?;
            }
            Constant::ClassInfo(index) => {
                w.write(&[CONSTANT_CLASS])?;
                w.write(&index.to_be_bytes())?;
            }
            Constant::NameAndType(name, type_descriptor) => {
                w.write(&[CONSTANT_NAME_AND_TYPE])?;
                w.write(&name.to_be_bytes())?;
                w.write(&type_descriptor.to_be_bytes())?;
            }
            Constant::MethodRef(class, name_and_type) => {
                w.write(&[CONSTANT_METHODREF])?;
                w.write(&class.to_be_bytes())?;
                w.write(&name_and_type.to_be_bytes())?;
            }
            Constant::FieldRef(class, name_and_type) => {
                w.write(&[CONSTANT_FIELDREF])?;
                w.write(&class.to_be_bytes())?;
                w.write(&name_and_type.to_be_bytes())?;
            }
            Constant::CString(index) => {
                w.write(&[CONSTANT_STRING])?;
                w.write(&index.to_be_bytes())?;
            }
            Constant::Int(n) => {
                w.write(&[CONSTANT_INTEGER])?;
                w.write(&(*n as u32).to_be_bytes())?;
            }
            Constant::Float(n) => {
                w.write(&[CONSTANT_FLOAT])?;
                debug!("const float: n={} v={:?}", n, n.to_be_bytes());
                w.write(&n.to_be_bytes())?;
            }
            Constant::LongHigh(n) => {
                w.write(&[CONSTANT_LONG])?;
                w.write(&(*n as u32).to_be_bytes())?;
            }
            // This is always preceded by LongHigh which writes the tag
            Constant::LongLow(n) => {
                w.write(&(*n as u32).to_be_bytes())?;
            }
            Constant::Long(_) => unreachable!(),
        }
        Ok(())
    }

    fn verification_type_info<W: std::io::Write>(
        &self,
        v: &VerificationTypeInfo,
        w: &mut W,
    ) -> Result<(), Error> {
        match v {
            VerificationTypeInfo::Int => {
                w.write(&[ITEM_INTEGER])?;
            }
        }

        Ok(())
    }

    fn stack_map_frame<W: std::io::Write>(
        &self,
        entry: &StackMapFrame,
        w: &mut W,
    ) -> Result<(), Error> {
        match entry {
            StackMapFrame::SameFrame { offset } => {
                assert!(*offset <= 63);
                w.write(&[*offset])?;
            }
            StackMapFrame::SameLocalsOneStackItemFrame { offset, stack } => {
                assert!(*offset <= 63);
                w.write(&[64 + *offset])?;
                self.verification_type_info(stack, w)?;
            }
            StackMapFrame::FullFrame {
                offset,
                stack,
                locals,
            } => {
                w.write(&FULL_FRAME.to_be_bytes())?;
                w.write(&offset.to_be_bytes())?;

                w.write(&(locals.len() as u16).to_be_bytes())?;
                for v in locals {
                    self.verification_type_info(v, w)?;
                }

                w.write(&(stack.len() as u16).to_be_bytes())?;
                for v in stack {
                    self.verification_type_info(v, w)?;
                }
            }
        }
        Ok(())
    }

    fn stack_map_frames<W: std::io::Write>(
        &self,
        entries: &[StackMapFrame],
        w: &mut W,
    ) -> Result<(), Error> {
        debug!("stack_map_frames={:?}", entries);
        w.write(&(entries.len() as u16).to_be_bytes())?;

        for e in entries {
            self.stack_map_frame(e, w)?;
        }

        Ok(())
    }

    fn line_number_tables<W: std::io::Write>(
        &self,
        line_number_tables: &[LineNumberTable],
        w: &mut W,
    ) -> Result<(), Error> {
        debug!("line_number_tables={:?}", line_number_tables);
        w.write(&(line_number_tables.len() as u16).to_be_bytes())?;

        for l in line_number_tables {
            w.write(&l.start_pc.to_be_bytes())?;
            w.write(&l.line_number.to_be_bytes())?;
        }
        Ok(())
    }
}
