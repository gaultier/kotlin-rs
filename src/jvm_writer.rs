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
            StackMapFrame::Same { .. } => 1,
            StackMapFrame::SameLocalsOneStackItem { stack, .. } => 1 + stack.size(),
            StackMapFrame::Full { locals, stack, .. } => {
                1 + // tag
                    2 // offset
                    + 2 // locals len
                    + locals.iter().map(|l| l.size()).sum::<u32>()
                    + 2 // stack len
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
        w.write_all(&[0xca, 0xfe, 0xba, 0xbe])?;
        Ok(())
    }

    fn version<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // Minor
        w.write_all(&[0x00, 0x00])?;
        // Major: 50 => java 6
        w.write_all(&50u16.to_be_bytes())?;
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
        w.write_all(len)?;

        for constant in &self.constants {
            self.constant(constant, w)?;
        }

        Ok(())
    }

    fn access_flags<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO: figure out why
        w.write_all(&CLASS_ACC_SUPER.to_be_bytes())?;
        Ok(())
    }

    fn this_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(&self.this_class.to_be_bytes())?;
        Ok(())
    }

    fn super_class<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        w.write_all(&self.super_class.to_be_bytes())?;
        Ok(())
    }

    fn interfaces<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO
        w.write_all(&[0x00, 0x00])?;
        Ok(())
    }

    fn fields<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        // TODO
        w.write_all(&[0x00, 0x00])?;
        Ok(())
    }

    fn methods<W: std::io::Write>(&self, w: &mut W) -> Result<(), Error> {
        let len = &(self.methods.len() as u16).to_be_bytes();
        debug!("methods size={:#04X} {:#04X}", len[0], len[1]);
        w.write_all(len)?;

        for m in &self.methods {
            self.method(m, w)?;
        }

        Ok(())
    }

    fn method<W: std::io::Write>(&self, method: &Function, w: &mut W) -> Result<(), Error> {
        debug!("method={:?}", method);

        w.write_all(&method.access_flags.to_be_bytes())?;
        w.write_all(&method.name.to_be_bytes())?;
        w.write_all(&method.descriptor.to_be_bytes())?;

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
                w.write_all(&name.to_be_bytes())?;

                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                debug!("code: size={} code={:?}", size, code);
                w.write_all(&size.to_be_bytes())?;

                w.write_all(&max_stack.to_be_bytes())?;

                w.write_all(&max_locals.to_be_bytes())?;

                w.write_all(&(code.len() as u32).to_be_bytes())?;
                w.write_all(&code)?;

                w.write_all(&(exception_table.len() as u16).to_be_bytes())?;
                // TODO
                assert!(exception_table.is_empty());
                // w.write_all(&exception_table)?;

                self.attributes(attributes, w)?;
            }
            Attribute::SourceFile { name, source_file } => {
                w.write_all(&name.to_be_bytes())?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write_all(&size.to_be_bytes())?;
                w.write_all(&source_file.to_be_bytes())?;
            }
            Attribute::LineNumberTable {
                name,
                line_number_tables,
            } => {
                w.write_all(&name.to_be_bytes())?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                w.write_all(&size.to_be_bytes())?;

                self.line_number_tables(line_number_tables, w)?;
            }
            Attribute::StackMapTable { name, entries } => {
                w.write_all(&name.to_be_bytes())?;
                let size: u32 = (attribute.size() as isize - (2 + 4)) as u32;
                debug!("attribute stack_map_table: size={}", size);
                w.write_all(&size.to_be_bytes())?;
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
        w.write_all(len)?;

        for a in attributes {
            self.attribute(a, w)?;
        }

        Ok(())
    }

    fn constant<W: std::io::Write>(&self, constant: &Constant, w: &mut W) -> Result<(), Error> {
        debug!("constant={:?}", constant);

        match constant {
            Constant::Utf8(s) => {
                w.write_all(&[CONSTANT_UTF8])?;
                w.write_all(&(s.len() as u16).to_be_bytes())?;
                w.write_all(&s.as_bytes())?;
            }
            Constant::ClassInfo(index) => {
                w.write_all(&[CONSTANT_CLASS])?;
                w.write_all(&index.to_be_bytes())?;
            }
            Constant::NameAndType(name, type_descriptor) => {
                w.write_all(&[CONSTANT_NAME_AND_TYPE])?;
                w.write_all(&name.to_be_bytes())?;
                w.write_all(&type_descriptor.to_be_bytes())?;
            }
            Constant::MethodRef(class, name_and_type) => {
                w.write_all(&[CONSTANT_METHODREF])?;
                w.write_all(&class.to_be_bytes())?;
                w.write_all(&name_and_type.to_be_bytes())?;
            }
            Constant::FieldRef(class, name_and_type) => {
                w.write_all(&[CONSTANT_FIELDREF])?;
                w.write_all(&class.to_be_bytes())?;
                w.write_all(&name_and_type.to_be_bytes())?;
            }
            Constant::CString(index) => {
                w.write_all(&[CONSTANT_STRING])?;
                w.write_all(&index.to_be_bytes())?;
            }
            Constant::Int(n) => {
                w.write_all(&[CONSTANT_INTEGER])?;
                w.write_all(&(*n as u32).to_be_bytes())?;
            }
            Constant::Float(n) => {
                w.write_all(&[CONSTANT_FLOAT])?;
                debug!("const float: n={} v={:?}", n, n.to_be_bytes());
                w.write_all(&n.to_be_bytes())?;
            }
            Constant::LongHigh(n) => {
                w.write_all(&[CONSTANT_LONG])?;
                w.write_all(&(*n as u32).to_be_bytes())?;
            }
            // This is always preceded by LongHigh which writes the tag
            Constant::LongLow(n) => {
                w.write_all(&(*n as u32).to_be_bytes())?;
            }
            Constant::Long(_) => unreachable!(),
            Constant::DoubleHigh(n) => {
                w.write_all(&[CONSTANT_DOUBLE])?;
                w.write_all(&(*n as u32).to_be_bytes())?;
            }
            // This is always preceded by DoubleHigh which writes the tag
            Constant::DoubleLow(n) => {
                w.write_all(&(*n as u32).to_be_bytes())?;
            }
            Constant::Double(_) => unreachable!(),
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
                w.write_all(&[ITEM_INTEGER])?;
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
            StackMapFrame::Same { offset } => {
                assert!(*offset <= 63);
                w.write_all(&[*offset])?;
            }
            StackMapFrame::SameLocalsOneStackItem { offset, stack } => {
                assert!(*offset <= 63);
                w.write_all(&[64 + *offset])?;
                self.verification_type_info(stack, w)?;
            }
            StackMapFrame::Full {
                offset,
                stack,
                locals,
            } => {
                debug!("full frame: size={}", entry.size());
                w.write_all(&FULL_FRAME.to_be_bytes())?;
                w.write_all(&offset.to_be_bytes())?;

                w.write_all(&(locals.len() as u16).to_be_bytes())?;
                for v in locals {
                    self.verification_type_info(v, w)?;
                }

                w.write_all(&(stack.len() as u16).to_be_bytes())?;
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
        w.write_all(&(entries.len() as u16).to_be_bytes())?;

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
        w.write_all(&(line_number_tables.len() as u16).to_be_bytes())?;

        for l in line_number_tables {
            w.write_all(&l.start_pc.to_be_bytes())?;
            w.write_all(&l.line_number.to_be_bytes())?;
        }
        Ok(())
    }
}
