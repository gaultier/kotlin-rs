use crate::parse::Type;

pub(crate) const CTOR_STR: &'static str = "<init>";

pub(crate) const _CLASS_ACC_PUBLIC: u16 = 0x0001; // Declared public; may be accessed from outside its package.
pub(crate) const _CLASS_ACC_FINAL: u16 = 0x0010; // Declared final; no subclasses allowed.
pub(crate) const CLASS_ACC_SUPER: u16 = 0x0020; // Treat superclass methods specially when invoked by the invokespecial instruction.
pub(crate) const _CLASS_ACC_INTERFACE: u16 = 0x0200; // Is an interface, not a class.
pub(crate) const _CLASS_ACC_ABSTRACT: u16 = 0x0400; // Declared abstract; must not be instantiated.
pub(crate) const _CLASS_ACC_SYNTHETIC: u16 = 0x1000; // Declared synthetic; not present in the source code.
pub(crate) const _CLASS_ACC_ANNOTATION: u16 = 0x2000; // Declared as an annotation type.
pub(crate) const _CLASS_ACC_ENUM: u16 = 0x4000; // Declared as an enum type.

pub(crate) const METHOD_ACC_PUBLIC: u16 = 0x0001; // Declared public; may be accessed from outside its package.
pub(crate) const _METHOD_ACC_PRIVATE: u16 = 0x0002; // Declared private; usable only within the defining class.
pub(crate) const _METHOD_ACC_PROTECTED: u16 = 0x0004; // Declared protected; may be accessed within subclasses.
pub(crate) const METHOD_ACC_STATIC: u16 = 0x0008; // Declared static.
pub(crate) const _METHOD_ACC_FINAL: u16 = 0x0010; // Declared final; never directly assigned to after object construction (JLS §17.5).
pub(crate) const _METHOD_ACC_VOLATILE: u16 = 0x0040; // Declared volatile; cannot be cached.
pub(crate) const _METHOD_ACC_TRANSIENT: u16 = 0x0080; // Declared transient; not written or read by a persistent object manager.
pub(crate) const _METHOD_ACC_SYNTHETIC: u16 = 0x1000; // Declared synthetic; not present in the source code.
pub(crate) const _METHOD_ACC_ENUM: u16 = 0x4000; // Declared as an element of an enum.

pub(crate) const CONSTANT_CLASS: u8 = 7;
pub(crate) const CONSTANT_FIELDREF: u8 = 9;
pub(crate) const CONSTANT_METHODREF: u8 = 10;
pub(crate) const _CONSTANT_INTERFACE_METHODREF: u8 = 11;
pub(crate) const CONSTANT_STRING: u8 = 8;
pub(crate) const CONSTANT_INTEGER: u8 = 3;
pub(crate) const CONSTANT_FLOAT: u8 = 4;
pub(crate) const CONSTANT_LONG: u8 = 5;
pub(crate) const _CONSTANT_DOUBLE: u8 = 6;
pub(crate) const CONSTANT_NAME_AND_TYPE: u8 = 12;
pub(crate) const CONSTANT_UTF8: u8 = 1;
pub(crate) const _CONSTANT_METHOD_HANDLE: u8 = 15;
pub(crate) const _CONSTANT_METHOD_TYPE: u8 = 16;
pub(crate) const _CONSTANT_INVOKE_DYNAMIC: u8 = 18;

#[derive(Debug, Clone)]
enum OpCode {
    Return,
    Nop,
    InvokeSpecial(u16),
    GetStatic(u16),
    LoadConstant(u16, Type),
    InvokeVirtual(u16),
    IPush(i32),
    FPush(f32),
    LPush(i64),
    IAdd,
    ISub,
    IMult,
    IDiv,
    FAdd,
    FSub,
    FMult,
    FDiv,
    LAdd,
    LSub,
    LMult,
    LDiv,
    IfEq,
    ILoad(u16),
    IStore(u16),
    FLoad(u16),
    FStore(u16),
    LLoad(u16),
    LStore(u16),
}

impl OpCode {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            OpCode::Return => vec![OP_RETURN],
            OpCode::Nop => vec![OP_NOP],
            OpCode::InvokeSpecial(i) => {
                vec![OP_INVOKE_SPECIAL, i.to_be_bytes()[0], i.to_be_bytes()[1]]
            }
            OpCode::InvokeVirtual(i) => {
                vec![OP_INVOKE_VIRTUAL, i.to_be_bytes()[0], i.to_be_bytes()[1]]
            }
            OpCode::GetStatic(i) => vec![OP_GET_STATIC, i.to_be_bytes()[0], i.to_be_bytes()[2]],
            OpCode::LoadConstant(i, Type::Int) if *i <= std::u8::MAX as u16 => {
                vec![OP_LDC, i.to_be_bytes()[0], i.to_be_bytes()[2]]
            }
            OpCode::LoadConstant(i, Type::Float) if *i <= std::u8::MAX as u16 => {
                vec![OP_LDC, i.to_be_bytes()[0], i.to_be_bytes()[2]]
            }
            OpCode::LoadConstant(i, Type::Long) => {
                vec![OP_LDC2_W, i.to_be_bytes()[0], i.to_be_bytes()[2]]
            }
            OpCode::LoadConstant(i, _) => vec![OP_LDC_W, i.to_be_bytes()[0], i.to_be_bytes()[2]],
            OpCode::IPush(v) => vec![OP_BIPUSH, i.to_be_bytes()[0], i.to_be_bytes()[2]],
            OpCode::GetStatic(i) => vec![0xb2, i.to_be_bytes()[0], i.to_be_bytes()[2]],
            OpCode::GetStatic(i) => vec![0xb2, i.to_be_bytes()[0], i.to_be_bytes()[2]],
        }
    }
}

pub(crate) const OP_RETURN: u8 = 0xb1;
pub(crate) const OP_NOP: u8 = 0x00;
pub(crate) const OP_ALOAD_0: u8 = 0x2a;
pub(crate) const OP_INVOKE_SPECIAL: u8 = 0xb7;
pub(crate) const OP_GET_STATIC: u8 = 0xb2;
pub(crate) const OP_LDC: u8 = 0x12;
pub(crate) const OP_LDC_W: u8 = 0x13;
pub(crate) const OP_LDC2_W: u8 = 0x14;
pub(crate) const OP_INVOKE_VIRTUAL: u8 = 0xb6;
pub(crate) const OP_ICONST_M1: u8 = 0x02;
pub(crate) const OP_ICONST_0: u8 = 0x03;
pub(crate) const OP_ICONST_1: u8 = 0x04;
pub(crate) const OP_ICONST_2: u8 = 0x05;
pub(crate) const OP_ICONST_3: u8 = 0x06;
pub(crate) const OP_ICONST_4: u8 = 0x07;
pub(crate) const OP_ICONST_5: u8 = 0x08;
pub(crate) const OP_BIPUSH: u8 = 0x10;
pub(crate) const OP_SIPUSH: u8 = 0x11;
pub(crate) const OP_INEG: u8 = 0x74;
pub(crate) const OP_IADD: u8 = 0x60;
pub(crate) const OP_IMUL: u8 = 0x68;
pub(crate) const OP_ISUB: u8 = 0x64;
pub(crate) const OP_IDIV: u8 = 0x6c;
pub(crate) const OP_LADD: u8 = 0x61;
pub(crate) const OP_LSUB: u8 = 0x65;
pub(crate) const OP_LMUL: u8 = 0x69;
pub(crate) const OP_LDIV: u8 = 0x6d;
pub(crate) const OP_LCONST_0: u8 = 0x09;
pub(crate) const OP_LCONST_1: u8 = 0x0a;
pub(crate) const _OP_I2L: u8 = 0x85;
pub(crate) const OP_FCONST_0: u8 = 0x0b;
pub(crate) const OP_FCONST_1: u8 = 0x0c;
pub(crate) const OP_FCONST_2: u8 = 0x0d;
pub(crate) const OP_FADD: u8 = 0x62;
pub(crate) const OP_FSUB: u8 = 0x66;
pub(crate) const OP_FMUL: u8 = 0x6a;
pub(crate) const OP_FDIV: u8 = 0x6e;
pub(crate) const _OP_IFNE: u8 = 0x9a;
pub(crate) const OP_IFEQ: u8 = 0x99;
pub(crate) const OP_GOTO: u8 = 0xa7;
pub(crate) const _OP_ISTORE_0: u8 = 0x3b;
pub(crate) const _OP_ILOAD_0: u8 = 0x1a;
pub(crate) const OP_ILOAD: u8 = 0x15;
pub(crate) const OP_FLOAD: u8 = 0x17;
pub(crate) const OP_LLOAD: u8 = 0x16;
pub(crate) const OP_ISTORE: u8 = 0x36;
pub(crate) const OP_FSTORE: u8 = 0x38;
pub(crate) const OP_LSTORE: u8 = 0x37;
pub(crate) const _OP_POP: u8 = 0x57;

pub(crate) const OP_IMPDEP1: u8 = 0xfe;
pub(crate) const OP_IMPDEP2: u8 = 0xff;

pub(crate) const ITEM_INTEGER: u8 = 1;

pub(crate) const FULL_FRAME: u8 = 255;
