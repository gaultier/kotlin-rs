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
pub(crate) const _CONSTANT_INTEGER: u8 = 3;
pub(crate) const _CONSTANT_FLOAT: u8 = 4;
pub(crate) const _CONSTANT_LONG: u8 = 5;
pub(crate) const _CONSTANT_DOUBLE: u8 = 6;
pub(crate) const CONSTANT_NAME_AND_TYPE: u8 = 12;
pub(crate) const CONSTANT_UTF8: u8 = 1;
pub(crate) const _CONSTANT_METHOD_HANDLE: u8 = 15;
pub(crate) const _CONSTANT_METHOD_TYPE: u8 = 16;
pub(crate) const _CONSTANT_INVOKE_DYNAMIC: u8 = 18;

pub(crate) const OP_RETURN: u8 = 0xb1;
pub(crate) const _OP_NOP: u8 = 0x00;
pub(crate) const OP_ALOAD_0: u8 = 0x2a;
pub(crate) const OP_INVOKE_SPECIAL: u8 = 0xb7;
