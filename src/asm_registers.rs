use log::debug;
use std::fmt;

// x86_64 specific
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum Register {
    Rax = 1,
    Rbx = 2,
    Rcx = 4,
    Rdx = 8,
    Rbp = 16,
    Rsp = 32,
    Rsi = 64,
    Rdi = 128,
    R8 = 256,
    R9 = 512,
    R10 = 1024,
    R11 = 2048,
    R12 = 4096,
    R13 = 8192,
    R14 = 16384,
    R15 = 32768,
}

pub(crate) const REGISTER_RETURN_VALUE: Register = Register::Rax;
pub(crate) const REGISTER_ARG_1: Register = Register::Rdi;
pub(crate) const REGISTER_ARG_2: Register = Register::Rsi;

impl Register {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            Register::Rax => "rax",
            Register::Rbx => "rbx",
            Register::Rcx => "rcx",
            Register::Rdx => "rdx",
            Register::Rbp => "rbp",
            Register::Rsp => "rsp",
            Register::Rsi => "rsi",
            Register::Rdi => "rdi",
            Register::R8 => "r8",
            Register::R9 => "r9",
            Register::R10 => "r10",
            Register::R11 => "r11",
            Register::R12 => "r12",
            Register::R13 => "r13",
            Register::R14 => "r14",
            Register::R15 => "r15",
        }
    }

    pub(crate) fn as_byte_str(&self) -> &'static str {
        match self {
            Register::Rax => "al",
            Register::Rbx => "bl",
            Register::Rcx => "cl",
            Register::Rdx => "dl",
            Register::Rbp => "bl",
            Register::Rsp => "sl",
            Register::Rsi => "sl",
            Register::Rdi => "dl",
            Register::R8 => "8l",
            Register::R9 => "9l",
            Register::R10 => "10l",
            Register::R11 => "11l",
            Register::R12 => "12l",
            Register::R13 => "13l",
            Register::R14 => "14l",
            Register::R15 => "15l",
        }
    }
}

impl From<u16> for Register {
    fn from(i: u16) -> Register {
        match i {
            1 => Register::Rax,
            2 => Register::Rbx,
            4 => Register::Rcx,
            8 => Register::Rdx,
            16 => Register::Rbp,
            32 => Register::Rsp,
            64 => Register::Rsi,
            128 => Register::Rdi,
            256 => Register::R8,
            512 => Register::R9,
            1024 => Register::R10,
            2048 => Register::R11,
            4096 => Register::R12,
            8192 => Register::R13,
            16384 => Register::R14,
            32768 => Register::R15,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug)]
pub(crate) struct Registers {
    in_use: u16,
}

impl Registers {
    pub(crate) fn new() -> Registers {
        Registers { in_use: 0 }
    }

    pub(crate) fn is_free(&self, register: Register) -> bool {
        (self.in_use & (register as u16)) == 0
    }

    pub(crate) fn reserve(&mut self, register: Register) {
        self.in_use |= register as u16;
    }

    pub(crate) fn allocate(&mut self) -> Option<Register> {
        let mut i = 1u16;
        while i <= Register::R15 as u16 {
            // Skip rbp and rsp since they are used for the stack
            if i == Register::Rbp as u16 || i == Register::Rsp as u16 {
                i *= 2;
                continue;
            }

            let register = Register::from(i);
            if self.is_free(register) {
                debug!("allocate: new register={} in_use={}", register, self.in_use);
                self.reserve(register);
                return Some(register);
            }

            i *= 2;
        }
        None
    }

    pub(crate) fn free(&mut self, register: Register) {
        self.in_use &= !(register as u16);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_free_initially() {
        let registers = Registers::new();
        assert!(registers.is_free(Register::Rax));
    }

    #[test]
    fn is_not_free_after_being_reserved() {
        let mut registers = Registers::new();
        registers.reserve(Register::Rax);
        assert!(!registers.is_free(Register::Rax));
    }

    #[test]
    fn is_free_after_being_freed() {
        let mut registers = Registers::new();
        registers.reserve(Register::Rax);
        assert!(!registers.is_free(Register::Rax));

        registers.free(Register::Rax);
        assert!(registers.is_free(Register::Rax));
    }
}
