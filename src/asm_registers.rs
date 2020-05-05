use log::debug;
use std::fmt;

// x86_64 specific
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum Register {
    Rax = 1 << 0,
    Rbx = 1 << 1,
    Rcx = 1 << 2,
    Rdx = 1 << 3,
    Rbp = 1 << 4,
    Rsp = 1 << 5,
    Rsi = 1 << 6,
    Rdi = 1 << 7,
    R8 = 1 << 8,
    R9 = 1 << 9,
    R10 = 1 << 10,
    R11 = 1 << 11,
    R12 = 1 << 12,
    R13 = 1 << 13,
    R14 = 1 << 14,
    R15 = 1 << 15,
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
            Register::Rbp => "bpl",
            Register::Rsp => "spl",
            Register::Rsi => "sil",
            Register::Rdi => "dil",
            Register::R8 => "r8b",
            Register::R9 => "r9b",
            Register::R10 => "r10b",
            Register::R11 => "r11b",
            Register::R12 => "r12b",
            Register::R13 => "r13b",
            Register::R14 => "r14b",
            Register::R15 => "r15b",
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

impl fmt::Display for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut i = 1;
        while i < (1 << 15) {
            let register = Register::from(i);
            if !self.is_free(register) {
                write!(f, "{} ", register)?;
            }
            i = i << 1;
        }
        Ok(())
    }
}

impl Registers {
    pub(crate) fn new() -> Registers {
        Registers { in_use: 0 }
    }

    pub(crate) fn is_free(&self, register: Register) -> bool {
        (self.in_use & (register as u16)) == 0
    }

    pub(crate) fn register_fn_arg(i: u16) -> Option<Register> {
        match i {
            1 => Some(Register::Rdi),
            2 => Some(Register::Rsi),
            3 => Some(Register::Rdx),
            4 => Some(Register::Rcx),
            5 => Some(Register::R8),
            6 => Some(Register::R9),
            _ => None,
        }
    }

    pub(crate) fn reserve(&mut self, register: Register) {
        self.in_use |= register as u16;
    }

    pub(crate) fn allocate(&mut self) -> Option<Register> {
        let mut i = 32768;
        while i != 0 {
            // Skip rbp and rsp since they are used for the stack
            if i == Register::Rbp as u16 || i == Register::Rsp as u16 {
                i = i >> 1;
                continue;
            }

            let register = Register::from(i);
            if self.is_free(register) {
                debug!("allocate: new register={} in_use={}", register, self.in_use);
                self.reserve(register);
                return Some(register);
            }

            i = i >> 1;
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
