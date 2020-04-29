use std::fmt;

// x86_64 specific
#[derive(Debug, PartialEq, Eq)]
enum Register {
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
        match self {
            Register::Rax => f.write_str("rax"),
            Register::Rbx => f.write_str("rbx"),
            Register::Rcx => f.write_str("rcx"),
            Register::Rdx => f.write_str("rdx"),
            Register::Rbp => f.write_str("rbp"),
            Register::Rsp => f.write_str("rsp"),
            Register::Rsi => f.write_str("rsi"),
            Register::Rdi => f.write_str("rdi"),
            Register::R8 => f.write_str("r8"),
            Register::R9 => f.write_str("r9"),
            Register::R10 => f.write_str("r10"),
            Register::R11 => f.write_str("r11"),
            Register::R12 => f.write_str("r12"),
            Register::R13 => f.write_str("r13"),
            Register::R14 => f.write_str("r14"),
            Register::R15 => f.write_str("r15"),
        }
    }
}

#[derive(Debug)]
struct Registers {
    in_use: u16,
}

impl Registers {
    pub(crate) fn allocate(&mut self) -> Option<Register> {
        let mut i = 0u16;
        while i <= Register::R15 as u16 {
            if (self.in_use & i) == 0 {
                self.in_use |= i;
                return Some(Register::from(i));
            }

            i *= 2;
        }
        None
    }
}
