use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Ins {
    pub typed: InsType,
}

impl Ins {
    pub fn flatten(&self) -> Vec<u8> {
        vec![]
    }
}

impl std::fmt::Debug for Ins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.typed)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Reg),
    U16(u16),
    U32(u32),
    U64(u64),
    UndeterminedMapping(usize),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpirationPolicy {
    None,
    Dest,
    Src,
    Both
}

#[derive(Clone)]
pub struct TwoOperands {
    pub dest: Operand,
    pub src: Operand,
    pub expires: ExpirationPolicy,
}

impl TwoOperands {
    pub fn new(dest: Operand, src: Operand) -> Self {
        TwoOperands {
            dest, src,
            expires: ExpirationPolicy::None,
        }
    }
}

impl std::fmt::Debug for TwoOperands {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}, {:?}, {:?}", self.dest, self.src, self.expires)
    }
}

#[derive(Debug, Clone)]
pub enum InsType {
    Mov(TwoOperands),
    Add(TwoOperands),
    Jmp(usize),
    Call(Rc<str>),
    Ret,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub ins: Vec<Ins>,
}

#[derive(Debug, Clone)]
pub struct Context {
    pub code: Vec<u8>,
    pub data: Vec<u8>,
    pub relocation: Vec<(usize, Rc<str>)>,
}

pub type IsaContexts = HashMap<Rc<str>, Context>;