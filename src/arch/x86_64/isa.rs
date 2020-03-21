use std::collections::HashMap;
use std::rc::Rc;
use crate::ssa::isa::FunctionName;

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

#[derive(Clone)]
pub struct TwoOperands {
    pub dest: Operand,
    pub src: Operand,
}

impl std::fmt::Debug for TwoOperands {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}, {:?}", self.dest, self.src)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct VirtualThreeOperands {
    pub dest : usize,
    pub left : usize,
    pub right: usize,
}

#[derive(Debug, Clone)]
pub enum InsType {
    MovI32(TwoOperands),
    AddI32(TwoOperands),
    SubI32(TwoOperands),
    MulI32(TwoOperands),
    DivI32(TwoOperands),
    Cmp(TwoOperands),
    Jmp(usize),
    Jgt(usize),
    Jlt(usize),
    Call(Rc<FunctionName>),
    Lt(VirtualThreeOperands),
    IfJmp { condvar: Operand, iftrue: usize, iffalse: usize },
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
    pub relocation: Vec<(usize, Rc<FunctionName>)>,
}

pub type IsaContexts = HashMap<Rc<FunctionName>, Context>;