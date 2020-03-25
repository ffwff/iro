use crate::ssa::isa::FunctionName;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Reg {
    Rax = 0,
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
    R15,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Reg),
    U16(u16),
    U32(u32),
    U64(u64),
    Memory { disp: i32, base: Reg },
    UndeterminedMapping(usize),
}

impl Operand {
    pub fn is_lit(&self) -> bool {
        match self {
            Operand::U16(_) | Operand::U32(_) | Operand::U64(_) => true,
            _ => false,
        }
    }
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
    pub dest: usize,
    pub left: usize,
    pub right: usize,
}

#[derive(Debug, Clone)]
pub enum Ins {
    MovI64(TwoOperands),
    MovI32(TwoOperands),
    AddI32(TwoOperands),
    SubI32(TwoOperands),
    MulI32(TwoOperands),
    DivI32(TwoOperands),
    CmpI32 {
        ops: TwoOperands,
        is_postlude: bool,
    },
    Jmp(usize),
    Jgt(usize),
    Jge(usize),
    Jlt(usize),
    Jle(usize),
    Call(Rc<FunctionName>),
    Ret,
    Push(Operand),
    Enter {
        local_size: u32,
        save_regs: Vec<Reg>,
    },
    LeaveAndRet {
        save_regs: Vec<Reg>,
    },
    Clobber(Vec<(Reg, Option<usize>)>),
    Unclobber(Vec<(Reg, Option<usize>)>),
    Gt(VirtualThreeOperands),
    Lt(VirtualThreeOperands),
    Gte(VirtualThreeOperands),
    Lte(VirtualThreeOperands),
    IfJmp {
        condvar: Operand,
        iftrue: usize,
        iffalse: usize,
    },
}

impl Ins {
    pub fn each_used_var<T>(&self, dest_first: bool, mut callback: T)
    where
        T: FnMut(&Operand),
    {
        match self {
            Ins::MovI32(ops)
            | Ins::AddI32(ops)
            | Ins::SubI32(ops)
            | Ins::MulI32(ops)
            | Ins::DivI32(ops)
            | Ins::CmpI32 { ops, .. } => {
                if dest_first {
                    callback(&ops.dest);
                    callback(&ops.src);
                } else {
                    callback(&ops.src);
                    callback(&ops.dest);
                }
            }
            Ins::Clobber(clobbers) | Ins::Unclobber(clobbers) => {
                for (reg, _) in clobbers {
                    let operand = Operand::Register(*reg);
                    callback(&operand);
                }
            }
            _ => (),
        }
    }

    pub fn rename_var_by<T>(&mut self, dest_first: bool, mut callback: T)
    where
        T: FnMut(&mut Operand),
    {
        match self {
            Ins::MovI32(ops)
            | Ins::AddI32(ops)
            | Ins::SubI32(ops)
            | Ins::MulI32(ops)
            | Ins::DivI32(ops)
            | Ins::CmpI32 { ops, .. } => {
                if dest_first {
                    callback(&mut ops.dest);
                    callback(&mut ops.src);
                } else {
                    callback(&mut ops.src);
                    callback(&mut ops.dest);
                }
            }
            _ => (),
        }
    }

    pub fn is_jmp(&self) -> bool {
        match self {
            Ins::Jmp(_) | Ins::Jgt(_) | Ins::Jlt(_) | Ins::Jge(_) | Ins::Jle(_) => true,
            _ => false,
        }
    }

    pub fn is_postlude(&self) -> bool {
        if self.is_jmp() {
            return true;
        }
        match self {
            Ins::CmpI32 { is_postlude, .. } => *is_postlude,
            _ => false,
        }
    }

    pub fn is_ret(&self) -> bool {
        match self {
            Ins::Ret => true,
            _ => false,
        }
    }

    pub fn mov_size(&self) -> Option<usize> {
        match self {
            Ins::MovI32(_) => Some(4),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub ins: Vec<Ins>,
}
