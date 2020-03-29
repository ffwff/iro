use crate::ssa::isa::FunctionName;
use std::rc::Rc;
use std::collections::{BTreeMap, BTreeSet};

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

#[derive(Debug, Clone, PartialEq)]
pub enum OperandSize {
    I32,
    I64,
}

impl OperandSize {
    pub fn size(&self) -> usize {
        match self {
            OperandSize::I32 => 4,
            OperandSize::I64 => 8,
        }
    }
}

#[derive(Clone)]
pub struct TwoOperands {
    pub dest: Operand,
    pub src: Operand,
    pub size: OperandSize,
}

impl std::fmt::Debug for TwoOperands {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}> {:?}, {:?}", self.size, self.dest, self.src)?;
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
    Mov(TwoOperands),
    Add(TwoOperands),
    Sub(TwoOperands),
    IMul(TwoOperands),
    IDiv(TwoOperands),
    Mul(TwoOperands),
    Div(TwoOperands),
    Cmp {
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
    InlineBytes(Vec<u8>),
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
        if let Some(ops) = self.get_two_operands() {
            if dest_first {
                callback(&ops.dest);
                callback(&ops.src);
            } else {
                callback(&ops.src);
                callback(&ops.dest);
            }
            return;
        }
        match self {
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
        T: FnMut(&mut Operand, &mut Operand),
    {
        if let Some(ops) = self.get_two_operands_mut() {
            if dest_first {
                callback(&mut ops.dest, &mut ops.src);
                callback(&mut ops.src, &mut ops.dest);
            } else {
                callback(&mut ops.src, &mut ops.dest);
                callback(&mut ops.dest, &mut ops.src);
            }
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
            Ins::Cmp { is_postlude, .. } => *is_postlude,
            _ => false,
        }
    }

    pub fn is_ret(&self) -> bool {
        match self {
            Ins::Ret => true,
            _ => false,
        }
    }

    pub fn get_two_operands<'a>(&'a self) -> Option<&'a TwoOperands> {
        match self {
            Ins::Mov(ops)
            | Ins::Add(ops)
            | Ins::Sub(ops)
            | Ins::IMul(ops)
            | Ins::IDiv(ops)
            | Ins::Mul(ops)
            | Ins::Div(ops)
            | Ins::Cmp { ops, .. } => Some(ops),
            _ => None,
        }
    }

    pub fn get_two_operands_mut<'a>(&'a mut self) -> Option<&'a mut TwoOperands> {
        match self {
            Ins::Mov(ops)
            | Ins::Add(ops)
            | Ins::Sub(ops)
            | Ins::IMul(ops)
            | Ins::IDiv(ops)
            | Ins::Mul(ops)
            | Ins::Div(ops)
            | Ins::Cmp { ops, .. } => Some(ops),
            _ => None,
        }
    }

    pub fn is_mov(&self) -> bool {
        self.mov_size().is_some()
    }

    pub fn mov_size(&self) -> Option<usize> {
        match self {
            Ins::Mov(ops) => Some(ops.size.size()),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Block {
    pub ins: Vec<Ins>,
    pub postlude: Vec<Ins>,
    pub expected_in_vars: (usize, BTreeSet<(usize, Reg)>),
    pub possible_in_vars: Vec<(usize, BTreeSet<(usize, Reg)>)>,
    pub vars_out_to_reg: BTreeMap<usize, Reg>,
}

impl Block {
    pub fn new(ins: Vec<Ins>) -> Self {
        Block {
            ins,
            postlude: vec![],
            expected_in_vars: (0, btreeset![]),
            possible_in_vars: vec![],
            vars_out_to_reg: BTreeMap::new(),
        }
    }
}

impl std::fmt::Debug for Block {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_list()
            .entries(self.ins.iter().map(|ins| InsPrettifier { ins }))
            .finish()?;
        writeln!(fmt)?;
        fmt.debug_list()
            .entries(self.postlude.iter().map(|ins| InsPrettifier { ins }))
            .finish()?;
        write!(fmt, "\nexpected in: {:?}\npossible in: {:?}\nvars_out_to_reg: {:?}\n---", self.expected_in_vars, self.possible_in_vars, self.vars_out_to_reg)
    }
}

struct InsPrettifier<'a> {
    pub(self) ins: &'a Ins,
}

impl<'a> std::fmt::Debug for InsPrettifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ins)
    }
}
