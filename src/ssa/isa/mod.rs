use crate::utils::uniquerc::UniqueRc;
use std::collections::{BTreeSet, HashMap};
use std::fmt::Write;
use std::hash::Hash;
use std::rc::Rc;

mod types;
pub use types::*;

mod print;

#[derive(Debug, Clone, PartialEq)]
pub enum IntrinsicType {
    None,
    Extern(String),
}

impl IntrinsicType {
    pub fn is_none(&self) -> bool {
        self == &IntrinsicType::None
    }
}

pub type Variable = usize;

#[derive(Debug, Clone, Copy)]
pub struct InsPosition {
    pub block_idx: usize,
    pub ins_idx: usize,
}

#[derive(Clone)]
pub struct Context {
    pub blocks: Vec<Block>,
    pub variables: Vec<Type>,
    pub name: Rc<str>,
    pub args: Vec<Type>,
    pub rettype: Type,
    pub intrinsic: IntrinsicType,
}

impl Context {
    pub fn new(name: Rc<str>) -> Self {
        Context {
            blocks: vec![],
            variables: vec![],
            name,
            args: vec![],
            rettype: Type::NoReturn,
            intrinsic: IntrinsicType::None,
        }
    }

    pub fn with_args(name: Rc<str>, args: Vec<Type>) -> Self {
        Context {
            variables: args.clone(),
            blocks: vec![Block::new(
                args.iter()
                    .enumerate()
                    .map(|(idx, _)| Ins {
                        retvar: idx,
                        typed: InsType::LoadArg(idx),
                    })
                    .collect(),
            )],
            name,
            args,
            rettype: Type::NoReturn,
            intrinsic: IntrinsicType::None,
        }
    }

    pub fn with_intrinsics(
        name: Rc<str>,
        args: Vec<Type>,
        rettype: Type,
        intrinsic: IntrinsicType,
    ) -> Self {
        Context {
            variables: vec![],
            blocks: vec![],
            name,
            args,
            rettype,
            intrinsic,
        }
    }

    pub fn insert_var(&mut self, typed: Type) -> Variable {
        self.variables.push(typed);
        self.variables.len() - 1
    }

    pub fn new_block(&mut self) -> usize {
        self.blocks.push(Block::new(vec![]));
        self.blocks.len() - 1
    }

    pub fn block(&self) -> &Block {
        self.blocks.last().unwrap()
    }

    pub fn block_mut(&mut self) -> &mut Block {
        self.blocks.last_mut().unwrap()
    }

    pub fn print(&self) -> print::ContextPrinter {
        print::ContextPrinter(None, self)
    }
}

#[derive(Clone)]
pub struct Block {
    pub ins: Vec<Ins>,
    pub postlude: Ins,
    pub preds: Vec<usize>,
    pub succs: Vec<usize>,
    pub vars_in: BTreeSet<Variable>,
    pub vars_out: BTreeSet<Variable>,
    pub vars_declared_in_this_block: BTreeSet<Variable>,
    pub vars_used: BTreeSet<Variable>,
    pub vars_block_local: BTreeSet<Variable>,
}

impl Block {
    pub fn new(ins: Vec<Ins>) -> Self {
        Block {
            ins,
            postlude: Ins::new(0, InsType::Nop),
            preds: vec![],
            succs: vec![],
            vars_in: BTreeSet::new(),
            vars_out: BTreeSet::new(),
            vars_declared_in_this_block: BTreeSet::new(),
            vars_used: BTreeSet::new(),
            vars_block_local: BTreeSet::new(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ins {
    retvar: Variable,
    pub typed: InsType,
}

impl Ins {
    pub fn new(retvar: Variable, typed: InsType) -> Self {
        Ins { retvar, typed }
    }

    pub fn retvar(&self) -> Option<Variable> {
        if self.typed.has_retvar() {
            Some(self.retvar)
        } else {
            None
        }
    }

    pub fn mut_retvar(&mut self) -> Option<&mut Variable> {
        if self.typed.has_retvar() {
            Some(&mut self.retvar)
        } else {
            None
        }
    }

    pub fn rename_var_by<T>(&mut self, mut swap: T)
    where
        T: FnMut(Variable) -> Variable,
    {
        fn swap_indices<T>(mut indices: Vec<MemberExprIndex>, swap: &mut T) -> Vec<MemberExprIndex>
        where
            T: FnMut(Variable) -> Variable,
        {
            for idx in indices.iter_mut() {
                match idx {
                    MemberExprIndex {
                        var: MemberExprIndexVar::Variable(var),
                        ..
                    } => {
                        *var = swap(*var);
                    }
                    _ => (),
                }
            }
            indices
        }
        let old_typed = std::mem::replace(&mut self.typed, InsType::Nop);
        let new_typed = match old_typed {
            InsType::Move(x) => InsType::Move(swap(x)),
            InsType::MarkMoved(x) => InsType::MarkMoved(swap(x)),
            InsType::Drop(x) => InsType::Drop(swap(x)),
            InsType::Copy(x) => InsType::Copy(swap(x)),
            InsType::LoadSlice(mut args) => {
                for arg in &mut args {
                    let oldvar = *arg;
                    *arg = swap(oldvar);
                }
                InsType::LoadSlice(args)
            }
            InsType::Call { name, mut args } => {
                for arg in &mut args {
                    let oldvar = *arg;
                    *arg = swap(oldvar);
                }
                InsType::Call { name, args }
            }
            InsType::Return(x) => InsType::Return(swap(x)),
            InsType::Phi { .. } => unreachable!(),
            InsType::MemberReference {
                left,
                indices,
                modifier,
            } => InsType::MemberReference {
                left: swap(left),
                indices: swap_indices(indices, &mut swap),
                modifier,
            },
            InsType::MemberReferenceStore {
                left,
                indices,
                modifier,
                right,
            } => InsType::MemberReferenceStore {
                left: swap(left),
                indices: swap_indices(indices, &mut swap),
                modifier,
                right: swap(right),
            },
            InsType::Add((x, y)) => InsType::Add((swap(x), swap(y))),
            InsType::Sub((x, y)) => InsType::Sub((swap(x), swap(y))),
            InsType::Mul((x, y)) => InsType::Mul((swap(x), swap(y))),
            InsType::Div((x, y)) => InsType::Div((swap(x), swap(y))),
            InsType::Mod((x, y)) => InsType::Mod((swap(x), swap(y))),
            InsType::Lt((x, y)) => InsType::Lt((swap(x), swap(y))),
            InsType::Gt((x, y)) => InsType::Gt((swap(x), swap(y))),
            InsType::Lte((x, y)) => InsType::Lte((swap(x), swap(y))),
            InsType::Gte((x, y)) => InsType::Gte((swap(x), swap(y))),
            InsType::Equ((x, y)) => InsType::Equ((swap(x), swap(y))),
            InsType::AddC(_)
            | InsType::SubC(_)
            | InsType::MulC(_)
            | InsType::DivC(_)
            | InsType::ModC(_)
            | InsType::LtC(_)
            | InsType::GtC(_)
            | InsType::LteC(_)
            | InsType::GteC(_) => unimplemented!(),
            InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => InsType::IfJmp {
                condvar: swap(condvar),
                iftrue,
                iffalse,
            },
            InsType::Cast { var, typed } => InsType::Cast {
                var: swap(var),
                typed,
            },
            other => other,
        };
        std::mem::replace(&mut self.typed, new_typed);
    }

    pub fn rename_var(&mut self, oldvar: Variable, newvar: Variable) {
        self.rename_var_by(|var| if var == oldvar { newvar } else { var })
    }

    pub fn each_used_var<T>(&self, mut callback: T)
    where
        T: FnMut(Variable),
    {
        fn each_indices<T>(indices: &Vec<MemberExprIndex>, callback: &mut T)
        where
            T: FnMut(Variable),
        {
            for idx in indices {
                match idx {
                    MemberExprIndex {
                        var: MemberExprIndexVar::Variable(var),
                        ..
                    } => {
                        callback(*var);
                    }
                    _ => (),
                }
            }
        }
        match &self.typed {
            InsType::Move(x) | InsType::MarkMoved(x) | InsType::Drop(x) | InsType::Copy(x) => {
                callback(*x);
            }
            InsType::LoadSlice(args) => {
                for arg in args {
                    callback(*arg);
                }
            }
            InsType::Call { name: _, args } => {
                for arg in args {
                    callback(*arg);
                }
            }
            InsType::Return(x) => {
                callback(*x);
            }
            InsType::Phi { vars, .. } => {
                for arg in vars {
                    callback(*arg);
                }
            }
            InsType::MemberReference { left, indices, .. } => {
                callback(*left);
                each_indices(indices, &mut callback);
            }
            InsType::MemberReferenceStore {
                left,
                indices,
                right,
                ..
            } => {
                callback(*left);
                each_indices(indices, &mut callback);
                callback(*right);
            }
            InsType::Add((x, y))
            | InsType::Sub((x, y))
            | InsType::Mul((x, y))
            | InsType::Div((x, y))
            | InsType::Mod((x, y))
            | InsType::Lt((x, y))
            | InsType::Gt((x, y))
            | InsType::Lte((x, y))
            | InsType::Gte((x, y))
            | InsType::Equ((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::IfJmp { condvar, .. } => {
                callback(*condvar);
            }
            InsType::Cast { var, .. } => {
                callback(*var);
            }
            InsType::AddC(rc)
            | InsType::SubC(rc)
            | InsType::MulC(rc)
            | InsType::DivC(rc)
            | InsType::ModC(rc)
            | InsType::LtC(rc)
            | InsType::GtC(rc)
            | InsType::LteC(rc)
            | InsType::GteC(rc) => match rc {
                RegConst::RegLeft((reg, _)) => callback(*reg),
                RegConst::RegRight((_, reg)) => callback(*reg),
            },
            _ => (),
        }
    }

    pub fn each_moved_var<T>(&self, mut callback: T) -> bool
    where
        T: FnMut(Variable),
    {
        match &self.typed {
            InsType::Drop(var) | InsType::Move(var) | InsType::MarkMoved(var) => {
                callback(*var);
                true
            }
            InsType::Phi { vars, .. } => {
                for var in vars {
                    callback(*var);
                }
                true
            }
            _ => false,
        }
    }

    pub fn print(&self) -> print::InsPrinter {
        print::InsPrinter(self)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Constant {
    Bool(bool),
    I32(i32),
    I64(i64),
    F64(u64),
}

impl Constant {
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Constant::I32(x) => Some(*x as i64),
            Constant::I64(x) => Some(*x),
            _ => None,
        }
    }

    pub fn add(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::I32(x + y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::I64(x + y)),
            (Constant::F64(x), Constant::F64(y)) => Some(Constant::F64(f64::to_bits(
                f64::from_bits(x) + f64::from_bits(y),
            ))),
            (_, _) => None,
        }
    }

    pub fn sub(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::I32(x - y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::I64(x - y)),
            (Constant::F64(x), Constant::F64(y)) => Some(Constant::F64(f64::to_bits(
                f64::from_bits(x) - f64::from_bits(y),
            ))),
            (_, _) => None,
        }
    }

    pub fn mul(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::I32(x * y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::I64(x * y)),
            (Constant::F64(x), Constant::F64(y)) => Some(Constant::F64(f64::to_bits(
                f64::from_bits(x) * f64::from_bits(y),
            ))),
            (_, _) => None,
        }
    }

    pub fn div(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::I32(x / y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::I64(x / y)),
            (Constant::F64(x), Constant::F64(y)) => Some(Constant::F64(f64::to_bits(
                f64::from_bits(x) + f64::from_bits(y),
            ))),
            (_, _) => None,
        }
    }

    pub fn imod(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::I32(x % y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::I64(x % y)),
            (_, _) => None,
        }
    }

    pub fn lt(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::Bool(x < y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::Bool(x < y)),
            (Constant::F64(x), Constant::F64(y)) => {
                Some(Constant::Bool(f64::from_bits(x) < f64::from_bits(y)))
            }
            (_, _) => None,
        }
    }

    pub fn gt(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::Bool(x > y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::Bool(x > y)),
            (Constant::F64(x), Constant::F64(y)) => {
                Some(Constant::Bool(f64::from_bits(x) > f64::from_bits(y)))
            }
            (_, _) => None,
        }
    }

    pub fn lte(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::Bool(x <= y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::Bool(x <= y)),
            (Constant::F64(x), Constant::F64(y)) => {
                Some(Constant::Bool(f64::from_bits(x) <= f64::from_bits(y)))
            }
            (_, _) => None,
        }
    }

    pub fn gte(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::Bool(x >= y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::Bool(x >= y)),
            (Constant::F64(x), Constant::F64(y)) => {
                Some(Constant::Bool(f64::from_bits(x) >= f64::from_bits(y)))
            }
            (_, _) => None,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum RegConst {
    RegLeft((Variable, Constant)),
    RegRight((Constant, Variable)),
}

impl RegConst {
    pub fn as_reg_left(&self) -> Option<(Variable, Constant)> {
        match self {
            RegConst::RegLeft(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_reg_right(&self) -> Option<(Constant, Variable)> {
        match self {
            RegConst::RegRight(x) => Some(*x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TrapType {
    BoundsCheck,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum MemberExprIndexVar {
    StructIndex(usize),
    Variable(usize),
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct MemberExprIndex {
    pub var: MemberExprIndexVar,
    pub typed: Type,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum ReferenceModifier {
    Copy,
    Move,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum InsType {
    Nop,
    LoadNil,
    Drop(Variable),
    MarkMoved(Variable),
    Move(Variable),
    Copy(Variable),
    LoadArg(usize),
    LoadI32(i32),
    LoadI64(i64),
    LoadF64(u64),
    LoadBool(bool),
    LoadSubstring(Rc<str>),
    LoadSlice(Vec<Variable>),
    LoadStruct,
    MemberReference {
        left: Variable,
        indices: Vec<MemberExprIndex>,
        modifier: ReferenceModifier,
    },
    MemberReferenceStore {
        left: Variable,
        indices: Vec<MemberExprIndex>,
        modifier: ReferenceModifier,
        right: Variable,
    },
    Phi {
        vars: Vec<Variable>,
        defines: Variable,
    },
    Call {
        name: Rc<FunctionName>,
        args: Vec<Variable>,
    },
    Return(Variable),
    Trap(TrapType),
    Exit,
    Add((Variable, Variable)),
    Sub((Variable, Variable)),
    Mul((Variable, Variable)),
    Div((Variable, Variable)),
    Mod((Variable, Variable)),
    Lt((Variable, Variable)),
    Gt((Variable, Variable)),
    Lte((Variable, Variable)),
    Gte((Variable, Variable)),
    Equ((Variable, Variable)),
    Neq((Variable, Variable)),
    AddC(RegConst),
    SubC(RegConst),
    MulC(RegConst),
    DivC(RegConst),
    ModC(RegConst),
    LtC(RegConst),
    GtC(RegConst),
    LteC(RegConst),
    GteC(RegConst),
    Cast {
        var: Variable,
        typed: Type,
    },
    IfJmp {
        condvar: Variable,
        iftrue: usize,
        iffalse: usize,
    },
    Jmp(usize),
}

impl InsType {
    pub fn load_const(c: Constant) -> Self {
        match c {
            Constant::Bool(x) => InsType::LoadBool(x),
            Constant::I32(x) => InsType::LoadI32(x),
            Constant::I64(x) => InsType::LoadI64(x),
            Constant::F64(x) => InsType::LoadF64(x),
        }
    }

    pub fn is_jmp(&self) -> bool {
        match self {
            InsType::IfJmp { .. }
            | InsType::Jmp(_)
            | InsType::Trap(_)
            | InsType::Return(_)
            | InsType::Exit => true,
            _ => false,
        }
    }

    pub fn has_retvar(&self) -> bool {
        match self {
            typed if typed.is_jmp() => false,
            InsType::MarkMoved(_) => false,
            InsType::Drop(_) => false,
            InsType::MemberReferenceStore { .. } => false,
            _ => true,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            InsType::LoadNil
            | InsType::LoadI32(_)
            | InsType::LoadI64(_)
            | InsType::LoadF64(_)
            | InsType::LoadBool(_)
            | InsType::LoadSubstring(_) => true,
            _ => false,
        }
    }

    pub fn to_const(&self) -> Option<Constant> {
        match self {
            InsType::LoadI32(k) => Some(Constant::I32(*k)),
            InsType::LoadI64(k) => Some(Constant::I64(*k)),
            InsType::LoadF64(k) => Some(Constant::F64(*k)),
            _ => None,
        }
    }

    pub fn const_cast(&self, typed: &Type) -> Option<InsType> {
        match (self, typed) {
            (InsType::LoadI32(_), Type::I32) => Some(self.clone()),
            (InsType::LoadI32(n), Type::I64) => Some(InsType::LoadI64(*n as i64)),
            (InsType::LoadI64(_), Type::I64) => Some(self.clone()),
            (InsType::LoadI64(n), Type::I32) => Some(InsType::LoadI32(*n as i32)),
            _ => None,
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            InsType::MemberReferenceStore { .. }
            | InsType::Call { .. }
            | InsType::Return(_)
            | InsType::Exit => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            InsType::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_phi(&self) -> bool {
        match self {
            InsType::Phi { .. } => true,
            _ => false,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct FunctionName {
    pub name: Rc<str>,
    pub arg_types: Vec<Type>,
}

impl ToString for FunctionName {
    fn to_string(&self) -> String {
        let mut string = self.name.clone().to_string() + "(";
        string += &self
            .arg_types
            .iter()
            .map(|typed| {
                let mut string = String::new();
                write!(&mut string, "{}", typed).unwrap();
                string
            })
            .collect::<Vec<String>>()
            .join(", ");
        string += ")";
        string
    }
}

pub struct Builtins {
    pub structs: HashMap<Rc<str>, UniqueRc<StructType>>,
    pub generic_fat_pointer_struct: Rc<StructType>,
}

pub struct Program {
    pub contexts: HashMap<Rc<FunctionName>, Context>,
    pub entry: Rc<FunctionName>,
    pub builtins: Builtins,
}

impl Program {
    pub fn print(&self) -> print::ProgramPrinter {
        print::ProgramPrinter(self)
    }
}
