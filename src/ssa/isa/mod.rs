use crate::utils::uniquerc::UniqueRc;
use std::collections::BTreeSet;
use std::convert::TryInto;
use std::fmt::Write;
use std::hash::Hash;
use std::rc::Rc;
use fnv::FnvHashMap;
use smallvec::SmallVec;

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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(u32);

impl Variable {
    pub fn with_u32(x: u32) -> Self {
        Self(x)
    }

    pub fn from<T: TryInto<u32>>(x: T) -> Self {
        if let Ok(x) = x.try_into() {
            Self(x)
        } else {
            unreachable!("overflow error")
        }
    }
}

impl std::fmt::Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Variable> for u32 {
    fn from(x: Variable) -> u32 {
        x.0
    }
}

impl From<Variable> for usize {
    fn from(x: Variable) -> usize {
        x.0 as usize
    }
}

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
    pub call_names: Vec<Rc<FunctionName>>,
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
            call_names: vec![],
        }
    }

    pub fn with_args(name: Rc<str>, args: Vec<Type>) -> Self {
        Context {
            variables: args.clone(),
            blocks: vec![Block::new(
                args.iter()
                    .enumerate()
                    .map(|(idx, _)| {
                        Ins::new(Variable::from(idx), InsType::LoadArg(idx), std::u32::MAX)
                    })
                    .collect(),
            )],
            name,
            args,
            rettype: Type::NoReturn,
            intrinsic: IntrinsicType::None,
            call_names: vec![],
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
            call_names: vec![],
        }
    }

    pub fn insert_var<'a>(&'a mut self, typed: Type) -> Variable {
        self.variables.push(typed);
        Variable::with_u32((self.variables.len() - 1).try_into().unwrap())
    }

    pub fn variable<'a>(&'a self, idx: Variable) -> &'a Type {
        &self.variables[usize::from(idx)]
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

    pub fn call_name(&mut self, name: Rc<FunctionName>) -> u32 {
        let len = self.call_names.len();
        self.call_names.push(name);
        len as u32
    }

    pub fn print(&self) -> print::ContextPrinter {
        print::ContextPrinter(None, self)
    }
}

#[derive(Clone)]
pub struct Block {
    /// List of instructions in this block
    pub ins: Vec<Ins>,
    /// Temporary variable to store the postlude of this block
    pub postlude: Option<Ins>,
    /// Predecessors of the block in the context graph
    pub preds: SmallVec<[usize; 2]>,
    /// Successors of the block in the context graph
    pub succs: SmallVec<[usize; 2]>,
    /// Variables declared in this block
    pub vars_declared_in_this_block: BTreeSet<Variable>,
    /// Variables used in this block
    pub vars_used: BTreeSet<Variable>,
    /// Phi assignments in this block
    pub vars_phi: BTreeSet<Variable>,
    /// Variables that flow into this block
    pub vars_imported: BTreeSet<Variable>,
    /// All variables that flow into this block, regardless of if it's used or not
    pub vars_total_imported: BTreeSet<Variable>,
    /// Variables that flow into this block or are declared in the block,
    /// and flow out into successor blocks
    pub vars_exported: BTreeSet<Variable>,
}

impl Block {
    pub fn new(ins: Vec<Ins>) -> Self {
        Block {
            ins,
            postlude: None,
            preds: smallvec![],
            succs: smallvec![],
            vars_declared_in_this_block: BTreeSet::new(),
            vars_used: BTreeSet::new(),
            vars_phi: BTreeSet::new(),
            vars_imported: BTreeSet::new(),
            vars_exported: BTreeSet::new(),
            vars_total_imported: BTreeSet::new(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ins {
    auxvar: Variable,
    pub typed: InsType,
    source_location: u32,
}

impl Ins {
    pub fn new(auxvar: Variable, typed: InsType, source_location: u32) -> Self {
        Ins {
            auxvar,
            typed,
            source_location,
        }
    }

    pub fn empty_ret(typed: InsType, source_location: u32) -> Self {
        Ins {
            auxvar: Variable::with_u32(0),
            typed,
            source_location,
        }
    }

    pub fn source_location(&self) -> u32 {
        self.source_location
    }

    pub fn retvar(&self) -> Option<Variable> {
        if self.typed.has_retvar() {
            Some(self.auxvar)
        } else {
            None
        }
    }

    pub fn mut_retvar(&mut self) -> Option<&mut Variable> {
        if self.typed.has_retvar() {
            Some(&mut self.auxvar)
        } else {
            None
        }
    }

    pub fn memref_store_left(&self) -> Option<&Variable> {
        match &self.typed {
            InsType::MemberReferenceStore { .. } => Some(&self.auxvar),
            _ => None,
        }
    }

    pub fn memref_store_left_mut(&mut self) -> Option<&mut Variable> {
        match &self.typed {
            InsType::MemberReferenceStore { .. } => Some(&mut self.auxvar),
            _ => None,
        }
    }

    pub fn rename_var_by<T>(&mut self, mut swap: T)
    where
        T: FnMut(Variable) -> Variable,
    {
        fn swap_indices<T>(
            mut indices: Box<[MemberExprIndex]>,
            swap: &mut T,
        ) -> Box<[MemberExprIndex]>
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
        if let Some(left) = self.memref_store_left_mut() {
            *left = swap(*left);
        }

        // A safe optimization to replace self.typed
        let old_typed = std::mem::replace(&mut self.typed, InsType::Jmp(0));
        let new_typed = match old_typed {
            InsType::Move(x) => InsType::Move(swap(x)),
            InsType::MarkMoved(x) => InsType::MarkMoved(swap(x)),
            InsType::Drop(x) => InsType::Drop(swap(x)),
            InsType::Copy(x) => InsType::Copy(swap(x)),
            InsType::Load(x) => InsType::Load(swap(x)),
            InsType::Store { source, dest } => InsType::Store {
                source: swap(source),
                dest: swap(dest),
            },
            InsType::Borrow { var, modifier } => InsType::Borrow {
                var: swap(var),
                modifier,
            },
            InsType::LoadSlice(mut args) => {
                for arg in args.iter_mut() {
                    let oldvar = *arg;
                    *arg = swap(oldvar);
                }
                InsType::LoadSlice(args)
            }
            InsType::Call { name, mut args } => {
                for arg in args.iter_mut() {
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
                indices,
                modifier,
                right,
            } => InsType::MemberReferenceStore {
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
            InsType::OpConst { .. } => unimplemented!(),
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
        self.typed = new_typed;
    }

    pub fn rename_var(&mut self, oldvar: Variable, newvar: Variable) {
        self.rename_var_by(|var| if var == oldvar { newvar } else { var })
    }

    pub fn each_used_var<T>(&self, mut callback: T)
    where
        T: FnMut(Variable),
    {
        fn each_indices<T>(indices: &Box<[MemberExprIndex]>, callback: &mut T)
        where
            T: FnMut(Variable),
        {
            for idx in indices.iter() {
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
            InsType::Move(x)
            | InsType::MarkMoved(x)
            | InsType::Drop(x)
            | InsType::Copy(x)
            | InsType::Load(x) => {
                callback(*x);
            }
            InsType::Store { source, dest } => {
                callback(*source);
                callback(*dest);
            }
            InsType::Borrow { var, .. } => {
                callback(*var);
            }
            InsType::LoadSlice(args) => {
                for arg in args.iter() {
                    callback(*arg);
                }
            }
            InsType::Call { name: _, args } => {
                for arg in args.iter() {
                    callback(*arg);
                }
            }
            InsType::Return(x) => {
                callback(*x);
            }
            InsType::Phi { vars, .. } => {
                for arg in vars.iter() {
                    callback(*arg);
                }
            }
            InsType::MemberReference { left, indices, .. } => {
                callback(*left);
                each_indices(indices, &mut callback);
            }
            InsType::MemberReferenceStore { indices, right, .. } => {
                callback(*self.memref_store_left().unwrap());
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
            InsType::OpConst { register, .. } => callback(*register),
            _ => (),
        }
    }

    pub fn each_borrowed_var<T>(&self, mut callback: T) -> bool
    where
        T: FnMut(Variable),
    {
        match &self.typed {
            InsType::Borrow { var, .. } => {
                callback(*var);
                true
            }
            _ => false,
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
                for var in vars.iter() {
                    callback(*var);
                }
                true
            }
            InsType::MemberReferenceStore {
                modifier, right, ..
            } if *modifier == ReferenceModifier::Move => {
                callback(*right);
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
    pub fn as_int(&self) -> Option<i64> {
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

    pub fn equ(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::Bool(x == y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::Bool(x == y)),
            (Constant::F64(x), Constant::F64(y)) => {
                Some(Constant::Bool(f64::from_bits(x) == f64::from_bits(y)))
            }
            (_, _) => None,
        }
    }

    pub fn neq(&self, right: Constant) -> Option<Constant> {
        match (*self, right) {
            (Constant::I32(x), Constant::I32(y)) => Some(Constant::Bool(x != y)),
            (Constant::I64(x), Constant::I64(y)) => Some(Constant::Bool(x != y)),
            (Constant::F64(x), Constant::F64(y)) => {
                Some(Constant::Bool(f64::from_bits(x) != f64::from_bits(y)))
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
    Variable(Variable),
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
    Borrow(BorrowModifier),
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum BorrowModifier {
    Immutable,
    Mutable,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum OpConst {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Lte,
    Gte,
    Equ,
    Neq,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum InsType {
    LoadNil,
    Alloca,
    Drop(Variable),
    MarkMoved(Variable),
    Move(Variable),
    Copy(Variable),
    Borrow {
        var: Variable,
        modifier: BorrowModifier,
    },
    Load(Variable),
    Store {
        source: Variable,
        dest: Variable,
    },
    LoadArg(usize),
    LoadI32(i32),
    LoadI64(i64),
    LoadF64(u64),
    LoadBool(bool),
    LoadSubstring(Rc<str>),
    LoadSlice(Box<[Variable]>),
    LoadStruct,
    MemberReference {
        left: Variable,
        indices: Box<[MemberExprIndex]>,
        modifier: ReferenceModifier,
    },
    MemberReferenceStore {
        indices: Box<[MemberExprIndex]>,
        modifier: ReferenceModifier,
        right: Variable,
    },
    Phi {
        vars: Box<[Variable]>,
        defines: Variable,
    },
    Call {
        name: u32,
        args: Box<[Variable]>,
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
    OpConst {
        register: Variable,
        constant: Constant,
        op: OpConst,
        reg_left: bool,
    },
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
            InsType::Store { .. } => false,
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
            | InsType::Store { .. }
            | InsType::Borrow { .. }
            | InsType::Call { .. }
            | InsType::Return(_)
            | InsType::Drop(_)
            | InsType::Exit => true,
            InsType::MemberReference { modifier, .. } => {
                if let ReferenceModifier::Borrow(_) = modifier {
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_phi(&self) -> bool {
        match self {
            InsType::Phi { .. } => true,
            _ => false,
        }
    }

    pub fn is_dead_end_return(&self) -> bool {
        match self {
            InsType::Return(_) | InsType::Exit => true,
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
    pub structs: FnvHashMap<Rc<str>, UniqueRc<StructType>>,
    pub generic_fat_pointer_struct: Rc<StructType>,
}

pub struct Program {
    pub contexts: FnvHashMap<Rc<FunctionName>, Context>,
    pub entry: Rc<FunctionName>,
    pub builtins: Builtins,
}

impl Program {
    pub fn print(&self) -> print::ProgramPrinter {
        print::ProgramPrinter(self)
    }
}

#[cfg(test)]
mod isa_test {
    use super::*;

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn ins_is_32_bytes() {
        assert_eq!(std::mem::size_of::<Ins>(), 32);
    }
}
