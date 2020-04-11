use crate::ast::PointerTag;
use crate::codegen::structs::*;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::ops::BitAnd;
use std::rc::Rc;

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

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub struct Block {
    pub ins: Vec<Ins>,
    pub postlude: Ins,
    pub preds: Vec<usize>,
    pub succs: Vec<usize>,
    pub vars_in: BTreeSet<Variable>,
    pub vars_out: BTreeSet<Variable>,
    pub vars_declared_in_this_block: BTreeSet<Variable>,
    pub vars_used: BTreeSet<Variable>,
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
        let old_typed = std::mem::replace(&mut self.typed, InsType::Nop);
        let new_typed = match old_typed {
            InsType::LoadVar(x) => InsType::LoadVar(swap(x)),
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
            InsType::MemberReference { left, right } => InsType::MemberReference {
                left: swap(left),
                right,
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
            InsType::PointerIndex { var, index } => InsType::PointerIndex {
                var: swap(var),
                index: swap(index),
            },
            InsType::PointerIndexC { var, offset } => InsType::PointerIndexC {
                var: swap(var),
                offset,
            },
            InsType::FatIndex { var, index } => InsType::FatIndex {
                var: swap(var),
                index: swap(index),
            },
            InsType::FatIndexC { var, offset } => InsType::FatIndexC {
                var: swap(var),
                offset,
            },
            InsType::BoundsCheck { var, index } => InsType::BoundsCheck {
                var: swap(var),
                index: swap(index),
            },
            InsType::BoundsCheckC { var, offset } => InsType::BoundsCheckC {
                var: swap(var),
                offset,
            },
            InsType::PointerStore { var, index, right } => InsType::PointerStore {
                var: swap(var),
                index: swap(index),
                right: swap(right),
            },
            InsType::FatStore { var, index, right } => InsType::FatStore {
                var: swap(var),
                index: swap(index),
                right: swap(right),
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
        match &self.typed {
            InsType::LoadVar(x) => {
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
            InsType::MemberReference { left, .. } => {
                callback(*left);
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
            InsType::PointerIndex { var, index } => {
                callback(*var);
                callback(*index);
            }
            InsType::PointerIndexC { var, .. } => {
                callback(*var);
            }
            InsType::FatIndex { var, index } => {
                callback(*var);
                callback(*index);
            }
            InsType::FatIndexC { var, .. } => {
                callback(*var);
            }
            InsType::BoundsCheck { var, index } => {
                callback(*var);
                callback(*index);
            }
            InsType::BoundsCheckC { var, .. } => {
                callback(*var);
            }
            InsType::PointerStore { var, index, right } => {
                callback(*var);
                callback(*index);
                callback(*right);
            }
            InsType::FatStore { var, index, right } => {
                callback(*var);
                callback(*index);
                callback(*right);
            }
            InsType::AddC(rc)
            | InsType::SubC(rc)
            | InsType::MulC(rc)
            | InsType::DivC(rc)
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
}

impl std::fmt::Debug for Ins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.typed.has_retvar() {
            write!(f, "v{} = {:?}", self.retvar, self.typed)
        } else {
            write!(f, "{:?}", self.typed)
        }
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum InsType {
    Nop,
    LoadNil,
    LoadVar(Variable),
    LoadArg(usize),
    LoadI32(i32),
    LoadI64(i64),
    LoadF64(u64),
    LoadBool(bool),
    LoadSubstring(Rc<str>),
    LoadSlice(Vec<Variable>),
    MemberReference {
        left: Variable,
        right: Rc<str>,
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
    PointerIndex {
        var: Variable,
        index: Variable,
    },
    PointerIndexC {
        var: Variable,
        offset: i32,
    },
    FatIndex {
        var: Variable,
        index: Variable,
    },
    FatIndexC {
        var: Variable,
        offset: i32,
    },
    BoundsCheck {
        var: Variable,
        index: Variable,
    },
    BoundsCheckC {
        var: Variable,
        offset: i32,
    },
    FatStore {
        var: Variable,
        index: Variable,
        right: Variable,
    },
    PointerStore {
        var: Variable,
        index: Variable,
        right: Variable,
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
            InsType::PointerStore { .. }
            | InsType::FatStore { .. } => false,
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
            InsType::PointerStore { .. }
            | InsType::FatStore { .. }
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct Program {
    pub contexts: HashMap<Rc<FunctionName>, Context>,
    pub entry: Rc<FunctionName>,
    pub generic_fat_pointer_struct: StructData,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    NoReturn,
    Nil,
    Bool,
    I8,
    I16,
    I32,
    I64,
    I32Ptr(Rc<PointerType>),
    I64Ptr(Rc<PointerType>),
    F64,
    Struct(StructType),
    Slice(Rc<SliceType>),
    Union(Rc<BTreeSet<Type>>),
    NeverUsed,
}

impl Type {
    pub fn new_struct(data: Rc<StructData>) -> Self {
        Type::Struct(StructType(data))
    }

    pub fn ptr_for(&self, typed: Type, tag: PointerTag) -> Option<Self> {
        match self {
            Type::I32 => Some(Type::I32Ptr(Rc::new(PointerType::new(typed, tag)))),
            Type::I64 => Some(Type::I64Ptr(Rc::new(PointerType::new(typed, tag)))),
            _ => None,
        }
    }

    pub fn slice(self, len: u32) -> Self {
        Type::Slice(Rc::new(SliceType::new(self, Some(len))))
    }

    pub fn dyn_slice(self) -> Self {
        Type::Slice(Rc::new(SliceType::new(self, None)))
    }

    pub fn as_aggregate_data<'a>(&'a self, program: &'a Program) -> Option<&dyn AggregateData> {
        match self {
            Type::Struct(StructType(struct_data_rc)) => {
                let struct_data: &StructData = struct_data_rc.borrow();
                Some(struct_data)
            }
            maybe_ptr if maybe_ptr.is_fat_pointer() => Some(&program.generic_fat_pointer_struct),
            Type::Slice(slice_rc) => {
                let slice: &SliceType = &slice_rc.borrow();
                Some(slice.array_data().unwrap())
            }
            _ => None,
        }
    }

    pub fn unify(&self, other: &Type) -> Self {
        match (self, other) {
            (left, right) if left == right => left.clone(),
            (Type::NoReturn, right) => right.clone(),
            (left, Type::NoReturn) => left.clone(),
            (Type::Union(set_left), Type::Union(set_right)) => Type::Union(Rc::new({
                let lset: &BTreeSet<Type> = set_left.borrow();
                let rset: &BTreeSet<Type> = set_right.borrow();
                lset.bitand(rset)
            })),
            (Type::Union(set), right) => Type::Union(Rc::new({
                let bbset: &BTreeSet<Type> = set.borrow();
                let mut bset = bbset.clone();
                bset.insert(right.clone());
                bset
            })),
            (left, Type::Union(set)) => Type::Union(Rc::new({
                let bbset: &BTreeSet<Type> = set.borrow();
                let mut bset = bbset.clone();
                bset.insert(left.clone());
                bset
            })),
            (left, right) => Type::Union(Rc::new(btreeset! { left.clone(), right.clone() })),
        }
    }

    pub fn can_implicit_cast_to(&self, other: &Type) -> bool {
        match (self, other) {
            // Integers with smaller sizes can cast into bigger sizes implicitly
            (Type::I32, Type::I64) => true,
            (Type::I16, Type::I64) => true,
            (Type::I16, Type::I32) => true,
            (Type::I8, Type::I64) => true,
            (Type::I8, Type::I32) => true,
            (Type::I8, Type::I16) => true,
            _ => false,
        }
    }

    pub fn can_explicit_cast_to(&self, other: &Type) -> bool {
        match (self, other) {
            (left, right) if left.is_int() && right.is_int() => true,
            (Type::I32Ptr(_), Type::I32) => true,
            (Type::I64Ptr(_), Type::I64) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Type::Nil => true,
            Type::Union(set) => {
                let bbset: &BTreeSet<Type> = set.borrow();
                bbset.contains(&Type::Nil)
            }
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            _ => false,
        }
    }

    pub fn is_fat_pointer(&self) -> bool {
        match self {
            Type::I32Ptr(ptr_typed) | Type::I64Ptr(ptr_typed) => match &ptr_typed.typed {
                Type::Slice(slice) => slice.is_dyn(),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn as_union(&self) -> Option<Rc<BTreeSet<Type>>> {
        match self {
            Type::Union(set) => Some(set.clone()),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<StructType> {
        match self {
            Type::Struct(struct_data) => Some(struct_data.clone()),
            _ => None,
        }
    }

    pub fn as_slice(&self) -> Option<Rc<SliceType>> {
        match self {
            Type::Slice(slice) => Some(slice.clone()),
            _ => None,
        }
    }

    pub fn int_from_bits(bits: usize) -> Option<Self> {
        match bits {
            8 => Some(Type::I8),
            32 => Some(Type::I32),
            64 => Some(Type::I64),
            _ => None,
        }
    }

    pub fn bytes(&self) -> Option<usize> {
        match self {
            Type::I8 => Some(1),
            Type::I32 | Type::I32Ptr(_) => Some(4),
            Type::I64 | Type::I64Ptr(_) => Some(8),
            Type::F64 => Some(8),
            Type::Slice(slice_rc) => {
                let slice: &SliceType = &slice_rc.borrow();
                if let Some(length) = slice.len {
                    if let Some(bytes) = slice.typed.bytes() {
                        return Some(bytes * length as usize);
                    }
                }
                None
            }
            _ => None,
        }
    }

    pub fn instance_bytes(&self) -> Option<usize> {
        self.instance_type().map(|typed| typed.bytes()).flatten()
    }

    pub fn instance_type(&self) -> Option<&Type> {
        match self {
            Type::I32Ptr(ptr_typed) | Type::I64Ptr(ptr_typed) => {
                if let Type::Slice(slice) = &ptr_typed.typed {
                    if slice.is_dyn() {
                        return Some(&slice.typed)
                    }
                }
                Some(&ptr_typed.typed)
            },
            Type::Slice(slice_rc) => {
                let slice: &SliceType = &slice_rc.borrow();
                Some(&slice.typed)
            }
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::NoReturn => write!(f, "(no return)"),
            Type::Nil => write!(f, "Nil"),
            Type::Bool => write!(f, "Bool"),
            Type::I8 => write!(f, "I8"),
            Type::I16 => write!(f, "I16"),
            Type::I32 => write!(f, "I32"),
            Type::I64 => write!(f, "I64"),
            Type::I32Ptr(ptr_typed) | Type::I64Ptr(ptr_typed) => write!(f, "&{}", ptr_typed.typed),
            Type::F64 => write!(f, "F64"),
            Type::Struct(struct_typed) => write!(f, "{}", struct_typed.0),
            Type::Slice(slice_rc) => {
                let slice: &SliceType = slice_rc.borrow();
                if let Some(length) = slice.len.clone() {
                    write!(f, "[{}; {}]", slice.typed, length)
                } else {
                    write!(f, "[{}]", slice.typed)
                }
            }
            Type::Union(_) => unimplemented!(),
            Type::NeverUsed => write!(f, "(never used)"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructType(pub Rc<StructData>);

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for StructType {}

impl PartialOrd for StructType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StructType {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.0.borrow() as *const StructData).cmp(&(other.0.borrow() as *const StructData))
    }
}

impl Hash for StructType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0.borrow() as *const StructData).hash(state);
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct SliceType {
    pub typed: Type,
    pub len: Option<u32>,
    array_data: Option<ArrayData>,
}

impl SliceType {
    pub fn new(typed: Type, len: Option<u32>) -> Self {
        if let Some(len) = len {
            SliceType {
                array_data: Some(ArrayData::new(typed.clone(), len as usize)),
                typed,
                len: Some(len),
            }
        } else {
            SliceType {
                typed,
                len,
                array_data: None,
            }
        }
    }

    pub fn array_data(&self) -> Option<&ArrayData> {
        self.array_data.as_ref()
    }

    pub fn is_dyn(&self) -> bool {
        self.len.is_none()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct PointerType {
    pub typed: Type,
    pub tag: PointerTag,
}

impl PointerType {
    pub fn new(typed: Type, tag: PointerTag) -> Self {
        Self { typed, tag }
    }
}
