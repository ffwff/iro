use crate::runtime::{GenericFunction, Runtime};
use std::borrow::Borrow;
use std::collections::{BTreeSet, HashMap};
use std::fmt::Write;
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

    pub fn insert_var(&mut self, typed: Type) -> usize {
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
    pub preds: Vec<usize>,
    pub succs: Vec<usize>,
    pub vars_in: BTreeSet<usize>,
    pub vars_out: BTreeSet<usize>,
    pub vars_declared_in_this_block: BTreeSet<usize>,
    pub vars_used: BTreeSet<usize>,
}

impl Block {
    pub fn new(ins: Vec<Ins>) -> Self {
        Block {
            ins,
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
    retvar: usize,
    pub typed: InsType,
}

impl Ins {
    pub fn new(retvar: usize, typed: InsType) -> Self {
        Ins { retvar, typed }
    }

    pub fn retvar(&self) -> Option<usize> {
        if self.typed.is_jmp() {
            None
        } else {
            Some(self.retvar)
        }
    }

    pub fn mut_retvar(&mut self) -> Option<&mut usize> {
        if self.typed.is_jmp() {
            None
        } else {
            Some(&mut self.retvar)
        }
    }

    pub fn rename_var_by<T>(&mut self, do_phi: bool, mut swap: T)
    where
        T: FnMut(usize) -> usize,
    {
        let old_typed = std::mem::replace(&mut self.typed, InsType::Nop);
        let new_typed = match old_typed {
            InsType::LoadVar(x) => InsType::LoadVar(swap(x)),
            InsType::Call { name, mut args } => {
                for arg in &mut args {
                    let oldvar = *arg;
                    *arg = swap(oldvar);
                }
                InsType::Call { name, args }
            }
            InsType::Return(x) => InsType::Return(swap(x)),
            InsType::Phi { mut vars } => {
                if do_phi {
                    for arg in &mut vars {
                        let oldvar = *arg;
                        *arg = swap(oldvar);
                    }
                }
                InsType::Phi { vars }
            }
            InsType::Add((x, y)) => InsType::Add((swap(x), swap(y))),
            InsType::Sub((x, y)) => InsType::Sub((swap(x), swap(y))),
            InsType::Mul((x, y)) => InsType::Mul((swap(x), swap(y))),
            InsType::Div((x, y)) => InsType::Div((swap(x), swap(y))),
            InsType::Lt((x, y)) => InsType::Lt((swap(x), swap(y))),
            InsType::Gt((x, y)) => InsType::Gt((swap(x), swap(y))),
            InsType::Lte((x, y)) => InsType::Lte((swap(x), swap(y))),
            InsType::Gte((x, y)) => InsType::Gte((swap(x), swap(y))),
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
            other => other,
        };
        std::mem::replace(&mut self.typed, new_typed);
    }

    pub fn rename_var(&mut self, do_phi: bool, oldvar: usize, newvar: usize) {
        self.rename_var_by(do_phi, |var| if var == oldvar { newvar } else { var })
    }

    pub fn each_used_var<T>(&self, mut callback: T)
    where
        T: FnMut(usize),
    {
        match &self.typed {
            InsType::LoadVar(x) => {
                callback(*x);
            }
            InsType::Call { name: _, args } => {
                for arg in args {
                    callback(*arg);
                }
            }
            InsType::Return(x) => {
                callback(*x);
            }
            InsType::Phi { vars } => {
                for arg in vars {
                    callback(*arg);
                }
            }
            InsType::Add((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Sub((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Mul((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Div((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Lt((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Gt((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Lte((x, y)) => {
                callback(*x);
                callback(*y);
            }
            InsType::Gte((x, y)) => {
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
        if self.typed.is_jmp() {
            write!(f, "{:?}", self.typed)
        } else {
            write!(f, "v{} = {:?}", self.retvar, self.typed)
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Constant {
    I32(i32),
    I64(i64),
    F64(u64),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum RegConst {
    RegLeft((usize, Constant)),
    RegRight((Constant, usize)),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum InsType {
    Nop,
    LoadNil,
    LoadVar(usize),
    LoadArg(usize),
    LoadI32(i32),
    LoadI64(i64),
    LoadF64(u64),
    LoadString(Rc<str>),
    Phi {
        vars: Vec<usize>,
    },
    Call {
        name: Rc<FunctionName>,
        args: Vec<usize>,
    },
    Return(usize),
    Exit,
    Add((usize, usize)),
    Sub((usize, usize)),
    Mul((usize, usize)),
    Div((usize, usize)),
    Lt((usize, usize)),
    Gt((usize, usize)),
    Lte((usize, usize)),
    Gte((usize, usize)),
    AddC(RegConst),
    SubC(RegConst),
    MulC(RegConst),
    DivC(RegConst),
    LtC(RegConst),
    GtC(RegConst),
    LteC(RegConst),
    GteC(RegConst),
    Cast {
        var: usize,
        typed: Type,
    },
    IfJmp {
        condvar: usize,
        iftrue: usize,
        iffalse: usize,
    },
    Jmp(usize),
}

impl InsType {
    pub fn is_jmp(&self) -> bool {
        match self {
            InsType::IfJmp { .. } | InsType::Jmp(_) | InsType::Return(_) | InsType::Exit => true,
            _ => false,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            InsType::LoadNil
            | InsType::LoadI32(_)
            | InsType::LoadI64(_)
            | InsType::LoadF64(_)
            | InsType::LoadString(_) => true,
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
            InsType::Call { .. } | InsType::Return(_) | InsType::Exit => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            InsType::Return(_) => true,
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
                write!(&mut string, "{:?}", typed).unwrap();
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
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    NoReturn,
    Nil,
    Bool,
    I32,
    I64,
    F64,
    Float,
    String,
    Union(Rc<BTreeSet<Type>>),
    NeverUsed,
}

impl Type {
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

    pub fn can_cast_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::I32, Type::I64) => true,
            _ => false,
        }
    }
}
