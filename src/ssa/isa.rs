use std::borrow::Borrow;
use std::rc::Rc;
use std::collections::{BTreeSet, BTreeMap, HashMap};
use std::ops::BitAnd;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntrinsicType {
    None,
    Print,
}

impl IntrinsicType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "print" => Some(IntrinsicType::Print),
            _ => None
        }
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
            blocks: vec![
                Block::new(
                    args.iter().enumerate().map(|(idx, _)| {
                        Ins { retvar: idx, typed: InsType::LoadArg(idx) }
                    }).collect(),
                )
            ],
            name,
            args,
            rettype: Type::NoReturn,
            intrinsic: IntrinsicType::None,
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
}

impl Block {
    pub fn new(ins: Vec<Ins>) -> Self {
        Block {
            ins,
        }
    }
}

#[derive(Clone)]
pub struct Ins {
    retvar: usize,
    pub typed: InsType,
}

impl Ins {
    pub fn new(retvar: usize, typed: InsType) -> Self {
        Ins {
            retvar,
            typed
        }
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

    pub fn rename_var_by<T>(&mut self, mut swap: T) where T: FnMut(usize) -> usize {
        let old_typed = std::mem::replace(&mut self.typed, InsType::Nop);
        let new_typed = match old_typed {
            InsType::LoadVar(x) => {
                InsType::LoadVar(swap(x))
            }
            InsType::Call { name, mut args } => {
                for arg in &mut args {
                    let oldvar = *arg;
                    *arg = swap(oldvar);
                }
                InsType::Call { name, args }
            }
            InsType::Return(x) => {
                InsType::Return(swap(x))
            }
            InsType::Phi { mut vars } => {
                for arg in &mut vars {
                    let oldvar = *arg;
                    *arg = swap(oldvar);
                }
                InsType::Phi { vars }
            }
            InsType::Add((x, y)) => { InsType::Add((swap(x), swap(y))) },
            InsType::Sub((x, y)) => { InsType::Sub((swap(x), swap(y))) },
            InsType::Mul((x, y)) => { InsType::Mul((swap(x), swap(y))) },
            InsType::Div((x, y)) => { InsType::Div((swap(x), swap(y))) },
            InsType::IfJmp { condvar, iftrue, iffalse } => {
                InsType::IfJmp { condvar: swap(condvar), iftrue, iffalse }
            }
            other => other,
        };
        std::mem::replace(&mut self.typed, new_typed);
    }

    pub fn rename_var(&mut self, oldvar: usize, newvar: usize) {
        self.rename_var_by(|var| {
            if var == oldvar {
                newvar
            } else {
                var
            }
        })
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

#[derive(Debug, Clone)]
pub enum InsType {
    Nop,
    LoadNil,
    LoadVar(usize),
    LoadArg(usize),
    LoadI32(i32),
    LoadString(Rc<str>),
    Phi { vars: Vec<usize> },
    Call { name: Rc<FunctionName>, args: Vec<usize> },
    Return(usize),
    Add((usize, usize)),
    Sub((usize, usize)),
    Mul((usize, usize)),
    Div((usize, usize)),
    IfJmp { condvar: usize, iftrue: usize, iffalse: usize },
    Jmp(usize),
}

impl InsType {
    pub fn is_jmp(&self) -> bool {
        match self {
            InsType::IfJmp { .. } |
            InsType::Jmp(_) |
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

pub type FuncContexts = HashMap<Rc<FunctionName>, Option<Context>>;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    NoReturn,
    Nil,
    I32,
    Float,
    String,
    Union(Rc<BTreeSet<Type>>),
}

impl Type {
    pub fn unify(&self, other: &Type) -> Self {
        match (self, other) {
            (left, right) if left == right => left.clone(),
            (Type::NoReturn, right) => right.clone(),
            (left, Type::NoReturn) => left.clone(),
            (Type::Union(set_left), Type::Union(set_right)) => Type::Union(
                Rc::new({
                    let lset: &BTreeSet<Type> = set_left.borrow();
                    let rset: &BTreeSet<Type> = set_right.borrow();
                    lset.bitand(rset)
                })),
            (Type::Union(set), right) => Type::Union(
                Rc::new({
                    let bbset: &BTreeSet<Type> = set.borrow();
                    let mut bset = bbset.clone();
                    bset.insert(right.clone());
                    bset
                })),
            (left, Type::Union(set)) => Type::Union(
                Rc::new({
                    let bbset: &BTreeSet<Type> = set.borrow();
                    let mut bset = bbset.clone();
                    bset.insert(left.clone());
                    bset
                })),
            (left, right) => Type::Union(
                Rc::new(btreeset!{ left.clone(), right.clone() })),
        }
    }
}