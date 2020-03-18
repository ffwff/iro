use std::borrow::Borrow;
use std::rc::Rc;
use std::collections::{BTreeSet, HashMap};
use std::ops::BitAnd;

#[derive(Debug, Clone)]
pub struct Context {
    pub blocks: Vec<Block>,
    pub variables: Vec<Type>,
    pub name: Rc<str>,
    pub args: Vec<Type>,
    pub rettype: Type,
}

impl Context {
    pub fn new(name: Rc<str>) -> Self {
        Context {
            blocks: vec![],
            variables: vec![],
            name,
            args: vec![],
            rettype: Type::NoReturn,
        }
    }

    pub fn with_args(name: Rc<str>, args: Vec<Type>) -> Self {
        Context {
            variables: args.clone(),
            blocks: vec![
                Block {
                    ins: args.iter().enumerate().map(|(idx, _)| {
                        Ins { retvar: idx, typed: InsType::LoadArg(idx) }
                    }).collect(),
                }
            ],
            name,
            args,
            rettype: Type::NoReturn,
        }
    }

    pub fn insert_var(&mut self, typed: Type) -> usize {
        self.variables.push(typed);
        self.variables.len() - 1
    }

    pub fn new_block(&mut self) -> usize {
        self.blocks.push(Block { ins: vec![] });
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
    Phi { branch_to_var: Vec<(usize, Option<usize>)> },
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