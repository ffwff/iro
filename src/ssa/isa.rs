use std::rc::{Rc, Weak};

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

    pub fn new_block(&mut self) {
        self.blocks.push(Block { ins: vec![] });
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
    pub retvar: usize,
    pub typed: InsType,
}

impl std::fmt::Debug for Ins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{} = {:?}", self.retvar, self.typed)
    }
}

#[derive(Debug, Clone)]
pub enum InsType {
    Nop,
    LoadArg(usize),
    LoadI32(i32),
    LoadString(Rc<str>),
    Call((Rc<FunctionName>, Vec<usize>)),
    Return(usize),
    Add((usize, usize)),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionName {
    pub name: Rc<str>,
    pub arg_types: Vec<Type>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    NoReturn,
    I32,
    Float,
    String,
}