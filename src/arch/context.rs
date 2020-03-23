use crate::ssa::isa::FunctionName;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct RelativeRelocation {
    pub label: usize,
    pub branch: usize,
}

#[derive(Debug, Clone)]
pub struct Context {
    pub code: Vec<u8>,
    pub data: Vec<u8>,
    /// Relative 32-bit function relocation
    pub func_relocation: Vec<(usize, Rc<FunctionName>)>,
    /// Relative 32-bit label relocation
    pub rel_relocation: Vec<RelativeRelocation>,
}

pub type Contexts = HashMap<Rc<FunctionName>, Context>;
