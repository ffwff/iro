use std::rc::Rc;
use std::collections::HashMap;
use crate::ssa::isa::FunctionName;

#[derive(Debug, Clone)]
pub struct Context {
    pub code: Vec<u8>,
    pub data: Vec<u8>,
    /// Relative 32-bit function relocation
    pub func_relocation: Vec<(usize, Rc<FunctionName>)>,
    pub label_relocation: Vec<(usize, usize)>,
}

pub type IsaContexts = HashMap<Rc<FunctionName>, Context>;