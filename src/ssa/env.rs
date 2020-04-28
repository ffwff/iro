use crate::ssa::isa;
use crate::ssa::isa::InsPosition;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub var: isa::Variable,
    pub is_mut: bool,
}

#[derive(Debug)]
pub struct Env {
    /// Table of variable identifiers to variable numbers
    pub vars: HashMap<Rc<str>, Variable>,
    /// Indices of break instructions in this environment
    pub break_idx: Option<Vec<InsPosition>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
            break_idx: None,
        }
    }

    pub fn new_breakable() -> Self {
        Env {
            vars: HashMap::new(),
            break_idx: Some(Vec::new()),
        }
    }
}
