use crate::ssa::isa;
use crate::ssa::isa::InsPosition;
use fnv::FnvHashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub var: isa::Variable,
    pub is_mut: bool,
}

#[derive(Debug)]
pub struct Env {
    /// Table of variable identifiers to variable numbers
    vars: FnvHashMap<Rc<str>, Variable>,
    /// Indices of break instructions in this environment
    pub break_idx: Option<Vec<InsPosition>>,
    /// Stack of variables inserted, from mostly recently inserted
    pub var_stack: Vec<isa::Variable>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: FnvHashMap::default(),
            break_idx: None,
            var_stack: vec![],
        }
    }

    pub fn new_breakable() -> Self {
        Env {
            vars: FnvHashMap::default(),
            break_idx: Some(Vec::new()),
            var_stack: vec![],
        }
    }

    pub fn get_var(&self, id: &Rc<str>) -> Option<&Variable> {
        self.vars.get(id)
    }

    pub fn insert_var(&mut self, id: Rc<str>, var: Variable) {
        self.vars.insert(id, var);
        self.var_stack.push(var.var);
    }

    pub fn var_stack(&self) -> &Vec<isa::Variable> {
        &self.var_stack
    }
}
