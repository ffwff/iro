use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub var: usize,
    pub is_mut: bool,
}

#[derive(Debug)]
pub struct Env {
    vars: HashMap<Rc<str>, Variable>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }

    pub fn vars(&self) -> &HashMap<Rc<str>, Variable> {
        &self.vars
    }

    pub fn vars_mut(&mut self) -> &mut HashMap<Rc<str>, Variable> {
        &mut self.vars
    }
}
