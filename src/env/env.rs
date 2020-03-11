use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::types::types::{Variable, TypeInfo};

pub struct Env {
    vars: HashMap<String, Variable>
}

impl Env {

    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }

    pub fn defvar(&mut self, s : String) -> Variable {
        let var = Rc::new(RefCell::new(TypeInfo::new()));
        self.vars.insert(s, var.clone());
        var
    }

    pub fn getvar(&self, s : &str) -> Option<Variable> {
        self.vars.get(s).cloned()
    }

}