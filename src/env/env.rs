use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::types::types::*;

#[derive(Debug)]
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
        let var = Rc::new(RefCell::new(VariableData::new()));
        self.vars.insert(s, var.clone());
        var
    }

    pub fn setvar(&mut self, s : String, new: Variable) {
        self.vars.insert(s, new);
    }

    pub fn getvar(&self, s : &str) -> Option<Variable> {
        self.vars.get(s).cloned()
    }

    pub fn vars(&self) -> &HashMap<String, Variable> {
        &self.vars
    }

    pub fn mut_vars(&mut self) -> &mut HashMap<String, Variable> {
        &mut self.vars
    }

}