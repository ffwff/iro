use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Env {
    vars: HashMap<Rc<str>, usize>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }

    pub fn vars(&self) -> &HashMap<Rc<str>, usize> {
        &self.vars
    }

    pub fn vars_mut(&mut self) -> &mut HashMap<Rc<str>, usize> {
        &mut self.vars
    }
}
