use crate::runtime::functions;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GenericFunction(pub(self) *const libc::c_void);
unsafe impl Send for GenericFunction {}

impl GenericFunction {
    pub fn ptr(&self) -> *const libc::c_void {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Runtime {
    funcs: HashMap<String, GenericFunction>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            funcs: hashmap![
                "print_i32".to_string() => GenericFunction(functions::print_i32 as _),
                "print_i64".to_string() => GenericFunction(functions::print_i64 as _),
            ],
        }
    }

    pub fn funcs(&self) -> &HashMap<String, GenericFunction> {
        &self.funcs
    }

    pub unsafe fn insert_func<S>(&mut self, string: S, generic: GenericFunction) where S: ToString {
        self.funcs.insert(string.to_string(), generic);
    }
}
