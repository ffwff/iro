use crate::runtime::functions;
use std::collections::HashMap;
use std::sync::Mutex;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GenericFunction(pub *const libc::c_void);
unsafe impl Send for GenericFunction {}

pub struct RuntimeData {
    funcs: HashMap<String, GenericFunction>,
}

impl RuntimeData {
    pub unsafe fn new() -> Self {
        RuntimeData {
            funcs: hashmap![
                "print_i32".to_string() => GenericFunction(functions::print_i32 as _),
                "print_i64".to_string() => GenericFunction(functions::print_i64 as _),
            ],
        }
    }

    pub fn funcs(&self) -> &HashMap<String, GenericFunction> {
        &self.funcs
    }

    pub fn insert_func(&mut self, key: String, callback: *const libc::c_void) {
        self.funcs.insert(key, GenericFunction(callback));
    }
}

lazy_static! {
    pub static ref RUNTIME: Mutex<RuntimeData> = unsafe { Mutex::new(RuntimeData::new()) };
}
