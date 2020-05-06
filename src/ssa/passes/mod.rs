pub mod fold;
pub mod gc;
pub mod graph;
pub mod mem;
pub mod memcheck;
pub mod postlude;
pub mod postprocess;
pub mod ssa;

use crate::ssa::isa::Variable;

pub struct ContextLocalData {
    pub invalid_build_graph: bool,
    pub unused_vars: Vec<Variable>,
}

impl ContextLocalData {
    pub fn new() -> Self {
        ContextLocalData {
            invalid_build_graph: true,
            unused_vars: vec![],
        }
    }
}
