use crate::ssa::isa::Variable;
use crate::ssa::passes::memcheck::path::MemoryState;
use std::collections::HashMap;

pub enum DropEffect {
    UnbindBorrowed(Variable),
}

pub type Drops = HashMap<Variable, DropEffect>;
