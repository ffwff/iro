use crate::ssa::isa::Variable;
use crate::ssa::passes::memcheck::path::MemoryState;
use std::collections::HashMap;

#[derive(Clone)]
pub enum DropEffect {
    UnbindBorrowed {
        borrower: Variable,
        target: Variable,
    },
    UnbindBorrowedMut {
        borrower: Variable,
        target: Variable,
    },
}

pub type Drops = HashMap<Variable, DropEffect>;
