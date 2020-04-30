use crate::ssa::isa::Variable;
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
