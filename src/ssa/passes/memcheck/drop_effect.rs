use crate::ssa::isa::Variable;
use fnv::FnvHashMap;

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

pub type Drops = FnvHashMap<Variable, DropEffect>;
