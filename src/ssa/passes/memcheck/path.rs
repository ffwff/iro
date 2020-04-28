use crate::compiler::sources::SpanIndex;
use crate::ssa::isa::Variable;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Index {
    /// Represents an index to a struct's member
    Struct(usize),
    /// Represents an index to somewhere in memory
    Dynamic,
}

#[derive(Debug, Clone)]
pub struct Directory {
    pub last_used: SpanIndex,
    pub sub_paths: HashMap<Index, Directory>,
}

impl Directory {
    pub fn new(last_used: SpanIndex) -> Self {
        Directory {
            sub_paths: HashMap::new(),
            last_used,
        }
    }

    pub fn extend(&mut self, other: Directory) {
        for (key, other_path) in other.sub_paths {
            if let Some(path) = self.sub_paths.get_mut(&key) {
                path.extend(other_path);
            } else {
                self.sub_paths.insert(key, other_path);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemoryState {
    PartiallyMoved(Directory),
    FullyMoved(SpanIndex),
}

impl MemoryState {
    pub fn last_used(&self) -> SpanIndex {
        match self {
            MemoryState::PartiallyMoved(dict) => dict.last_used,
            MemoryState::FullyMoved(last_used) => *last_used,
        }
    }
}

pub type Paths = HashMap<Variable, MemoryState>;
