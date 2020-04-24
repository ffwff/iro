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
    pub sub_paths: HashMap<Index, Directory>,
}

impl Directory {
    pub fn new() -> Self {
        Directory {
            sub_paths: HashMap::new(),
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
    FullyMoved,
}

pub type Paths = HashMap<usize, MemoryState>;
