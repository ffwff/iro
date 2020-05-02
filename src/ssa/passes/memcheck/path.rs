use crate::compiler::sources::SpanIndex;
use crate::ssa::isa::Variable;
use fnv::FnvHashMap;

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
    pub sub_paths: FnvHashMap<Index, Directory>,
}

impl Directory {
    pub fn new(last_used: SpanIndex) -> Self {
        Directory {
            sub_paths: FnvHashMap::default(),
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
    None,
    PartiallyMoved(Directory),
    FullyMoved(SpanIndex),
    FullyBorrowed(FnvHashMap<Variable, SpanIndex>),
    FullyBorrowedMut(SpanIndex),
}

#[derive(Debug, Clone)]
pub enum LastUsed {
    One(SpanIndex),
    Many(Vec<SpanIndex>),
}

impl MemoryState {
    pub fn into_opt(self) -> Option<Self> {
        match self {
            MemoryState::None => None,
            _ => Some(self),
        }
    }

    pub fn as_opt_ref(&self) -> Option<&Self> {
        match self {
            MemoryState::None => None,
            _ => Some(self),
        }
    }

    pub fn as_opt_mut(&mut self) -> Option<&mut Self> {
        match self {
            MemoryState::None => None,
            _ => Some(self),
        }
    }

    pub fn last_used(&self) -> LastUsed {
        match self {
            MemoryState::None => unreachable!(),
            MemoryState::PartiallyMoved(dict) => LastUsed::One(dict.last_used),
            MemoryState::FullyMoved(last_used) => LastUsed::One(*last_used),
            MemoryState::FullyBorrowed(map) => LastUsed::Many(map.values().cloned().collect()),
            MemoryState::FullyBorrowedMut(last_used) => LastUsed::One(*last_used),
        }
    }

    pub fn borrow(self, borrower: Variable, span: SpanIndex) -> Option<Self> {
        if let MemoryState::FullyBorrowed(mut map) = self {
            map.insert(borrower, span);
            if map.is_empty() {
                Some(MemoryState::None)
            } else {
                Some(MemoryState::FullyBorrowed(map))
            }
        } else if let MemoryState::None = self {
            Some(MemoryState::FullyBorrowed(fnv_hashmap![
                borrower => span,
            ]))
        } else {
            None
        }
    }

    pub fn unborrow(self, borrower: Variable) -> Self {
        dbg_println!("unborrow from {:?}", borrower);
        if let MemoryState::FullyBorrowed(mut map) = self {
            map.remove(&borrower).expect("invalid unborrow");
            if map.is_empty() {
                MemoryState::None
            } else {
                MemoryState::FullyBorrowed(map)
            }
        } else {
            panic!("called on a non fully immutably, borrowed mem state");
        }
    }
}

pub type Paths = FnvHashMap<Variable, MemoryState>;
