#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Path {
    Variable(usize),
    Member {
        left: usize,
        indices: Vec<Index>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Index {
    Struct(usize),
    Dynamic,
}