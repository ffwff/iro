#[derive(Clone, PartialEq, Eq)]
pub struct TypeInfo {
    union_types: Vec<Type>
}

impl TypeInfo {

    pub fn new() -> Self {
        TypeInfo {
            union_types: vec![]
        }
    }

    pub fn new_with_type(typed : Type) -> Self {
        TypeInfo {
            union_types: vec![typed]
        }
    }

    pub fn add_type(&mut self, typed : Type) {
        self.union_types.push(typed);
    }

    pub fn union_types(&self) -> &Vec<Type> {
        &self.union_types
    }

}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.union_types)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    String,
    Integer,
    Float
}