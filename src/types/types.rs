use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

#[derive(Clone, PartialEq, Eq)]
pub struct TypeInfo {
    typed: Type
}

impl TypeInfo {

    pub fn new() -> Self {
        TypeInfo {
            typed: Type::Untyped
        }
    }

    pub fn new_with_type(typed : Type) -> Self {
        TypeInfo {
            typed: typed
        }
    }

    pub fn typed(&self) -> &Type {
        &self.typed
    }

    pub fn add_type(&mut self, typed : Type) {
        match std::mem::replace(&mut self.typed, Type::Untyped) {
            Type::Untyped => {
                std::mem::replace(&mut self.typed, typed);
            }
            Type::Union(mut set) => {
                match typed {
                    Type::Union(other) => {
                        set.extend(other);
                    }
                    _ => {
                        set.insert(typed);
                    }
                }
                std::mem::replace(&mut self.typed, Type::Union(set));
            }
            this_type => {
                let union = match typed {
                    Type::Union(mut other) => {
                        other.insert(this_type);
                        other
                    }
                    _ => {
                        let mut set = HashSet::new();
                        set.insert(this_type);
                        set.insert(typed);
                        set
                    }
                };
                std::mem::replace(&mut self.typed, Type::Union(union));
            }
        }
    }

}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.typed)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableData {
    pub type_info: TypeInfo,
}

impl VariableData {
    pub fn new() -> Self {
        VariableData {
            type_info: TypeInfo::new(),
        }
    }

    pub fn new_with_type(type_info: TypeInfo) -> Self {
        VariableData {
            type_info,
        }
    }
}

pub type Variable = Rc<RefCell<VariableData>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Untyped,
    Nil,
    String,
    Integer,
    Float,
    Identifier(Variable),
    Union(HashSet<Type>)
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self {
            Type::Untyped => 0.hash(state),
            Type::Nil     => 1.hash(state),
            Type::String  => 2.hash(state),
            Type::Integer => 3.hash(state),
            Type::Float   => 4.hash(state),
            &Type::Identifier(var) => {
                let addr = var.as_ptr() as usize;
                addr.hash(state);
            }
            &Type::Union(set) => {
                for typed in set {
                    typed.hash(state);
                }
            }
        }
    }
}