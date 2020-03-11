use std::rc::Rc;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

#[derive(Clone, Eq)]
pub struct TypeInfo {
    typed: Type,
    identity: Option<Variable>,
}

impl TypeInfo {

    pub fn new() -> Self {
        TypeInfo {
            typed: Type::Untyped,
            identity: None,
        }
    }

    pub fn new_with_type(typed : Type) -> Self {
        TypeInfo {
            typed: typed,
            identity: None,
        }
    }

    pub fn from_identity(identity: Variable) -> Self {
        TypeInfo {
            identity: Some(identity.clone()),
            typed: {
                let varinfo : &RefCell<VariableData> = identity.borrow();
                varinfo.borrow().type_info.typed.clone()
            },
        }
    }

    pub fn derived(&self) -> Self {
        TypeInfo {
            identity: None,
            typed: self.typed.clone()
        }
    }

    pub fn typed(&self) -> &Type {
        &self.typed
    }

    pub fn identity(&self) -> &Option<Variable> {
        &self.identity
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
        if let Some(id) = self.identity() {
            write!(f, "{:?} ({:p})", self.typed, id.as_ptr())
        } else {
            write!(f, "{:?}", self.typed)
        }
    }
}

impl std::cmp::PartialEq for TypeInfo {
    fn eq(&self, other: &Self) -> bool {
        self.typed == other.typed
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
            &Type::Identifier(var) => var.as_ptr().hash(state),
            &Type::Union(set) => {
                for typed in set {
                    typed.hash(state);
                }
            }
        }
    }
}