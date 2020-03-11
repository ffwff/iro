use std::rc::Rc;
use std::cell::RefCell;

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
            Type::Union(mut vec) => {
                match typed {
                    Type::Union(other) => {
                        vec.extend(other);
                    }
                    _ => vec.push(typed),
                }
                vec.dedup();
                std::mem::replace(&mut self.typed, Type::Union(vec));
            }
            this_type => {
                let union = match typed {
                    Type::Union(mut other) => {
                        other.push(this_type);
                        other.dedup();
                        other
                    }
                    _ => vec![this_type, typed],
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
    Union(Vec<Type>)
}