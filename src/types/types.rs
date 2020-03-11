use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, PartialEq, Eq)]
pub struct TypeInfo {
    typed: Type
}

impl TypeInfo {

    pub fn new() -> Self {
        TypeInfo {
            typed: Type::None
        }
    }

    pub fn new_with_type(typed : Type) -> Self {
        TypeInfo {
            typed: typed
        }
    }

    pub fn add_type(&mut self, typed : Type) {
        match std::mem::replace(&mut self.typed, Type::None) {
            Type::None => {
                std::mem::replace(&mut self.typed, typed);
            }
            Type::Union(mut vec) => {
                vec.push(typed);
                std::mem::replace(&mut self.typed, Type::Union(vec));
            }
            other => {
                std::mem::replace(&mut self.typed, Type::Union(vec![ other, typed ]));
            }
        }
    }

}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.typed)
    }
}

pub type Variable = Rc<RefCell<TypeInfo>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    None,
    String,
    Integer,
    Float,
    Identifier(Variable),
    Union(Vec<Type>)
}