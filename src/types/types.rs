use std::rc::{Rc, Weak};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::hash::{Hash, Hasher};

#[derive(Clone, Hash, Eq)]
pub struct TypeInfo {
    typed: Type,
}

impl TypeInfo {

    pub fn new() -> Self {
        TypeInfo {
            typed: Type::Untyped,
        }
    }

    pub fn new_with_type(typed : Type) -> Self {
        TypeInfo {
            typed: typed,
        }
    }

    pub fn from_variable(var: &Variable) -> Self {
        TypeInfo {
            typed: {
                let varinfo : &RefCell<VariableData> = var.borrow();
                varinfo.borrow().type_info.typed.clone()
            },
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
                if this_type == typed {
                    std::mem::replace(&mut self.typed, typed);
                    return;
                }
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

    pub fn as_function(&self) -> Option<Function> {
        match &self.typed {
            Type::Function(function) => Some(function.clone()),
            _ => None
        }
    }

}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.typed {
            Type::Identifier(var) => write!(f, "Identifier ({:p}) {:?}", var.as_ptr(), var),
            Type::Unresolved(var) => write!(f, "Unresolved ({:p}) {:?}", var.as_ptr(), var),
            _ => write!(f, "{:#?}", self.typed)
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

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct FunctionOverload {
    pub args: Vec<TypeInfo>,
    pub returntype: TypeInfo,
}

impl FunctionOverload {
    pub fn new() -> Self {
        FunctionOverload {
            args: vec![],
            returntype: TypeInfo::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionData {
    pub args: Vec<(String, Variable)>,
    pub returntype: TypeInfo,
    pub overloads: Option<HashSet<FunctionOverload>>,
}

impl FunctionData {
    pub fn new(args: Vec<(String, Variable)>) -> Self {
        FunctionData {
            args,
            returntype: TypeInfo::new(),
            overloads: None,
        }
    }

    pub fn check_overloads(&mut self) {
        self.overloads = None;
        for (_, var) in &self.args {
            let vdata_rc : &RefCell<VariableData> = var.borrow();
            let vdata : &VariableData = &vdata_rc.borrow();
            match &vdata.type_info.typed() {
                Type::Unresolved(_) => {
                    self.overloads = Some(HashSet::new());
                    return;
                },
                _ => (),
            }
        }
        match &self.returntype.typed()  {
            Type::Unresolved(_) => {
                self.overloads = Some(HashSet::new());
            },
            _ => (),
        }
    }
}

pub type Function = Rc<RefCell<FunctionData>>;

#[derive(Clone)]
pub struct UnresolveData {
    pub id: Option<Weak<RefCell<VariableData>>>,
}

impl std::cmp::PartialEq for UnresolveData {
    fn eq(&self, other: &Self) -> bool {
        match (&self.id, &other.id) {
            (Some(a), Some(b)) => Weak::<_>::ptr_eq(a, b),
            (None, None) => true,
            (_, _) => false,
        }
    }
}

impl std::cmp::Eq for UnresolveData {}

impl std::fmt::Debug for UnresolveData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.id {
            Some(var) => write!(f, "Unresolved ({:p})", var),
            None => write!(f, "Unresolved"),
        }
    }
}

pub type Unresolved = Rc<RefCell<UnresolveData>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Untyped,
    Nil,
    String,
    Integer,
    Float,
    Identifier(Variable),
    Unresolved(Unresolved),
    Union(HashSet<Type>),
    Function(Function),
    Type(Box<Type>),
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
            &Type::Unresolved(var) => var.as_ptr().hash(state),
            &Type::Function(var) => var.as_ptr().hash(state),
            &Type::Type(var) => var.hash(state),
            &Type::Union(set) => {
                for typed in set {
                    typed.hash(state);
                }
            }
        }
    }
}