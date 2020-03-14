use std::rc::{Rc, Weak};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use crate::types::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Untyped,
    Unresolved,
    UnresolvedRedirect(TypeInfo),
    Nil,
    String,
    Int,
    Float,
    Union(HashSet<Type>),
    Function(Function),
    Type(Box<Type>),
}

impl Type {
    fn has_unresolved(&self) -> bool {
        match &self {
            // Type::Unresolved(_) => true,
            Type::Union(set) => {
                for typed in set {
                    if typed.has_unresolved() {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    pub fn as_function(&self) -> Option<Function> {
        match &self {
            Type::Function(function) => Some(function.clone()),
            _ => None
        }
    }
}

/*
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Type::Untyped => write!(f, "Untyped"),
            Type::Nil => write!(f, "Nil"),
            Type::String => write!(f, "String"),
            Type::Int => write!(f, "Integer"),
            Type::Float => write!(f, "Float"),
            Type::Identifier(var) => write!(f, "Identifier ({:p})", var.as_ptr()),
            Type::Unresolved(var) => write!(f, "Unresolved ({:p})", var.as_ptr()),
            Type::Union(set) => write!(f, "{}", set.iter().map(|typed| {
                let mut output = String::new();
                write!(&mut output, "{}", typed).unwrap();
                output
            }).collect::<Vec::<String>>().join(" | ")),
            Type::Function(func_rcc) => {
                let func_rc : &RefCell<FunctionData> = func_rcc.borrow();
                let func : &FunctionData = &func_rc.borrow();
                let mut template_vars = HashMap::new();
                let mut template_idx = 0u32;
                write!(f, "Fn({}) -> ",
                    func.args.iter().map(|(string, var_rcc)| {
                        let mut output = String::new();
                        let var_rc : &RefCell<VariableData> = var_rcc.borrow();
                        let var : &VariableData = &var_rc.borrow();
                        if var.type_info.typed().has_unresolved() {
                            if let Some(idx) = template_vars.get(&var.type_info) {
                                write!(&mut output, "{}: {}", string,
                                    std::char::from_u32(idx + 'T' as u32).unwrap()).unwrap();
                            } else {
                                write!(&mut output, "{}: {}", string,
                                    std::char::from_u32(template_idx + 'T' as u32).unwrap()).unwrap();
                                template_vars.insert(var.type_info.clone(), template_idx);
                                template_idx += 1;
                            }
                        } else {
                            write!(&mut output, "{}: {}", string, var.type_info.typed()).unwrap();
                        }
                        output
                    }).collect::<Vec::<String>>().join(", "))?;
                if func.returntype.typed().has_unresolved() {
                    if let Type::Union(set) = &func.returntype.typed() {
                        write!(f, "{}", set.iter().map(|typed| {
                            let mut output = String::new();
                            if typed.has_unresolved() {
                                let type_info = TypeInfo::new_with_type(typed.clone());
                                if let Some(idx) = template_vars.get(&type_info) {
                                    write!(&mut output, "{}",
                                        std::char::from_u32(idx + 'T' as u32).unwrap()).unwrap();
                                } else {
                                    write!(&mut output, "{}",
                                        std::char::from_u32(template_idx + 'T' as u32).unwrap()).unwrap();
                                }
                            } else {
                                write!(&mut output, "{}", typed).unwrap();
                            }
                            output
                        }).collect::<Vec::<String>>().join(" | "))
                    } else if let Some(idx) = template_vars.get(&func.returntype) {
                        write!(f, "{}",
                            std::char::from_u32(idx + 'T' as u32).unwrap())
                    } else {
                        write!(f, "{}",
                            std::char::from_u32(template_idx + 'T' as u32).unwrap())
                    }
                } else {
                    write!(f, "{}", func.returntype.typed())
                }
            }
            Type::Type(typed) => write!(f, "Type({})", typed),
        }
    }
} */

impl std::hash::Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self {
            Type::Untyped => 0.hash(state),
            Type::Nil     => 1.hash(state),
            Type::String  => 2.hash(state),
            Type::Int     => 3.hash(state),
            Type::Float   => 4.hash(state),
            Type::Unresolved => 5.hash(state),
            //&Type::Identifier(var) => var.as_ptr().hash(state),
            &Type::UnresolvedRedirect(var) => var.hash(state),
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