use std::rc::{Rc, Weak};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use crate::types::*;
use crate::env::var::*;
use crate::utils::RcWrapper;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionData {
    pub args: Vec<(Rc<str>, TypeInfo)>,
    pub returntype: TypeInfo,
    pub overloads: Option<HashMap<Vec<TypeInfo>, TypeInfo>>,
    pub declared: bool,
}

impl FunctionData {
    pub fn new(args: Vec<(Rc<str>, TypeInfo)>) -> Self {
        FunctionData {
            args,
            returntype: TypeInfo::new(),
            overloads: None,
            declared: true,
        }
    }

    pub fn has_overloads(&self) -> bool {
        self.overloads.is_some()
    }

    pub fn check_overloads(&mut self) {
        self.overloads = None;
        for (_, var) in &self.args {
            if var.is_unresolved() {
                self.overloads = Some(HashMap::new());
                return;
            }
        }
        if self.returntype.is_unresolved() {
            self.overloads = Some(HashMap::new());
        }
    }

    pub fn add_overload(&mut self, arg_types: Vec<TypeInfo>) -> Result<TypeInfo, ()> {
        if let Some(overloads) = self.overloads.as_mut() {
            if let Some(overload) = overloads.get(&arg_types) {
                Ok(overload.clone())
            } else if self.returntype.is_unresolved() {
                let unresolved = TypeInfo::new_typed(Type::Unresolved);
                overloads.insert(arg_types, unresolved.clone());
                Ok(unresolved)
            } else {
                Ok(self.returntype.clone())
            }
        } else {
            Err(())
        }
    }
}

pub type Function = RcWrapper<FunctionData>;