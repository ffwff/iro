use crate::types;
use crate::utils::RcWrapper;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableData {
    pub typed: types::TypeInfo,
}

impl VariableData {
    pub fn new(typed: types::TypeInfo) -> Self {
        VariableData {
            typed,
        }
    }
}

pub type Variable = RcWrapper<VariableData>;