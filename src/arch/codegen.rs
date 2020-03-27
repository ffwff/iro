use crate::arch::context;
use crate::ssa::isa::*;

#[derive(Debug, Clone)]
pub enum Error {
    InvalidOpcode,
}

pub trait Codegen {
    fn process(&mut self, program: &Program) -> Result<context::Contexts, Error>;
}
