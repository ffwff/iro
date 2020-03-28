use crate::arch::context;
use crate::arch::mmap;
use crate::ssa::isa::*;

#[derive(Debug, Clone)]
pub enum Error {
    InvalidOpcode,
}

pub trait Codegen {
    fn process(&mut self, program: &Program) -> Result<context::Contexts, Error>;
    unsafe fn make_mmap(&self, contexts: &context::Contexts) -> Result<mmap::Mmap, std::io::Error>;
}
