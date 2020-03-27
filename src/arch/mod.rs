pub mod x86_64;

pub mod context;
pub mod codegen;
pub mod mmap;

#[cfg(all(unix, target_arch="x86_64"))]
pub fn current_codegen() -> x86_64::visitor::Codegen {
    x86_64::visitor::Codegen::new()
}