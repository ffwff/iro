pub mod x86_64;

pub mod codegen;
pub mod context;
pub mod mmap;

#[cfg(all(unix, target_arch = "x86_64"))]
pub fn current_codegen() -> x86_64::codegen::Codegen {
    x86_64::codegen::Codegen::new()
}
