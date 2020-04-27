use iro::codegen::backend::JitBackend;
use iro::codegen::cranelift::CraneliftBackend;
use iro::codegen::settings::Settings;
use iro::compiler;
use iro::runtime::Runtime;
use iro::ssa::isa;

pub fn parse_and_run(source: &str, runtime: Runtime) -> Result<(), compiler::Error> {
    let program = parse_to_ssa(source)?;
    let settings = Settings::default();
    unsafe { (CraneliftBackend {}).run(&program, &settings, &runtime) }
}

pub fn parse_to_ssa(source: &str) -> Result<isa::Program, compiler::Error> {
    compiler::parse_source_to_ssa(source)
}
