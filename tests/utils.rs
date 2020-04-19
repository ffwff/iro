use iro::compiler;
use iro::codegen::cranelift::CraneliftBackend;
use iro::codegen::backend::JitBackend;
use iro::codegen::settings::Settings;
use iro::runtime::Runtime;

pub use iro::compiler::parse_to_ssa;

pub fn parse_and_run(source: &str, runtime: Runtime) -> Result<(), compiler::Error> {
    let program = parse_to_ssa(source)?;
    let settings = Settings::default();
    unsafe {
        (CraneliftBackend {}).run(&program, &settings, &runtime)
    }
}
