use iro::arch::context;

pub fn context() -> context::Context {
    context::Context {
        code: vec![],
        data: vec![],
        func_relocation: vec![],
        rel_relocation: vec![],
    }
}
