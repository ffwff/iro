use iro::arch::codegen::Codegen;
use iro::arch::current_codegen;
use iro::arch::mmap;
use iro::ast::Visitor;
use iro::runtime::Runtime;
use iro::ssa::opt;
use iro::ssa::visitor::SSAVisitor;
use iro::compiler;
use iro::utils;

pub fn parse_and_run(code: &str, runtime: Runtime) {
    let ast = utils::parse_input(code).unwrap();
    let mut visitor = SSAVisitor::new(runtime);
    visitor.visit_program(&ast).unwrap();
    let mut program = visitor.into_program().unwrap();
    let ssa_pipeline = compiler::ssa_pipeline();
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
    }
    let mut visitor = current_codegen();
    let contexts = visitor.process(&program).unwrap();
    unsafe {
        let mmap = visitor.make_mmap(&contexts).unwrap();
        mmap.execute(&program.entry).unwrap();
    }
}
