use iro::arch::mmap;
use iro::arch::x86_64;
use iro::ast::Visitor;
use iro::ssa::opt;
use iro::ssa::visitor::SSAVisitor;
use iro::utils;
use iro::runtime::Runtime;

pub fn parse_and_run(code: &str) {
    let ast = utils::parse_input(code).unwrap();
    let mut visitor = SSAVisitor::new(Runtime::new());
    visitor.visit_program(&ast).unwrap();
    let mut program = visitor.into_program().unwrap();
    let ssa_pipeline = utils::Pipeline::new(
        [
            opt::build_graph_and_rename_vars,
            opt::fold_constants,
            opt::remove_defined_never_used,
            opt::eliminate_phi,
            opt::data_flow_analysis,
        ]
        .to_vec(),
    );
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
    }
    let mut visitor = x86_64::visitor::Codegen::new();
    let contexts = visitor.process(&program).unwrap();
    unsafe {
        let mmap = mmap::Mmap::from_contexts(&contexts).unwrap();
        mmap.execute(&program.entry).unwrap();
    }
}