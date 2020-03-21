use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;
use iro::opt;
use iro::arch::x86_64;

fn main() {
    let ast = utils::parse_input("
    let a = 1
    while a < 5
        a = 2
    end
    let b = a + 3
    ").unwrap();
    println!("---\n{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let mut func_contexts = visitor.into_func_contexts().unwrap();
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::preprocess::build_graph_and_rename_vars(func_contexts);
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::preprocess::remove_defined_never_used(func_contexts);
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::preprocess::data_flow_analysis(func_contexts);
    println!("---\n{:#?}", func_contexts);
    /* func_contexts = opt::preprocess::remove_unused_local_vars(func_contexts);
    println!("---\n{:#?}", func_contexts); */
    /* func_contexts = opt::ssa::eliminate_unused(func_contexts);
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::ssa::eliminate_consts(func_contexts);
    println!("---\n{:#?}", func_contexts);
    let mut visitor = x86_64::visitor::FuncContextVisitor::new();
    visitor.process(&func_contexts); */
    // utils::ssa_visitor(&ast).unwrap();
}