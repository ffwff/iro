use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;
use iro::opt;

fn main() {
    let ast = utils::parse_input("
    let a = 1
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let mut func_contexts = visitor.into_func_contexts().unwrap();
    func_contexts = opt::preprocess::preprocess(func_contexts);
    println!("{:#?}", func_contexts);
    // utils::ssa_visitor(&ast).unwrap();
}