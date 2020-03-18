use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;

fn main() {
    let ast = utils::parse_input("
    let a = 1 + 2
    a = 2 + 3
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let func_contexts = visitor.into_func_contexts();
    println!("{:#?}", func_contexts);
    // utils::ssa_visitor(&ast).unwrap();
}