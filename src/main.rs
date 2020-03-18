use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;

fn main() {
    let ast = utils::parse_input("
    let x = if 1
        12
    else
    end
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let func_contexts = visitor.into_func_contexts();
    println!("{:#?}", func_contexts);
    // utils::ssa_visitor(&ast).unwrap();
}