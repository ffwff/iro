use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;

fn main() {
    let ast = utils::parse_input("
    if 1
    2
    else
    1
    end
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    println!("{:#?}", visitor);
    // utils::ssa_visitor(&ast).unwrap();
}