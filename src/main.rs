use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;

fn main() {
    let ast = utils::parse_input("
    def g(x,y)
        return x + y
    end
    g(1, 2)
    g(\"a\", \"a\")
    g(3, 4)
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    println!("{:#?}", visitor);
    // utils::ssa_visitor(&ast).unwrap();
}