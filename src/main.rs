use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;

fn main() {
    let ast = utils::parse_input("
    def f(x,y)
        return x+y
    end
    def g(x,y)
        let z = f(x, y) + 10
        return z
    end
    g(1, 2)
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast);
    println!("{:#?}", visitor);
    // utils::ssa_visitor(&ast).unwrap();
}