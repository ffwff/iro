use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;

fn main() {
    let ast = utils::parse_input("
    def f(x)
        if 10
            return x + 1
        else
            let y = if 10
                return \"A\"
            end
            y * 64
        end
    end
    let x = 10
    let y = f(x)
    ").unwrap();
    println!("{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let func_contexts = visitor.into_func_contexts();
    println!("{:#?}", func_contexts);
    // utils::ssa_visitor(&ast).unwrap();
}