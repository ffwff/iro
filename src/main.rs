use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ast::Visitor;
use iro::opt;
use iro::arch::x86_64;

fn main() {
    let ast = utils::parse_input("
    let i = 0
    while i < 5
        i += 1
    end
    ").unwrap();
    println!("---\n{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let mut func_contexts = visitor.into_func_contexts().unwrap();
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::preprocess::preprocess(func_contexts);
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::ssa::eliminate_unused(func_contexts);
    println!("---\n{:#?}", func_contexts);
    func_contexts = opt::ssa::eliminate_consts(func_contexts);
    println!("---\n{:#?}", func_contexts);
    let mut visitor = x86_64::visitor::FuncContextVisitor::new();
    visitor.process(&func_contexts);
    // utils::ssa_visitor(&ast).unwrap();
}