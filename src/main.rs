use iro::utils;
use iro::ssa::visitor::SSAVisitor;
use iro::ssa::opt;
use iro::ast::Visitor;
use iro::arch::x86_64;
use iro::arch::mmap;

fn main() {
    let ast = utils::parse_input("
    def f(a,b)
        if a > 5
            return a + b + 5
        end
        return a
    end
    f(1,2)
    ").unwrap();
    println!("---\n{:#?}", ast);
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let mut func_contexts = visitor.into_func_contexts().unwrap();
    func_contexts = opt::build_graph_and_rename_vars(func_contexts);
    func_contexts = opt::remove_defined_never_used(func_contexts);
    func_contexts = opt::data_flow_analysis(func_contexts);
    func_contexts = opt::eliminate_phi(func_contexts);
    let mut visitor = x86_64::visitor::FuncContextVisitor::new();
    let contexts = visitor.process(&func_contexts).unwrap();
    unsafe {
        let mmap = mmap::Mmap::from_contexts(&contexts).unwrap();
        println!("{:#?}", mmap);
        mmap.execute();
    }
}