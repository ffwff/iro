#![feature(fixed_size_array)]

use iro::arch::mmap;
use iro::arch::x86_64;
use iro::ast::Visitor;
use iro::ssa::opt;
use iro::ssa::visitor::SSAVisitor;
use iro::utils;
use core::array::FixedSizeArray;

fn main() {
    let ast = utils::parse_input(
        "
    def f(a,b)
        let x = 0
        while x < 10
            x += 1
        end
        return x
    end
    f(1,2)
    ",
    )
    .unwrap();
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let mut func_contexts = visitor.into_func_contexts().unwrap();
    utils::ssa_pipeline(&mut func_contexts, [
        opt::build_graph_and_rename_vars,
        opt::remove_defined_never_used,
        opt::eliminate_phi,
        opt::data_flow_analysis
    ].as_slice());
    let mut visitor = x86_64::visitor::FuncContextVisitor::new();
    let contexts = visitor.process(&func_contexts).unwrap();
    unsafe {
        let mmap = mmap::Mmap::from_contexts(&contexts).unwrap();
        mmap.execute();
    }
}
