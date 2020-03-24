use std::collections::BTreeMap;
#[macro_use]
extern crate maplit;
use iro::arch::mmap;
use iro::arch::x86_64;
use iro::ast::Visitor;
use iro::ssa::opt;
use iro::ssa::visitor::SSAVisitor;
use iro::utils;

fn parse_and_run(code: &str) {
    let ast = utils::parse_input(code).unwrap();
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast).unwrap();
    let mut program = visitor.into_program().unwrap();
    let ssa_pipeline = utils::Pipeline::new(
        [
            opt::build_graph_and_rename_vars,
            opt::remove_defined_never_used,
            opt::eliminate_phi,
            opt::data_flow_analysis,
        ]
        .to_vec(),
    );
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
    }
    let mut visitor = x86_64::visitor::Codegen::new();
    let contexts = visitor.process(&program).unwrap();
    unsafe {
        let mmap = mmap::Mmap::from_contexts(&contexts).unwrap();
        mmap.execute(&program.entry);
    }
}

fn run(program: &String, file: &String) {
    let source = std::fs::read_to_string(file).unwrap();
    parse_and_run(&source);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let program = &args[0];
    let commands = btreemap![
        "run" => ("Runs the specified program", run),
    ];
    for (idx, arg) in args.iter().skip(1).enumerate() {
        if let Some(command) = commands.get(arg.as_str()) {
            let func = command.1;
            return func(&program, args.last().unwrap());
        } else {
            println!("Usage:\n\t{} [options] <command> [file]\n", program);
            println!("Commands:");
            for (name, (desc, _)) in &commands {
                println!("\t{}: {}", name, desc);
            }
        }
    }
}
