use std::collections::BTreeMap;
#[macro_use]
extern crate maplit;
use iro::arch::codegen::Codegen;
use iro::arch::{current_codegen, mmap};
use iro::ast::Visitor;
use iro::runtime::Runtime;
use iro::ssa::visitor::SSAVisitor;
use iro::utils;
use iro::compiler;

fn parse_and_run(code: &str) {
    let ast = utils::parse_input(code).unwrap();
    let mut visitor = SSAVisitor::new(Runtime::new());
    visitor.visit_program(&ast).unwrap();
    let mut program = visitor.into_program().unwrap();
    let ssa_pipeline = compiler::ssa_pipeline();
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
    }
    let mut visitor = current_codegen();
    let contexts = visitor.process(&program).unwrap();
    unsafe {
        let mmap = visitor.make_mmap(&contexts).unwrap();
        mmap.execute(&program.entry).unwrap();
    }
}

macro_rules! error {
    ($args:expr, $first:expr) => {{
        print!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m ", $args[0]);
        println!($first)
    }};
    ($args:expr, $first:expr, $($last:expr),+) => {{
        print!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m ", $args[0]);
        println!($first, $($last),+)
    }};
}

fn run(idx: usize, args: &Vec<String>) {
    if args.len() - 1 == idx {
        error!(args, "no input files");
        return;
    }
    match std::fs::read_to_string(args.last().unwrap()) {
        Ok(source) => parse_and_run(&source),
        Err(err) => {
            error!(args, "{}", err);
        }
    }
}

fn usage<F>(program: &String, commands: &BTreeMap<&str, (&str, F)>) {
    println!("Usage:\n\t{} [options] <command> [file]\n", program);
    println!("Commands:");
    for (name, (desc, _)) in commands {
        println!("\t{}: {}", name, desc);
    }
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
            return func(idx + 1, &args);
        } else {
            break;
        }
    }
    return usage(&program, &commands);
}
