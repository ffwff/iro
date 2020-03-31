#[macro_use]
extern crate maplit;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use iro::runtime;
use iro::utils;

macro_rules! fatal {
    ($args:expr, $first:expr) => {{
        print!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m ", $args[0]);
        println!($first);
        std::process::exit(-1);
    }};
    ($args:expr, $first:expr, $($last:expr),+) => {{
        print!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m ", $args[0]);
        println!($first, $($last),+);
        std::process::exit(-1);
    }};
}

fn run(idx: usize, args: &Vec<String>) {
    let input_name = args.get(idx + 1).unwrap_or_else(|| fatal!(args, "No input specified"));
    match std::fs::read_to_string(input_name) {
        Ok(source) => {
            utils::parse_and_run(&source, runtime::Runtime::new()).unwrap();
        }
        Err(err) => {
            fatal!(args, "{}", err);
        }
    }
}

fn build(idx: usize, args: &Vec<String>) {
    let input_name = args.get(idx + 1).unwrap_or_else(|| fatal!(args, "No input specified"));
    let object_name = args.get(idx + 2)
        .map(|name| PathBuf::from(name))
        .unwrap_or_else(|| Path::new(&input_name).with_extension("o"));
    match std::fs::read_to_string(input_name) {
        Ok(source) => {
            let bytes = utils::parse_to_object(&source).unwrap();
            std::fs::write(object_name, bytes).unwrap_or_else(|e| fatal!(args, "{}", e));
        }
        Err(err) => {
            fatal!(args, "{}", err);
        }
    }
}

type CommandFn = fn(usize, &Vec<String>);

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let program = &args[0];
    let commands: BTreeMap<&str, (&str, CommandFn)> = btreemap![
        "run" => ("Runs the specified program", run as CommandFn),
        "build" => ("Builds the specified program as an object", build as CommandFn),
    ];
    for (idx, arg) in args.iter().skip(1).enumerate() {
        if let Some(command) = commands.get(arg.as_str()) {
            let func = command.1;
            return func(idx + 1, &args);
        } else {
            break;
        }
    }
    println!("Usage:\n\t{} [options] <command> [file]\n", program);
    println!("Commands:");
    for (name, (desc, _)) in commands {
        println!("\t{}: {}", name, desc);
    }
}
