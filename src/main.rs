#[macro_use]
extern crate maplit;
extern crate tempfile;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use iro::codegen::cranelift::CraneliftBackend;
use iro::codegen::settings::*;
use iro::compiler;
use iro::compiler::sources::Sources;
use iro::runtime;

use std::collections::BTreeMap;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::Builder;

macro_rules! fatal {
    ($options:expr, $first:expr) => {{
        print!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m ", $options.args[0]);
        println!($first);
        std::process::exit(-1);
    }};
    ($options:expr, $first:expr, $($last:expr),+) => {{
        print!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m ", $options.args[0]);
        println!($first, $($last),+);
        std::process::exit(-1);
    }};
}

#[derive(PartialEq)]
enum OutputType {
    Object,
    Executable,
}

struct Options<'a> {
    pub opt_level: OptLevel,
    pub output_type: OutputType,
    pub args: &'a Vec<String>,
    pub command_idx: usize,
}

impl<'a> Options<'a> {
    pub fn to_settings(&self) -> Settings {
        Settings {
            opt_level: self.opt_level,
        }
    }

    pub fn to_compiler(&self) -> compiler::Compiler {
        let settings = self.to_settings();
        compiler::Compiler::new(CraneliftBackend::backend(), settings)
    }

    pub fn arg(&self, offset: usize) -> Option<&String> {
        self.args.get(self.command_idx + offset)
    }
}

type CommandFn = fn(Options);

fn compiler_error(opts: Options, sources: Option<Sources>, error: compiler::Error) -> ! {
    if let Some(span) = error.span {
        let sources = sources.unwrap();
        let mut message = String::new();
        write!(&mut message, "{}", error.error).unwrap();

        let mut labels = vec![Label::primary(span.file, span.start..span.end)];
        for (span_idx, msg) in error.error.diagnostics() {
            let span = sources.get_span(span_idx).unwrap();
            labels.push(
                Label::secondary(span.file, span.start..span.end).with_message(msg.to_string()),
            );
        }

        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_labels(labels);
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        codespan_reporting::term::emit(&mut writer.lock(), &config, &sources, &diagnostic).unwrap();
    } else {
        eprintln!("\x1b[1m\x1b[38;5;11m{}:\x1b[0m {:?}", opts.args[0], error);
    }
    std::process::exit(-1)
}

fn run(opts: Options) {
    let input_name = opts
        .arg(1)
        .unwrap_or_else(|| fatal!(opts, "No input specified"));
    unsafe {
        if let (sources, Err(error)) = opts
            .to_compiler()
            .parse_and_run(input_name, &runtime::Runtime::new())
        {
            compiler_error(opts, sources, error);
        }
    }
}

fn build(opts: Options) {
    let input_name = opts
        .arg(1)
        .unwrap_or_else(|| fatal!(opts, "No input specified"));
    let (_sources, bytes) = match opts.to_compiler().parse_to_object(input_name) {
        (sources, Ok(x)) => (sources, x),
        (sources, Err(error)) => compiler_error(opts, sources, error),
    };
    if opts.output_type == OutputType::Object {
        let output_name = opts
            .arg(2)
            .map(|name| PathBuf::from(name))
            .unwrap_or_else(|| Path::new(&input_name).with_extension("o"));
        std::fs::write(&output_name, bytes)
            .unwrap_or_else(|e| fatal!(opts, "unable to write to file: {}", e));
        return;
    }
    let output_name = opts
        .arg(2)
        .map(|name| PathBuf::from(name))
        .unwrap_or_else(|| Path::new(&input_name).with_extension(""));
    let object_name = Builder::new()
        .rand_bytes(5)
        .suffix(".o")
        .tempfile()
        .unwrap_or_else(|e| fatal!(opts, "unable to create temp file: {}", e));
    std::fs::write(&object_name, bytes)
        .unwrap_or_else(|e| fatal!(opts, "unable to write to file: {}", e));
    let args = vec![
        "-o".to_string(),
        output_name.to_string_lossy().to_string(),
        object_name.path().to_string_lossy().to_string(),
    ];
    let output = Command::new("gcc")
        .args(&args)
        .output()
        .unwrap_or_else(|e| fatal!(opts, "unable to spawn gcc: {}", e));
    if !output.status.success() {
        fatal!(
            opts,
            "gcc error\n{}",
            String::from_utf8_lossy(&output.stderr)
        )
    }
}

fn usage(program: &String, commands: &BTreeMap<&str, (&str, CommandFn)>) {
    println!("Usage:\n\t{} [options] <command> [file]\n", program);
    println!("Commands:");
    for (name, (desc, _)) in commands {
        println!("\t{}: {}", name, desc);
    }
    println!("\nOptions:");
    let mut options = [
        ("-obj", "generate an object file instead of executable"),
        ("-O", "optimize for size and speed"),
        ("-Ospeed", "optimize for speed"),
    ];
    options.sort_by_key(|k| k.0);
    for (name, desc) in &options {
        println!("\t{}: {}", name, desc)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let program = &args[0];
    let commands: BTreeMap<&str, (&str, CommandFn)> = btreemap![
        "run" => ("Runs the specified program", run as CommandFn),
        "build" => ("Builds the specified program", build as CommandFn),
    ];
    let mut opts = Options {
        opt_level: OptLevel::None,
        output_type: OutputType::Executable,
        args: &args,
        command_idx: 0,
    };
    for (idx, arg) in args.iter().skip(1).enumerate() {
        match arg.as_str() {
            "-obj" => {
                opts.output_type = OutputType::Object;
                continue;
            }
            "-Ospeed" => {
                opts.opt_level = OptLevel::Speed;
                continue;
            }
            "-O" => {
                opts.opt_level = OptLevel::SpeedAndSize;
                continue;
            }
            _ => (),
        }
        if let Some(command) = commands.get(arg.as_str()) {
            let func = command.1;
            opts.command_idx = idx + 1;
            return func(opts);
        } else {
            break;
        }
    }
    return usage(program, &commands);
}
