use crate::ast::Visitor;
// use crate::codegen::cranelift::{Codegen, Settings};
use crate::codegen::settings::Settings;
use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::runtime;
use crate::ssa;
// use cranelift_module::FuncOrDataId;

pub mod optcell;
pub mod pipeline;
pub mod uniquerc;

#[macro_use]
macro_rules! dbg_println {
    ($first:expr) => {{
        if cfg!(debug_assertions) {
            eprint!("\x1b[1m\x1b[38;5;11m{}:{}:{}:\x1b[0m ", file!(), line!(), column!());
            eprintln!($first)
        }
    }};
    ($first:expr, $($last:expr),+) => {{
        if cfg!(debug_assertions) {
            eprint!("\x1b[1m\x1b[38;5;11m{}:{}:{}:\x1b[0m ", file!(), line!(), column!());
            eprintln!($first, $($last),+)
        }
    }};
}

pub fn parse_to_ssa(input: &str) -> Result<ssa::isa::Program, compiler::Error> {
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    let top_level_info = optcell::OptCell::new(ssa::visitor::TopLevelInfo::new());
    let mut visitor = ssa::visitor::SSAVisitor::new(&top_level_info);
    visitor.visit_program(&ast)?;
    let mut program = visitor.into_program();
    let ssa_pipeline = compiler::ssa_pipeline();
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
    }
    Ok(program)
}

pub fn parse_and_run(
    settings: Settings,
    input: &str,
    runtime: runtime::Runtime,
) -> Result<(), compiler::Error> {
    let program = parse_to_ssa(input)?;
    dbg_println!("\n{}", program.print());
    unimplemented!()
    /* let mut module = Codegen::process_jit(&program, &runtime);
    if let Some(main) = module.get_name("main()") {
        if let FuncOrDataId::Func(func_id) = main {
            let function = module.get_finalized_function(func_id);
            let main_fn = unsafe { std::mem::transmute::<_, extern "C" fn()>(function) };
            main_fn();
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }
    Ok(()) */
}

pub fn parse_to_object(settings: Settings, input: &str) -> Result<Vec<u8>, compiler::Error> {
    let program = parse_to_ssa(input)?;
    dbg_println!("\n{}", program.print());
    unimplemented!()
    // let module = Codegen::process_object(isa, &program);
    // Ok(module.finish().emit().unwrap())
}
