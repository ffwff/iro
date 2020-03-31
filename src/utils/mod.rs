use crate::ast;
use crate::ast::Visitor;
use crate::codegen::codegen::Codegen;
use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::runtime;
use crate::ssa;
use cranelift_module::FuncOrDataId;
use std::cell::RefCell;
use std::error::Error;

pub mod pipeline;

pub type ParseError = lalrpop_util::ParseError<usize, lexer::Tok, lexer::Error>;
pub type ParseResult = Result<ast::Program, ParseError>;

pub fn parse_and_run(input: &str, runtime: runtime::Runtime) -> Result<(), Box<dyn Error>> {
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    let top_level_info = RefCell::new(ssa::visitor::TopLevelInfo::new());
    let mut visitor = ssa::visitor::SSAVisitor::new(&top_level_info);
    visitor.visit_program(&ast)?;
    let mut program = visitor.into_program();
    let ssa_pipeline = compiler::ssa_pipeline();
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
    }
    let mut module = Codegen::process_jit(&program, &runtime);
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
    Ok(())
}

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
