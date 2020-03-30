use crate::ast;
use crate::ast::Visitor;
use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::runtime;
use crate::ssa;
use std::error::Error;

mod rc_wrapper;
pub use rc_wrapper::RcWrapper;

pub mod pipeline;

pub type ParseError = lalrpop_util::ParseError<usize, lexer::Tok, lexer::Error>;
pub type ParseResult = Result<ast::Program, ParseError>;

pub fn parse_input(input: &str) -> ParseResult {
    let tokenizer = lexer::Lexer::new(input);
    parser::TopParser::new().parse(tokenizer)
}

pub fn parse_and_run(code: &str) -> Result<(), Box<Error>> {
    let ast = parse_input(code)?;
    let mut visitor = ssa::visitor::SSAVisitor::new(runtime::Runtime::new());
    visitor.visit_program(&ast)?;
    let mut program = visitor.into_program().unwrap();
    let ssa_pipeline = compiler::ssa_pipeline();
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
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
