use crate::ast::pp_visitor::PreprocessVisitor;
use crate::codegen::backend;
use crate::codegen::settings::Settings;
use crate::compiler;
use crate::compiler::sources::Sources;
use crate::lexer;
use crate::parser;
use crate::runtime;
use crate::ssa;
use crate::ssa::passes;
use std::cell::RefCell;
use std::path::Path;

pub mod error;
pub use error::Error;

pub mod sources;

pub enum Flow {
    Continue,
    Break,
    Err(compiler::error::Code),
}

const SSA_PASSES: &[fn(&mut ssa::isa::Context) -> Flow] = &[
    // Stage 0: preprocess and build the SSA graph
    passes::graph::preprocess,
    passes::graph::build_graph,
    passes::mem::register_to_memory,
    passes::ssa::rename_vars_and_insert_phis,
    // Stage 1: constant folding and graph cleanup
    passes::fold::fold_constants,
    passes::gc::collect_garbage_vars,
    passes::postlude::separate_postlude,
    passes::graph::cleanup_jump_blocks,
    passes::postlude::fuse_postlude,
    passes::graph::build_graph,
    // Stage 2: SSA elimination and high-level memory ops insertion
    passes::postlude::separate_postlude,
    passes::mem::eliminate_phi,
    passes::mem::calculate_block_variable_declaration,
    passes::mem::calculate_data_flow,
    passes::mem::reference_drop_insertion,
    passes::memcheck::check,
    passes::postlude::fuse_postlude,
    passes::postprocess::cleanup_high_level_instructions,
];

pub fn parse_file_to_ssa(sources: &mut Sources) -> Result<ssa::isa::Program, compiler::Error> {
    let tokenizer = lexer::Lexer::new(sources.main_file().unwrap(), 0);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    PreprocessVisitor::postprocess(&ast, 0)?;
    let mut program = ssa::visitor::SSAVisitor::generate(&ast, &RefCell::new(sources))?;
    process_ssa(sources, &mut program)?;
    Ok(program)
}

pub fn parse_source_to_ssa(source: &str) -> Result<ssa::isa::Program, compiler::Error> {
    let mut sources = Sources::new();
    let tokenizer = lexer::Lexer::new(source, 0);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    PreprocessVisitor::postprocess(&ast, 0)?;
    let mut program = ssa::visitor::SSAVisitor::generate(&ast, &RefCell::new(&mut sources))?;
    process_ssa(&mut sources, &mut program)?;
    Ok(program)
}

fn process_ssa(
    sources: &mut Sources,
    program: &mut ssa::isa::Program,
) -> Result<(), compiler::Error> {
    for context in program.contexts.values_mut() {
        for pass in SSA_PASSES {
            match pass(context) {
                Flow::Continue => (),
                Flow::Break => {
                    break;
                }
                Flow::Err(code) => {
                    use crate::compiler::error::Code;
                    let span = match &code {
                        Code::MemoryError { position, .. } => sources.get_span(*position),
                        _ => None,
                    };
                    return Err(compiler::Error { error: code, span });
                }
            }
        }
    }
    Ok(())
}

pub struct Compiler {
    backend: backend::Backend,
    settings: Settings,
}

macro_rules! try_sources {
    ($expr:expr, $sources:expr) => {
        match $expr {
            Ok(x) => x,
            Err(x) => return (Some($sources), Err(x)),
        }
    };
}

macro_rules! with_sources {
    ($expr:expr, $sources:expr) => {
        (Some($sources), $expr)
    };
}

impl Compiler {
    pub fn new(backend: backend::Backend, settings: Settings) -> Self {
        Self { backend, settings }
    }

    /// # Safety
    ///
    /// This function calls into possibly unsafe iroha code.
    pub unsafe fn parse_and_run<P: AsRef<Path>>(
        &self,
        path: P,
        runtime: &runtime::Runtime,
    ) -> (Option<Sources>, Result<(), compiler::Error>) {
        let mut sources = match Sources::with_main_file(path.as_ref()) {
            Ok(sources) => sources,
            Err(error) => return (None, Err(compiler::Error::io_error(error))),
        };
        let program = try_sources!(parse_file_to_ssa(&mut sources), sources);
        with_sources!(
            if let Some(backend) = self.backend.as_jit_backend() {
                backend.run(&program, &self.settings, runtime)
            } else {
                Err(compiler::Error {
                    error: compiler::error::Code::BackendNoSupport,
                    span: None,
                })
            },
            sources
        )
    }

    pub fn parse_to_object<P: AsRef<Path>>(
        &self,
        path: P,
    ) -> (Option<Sources>, Result<Vec<u8>, compiler::Error>) {
        let mut sources = match Sources::with_main_file(path.as_ref()) {
            Ok(sources) => sources,
            Err(error) => return (None, Err(compiler::Error::io_error(error))),
        };
        let program = try_sources!(parse_file_to_ssa(&mut sources), sources);
        with_sources!(
            if let Some(backend) = self.backend.as_object_backend() {
                backend.generate_object(&program, &self.settings)
            } else {
                Err(compiler::Error {
                    error: compiler::error::Code::BackendNoSupport,
                    span: None,
                })
            },
            sources
        )
    }
}
