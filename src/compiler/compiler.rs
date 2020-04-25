use crate::ast::Visitor;
use crate::codegen::backend;
use crate::codegen::settings::Settings;
use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::runtime;
use crate::ssa;
use crate::ssa::passes;
use crate::utils::optcell::OptCell;

pub enum Flow {
    Continue,
    Break,
    Err,
}

const SSA_PASSES: &'static [fn(&mut ssa::isa::Context) -> Flow] = &[
    // Stage 0: preprocess and build the IR graph
    passes::graph::preprocess,
    passes::graph::build_graph,
    passes::gc::collect_garbage_vars_with_multiple_assigns,
    // Stage 1: "SSA" generation
    passes::dfa::data_flow_analysis,
    passes::ssa::rename_vars_and_insert_phis,
    // Stage 2: constant folding and graph cleanup
    passes::fold::fold_constants,
    passes::gc::collect_garbage_vars,
    passes::postlude::separate_postlude,
    passes::graph::cleanup_jump_blocks,
    passes::postlude::fuse_postlude,
    passes::graph::build_graph,
    // Stage 3: SSA elimination and high-level memory ops
    passes::ssa::eliminate_phi,
    passes::dfa::data_flow_analysis,
    passes::postlude::separate_postlude,
    passes::dfa::drop_insertion,
    // passes::memcheck::check,
    passes::postlude::fuse_postlude,
];

pub fn parse_to_ssa(input: &str) -> Result<ssa::isa::Program, compiler::Error> {
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    let top_level_info = OptCell::some(ssa::visitor::TopLevelInfo::new());
    let mut visitor = ssa::visitor::SSAVisitor::new(&top_level_info);
    visitor.visit_program(&ast)?;
    let mut program = visitor.into_program();
    for (_, context) in &mut program.contexts {
        for pass in SSA_PASSES {
            match pass(context) {
                Flow::Continue => (),
                Flow::Break => {
                    break;
                }
                Flow::Err => {
                    return Err(compiler::Error {
                        error: Box::new("ssa pass error"),
                        span: (0, 0),
                    });
                }
            }
        }
    }
    Ok(program)
}

pub struct Compiler {
    backend: backend::Backend,
    settings: Settings,
}

impl Compiler {
    pub fn new(backend: backend::Backend, settings: Settings) -> Self {
        Self { backend, settings }
    }

    pub unsafe fn parse_and_run(
        &self,
        input: &str,
        runtime: &runtime::Runtime,
    ) -> Result<(), compiler::Error> {
        let program = parse_to_ssa(input)?;
        if let Some(backend) = self.backend.as_jit_backend() {
            backend.run(&program, &self.settings, runtime)
        } else {
            panic!()
        }
    }

    pub fn parse_to_object(&self, input: &str) -> Result<Vec<u8>, compiler::Error> {
        let program = parse_to_ssa(input)?;
        if let Some(backend) = self.backend.as_object_backend() {
            backend.generate_object(&program, &self.settings)
        } else {
            panic!()
        }
    }
}
