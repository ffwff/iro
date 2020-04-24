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
}

const SSA_PASSES: &'static [fn(&mut ssa::isa::Context) -> Flow] = &[
    // Stage 0:
    passes::graph::preprocess,
    passes::graph::build_graph,
    passes::gc::collect_garbage_vars_with_multiple_assigns,
    // Stage 1:
    passes::dfa::data_flow_analysis,
    passes::ssa::rename_vars_and_insert_phis,
    passes::fold::fold_constants,
    passes::gc::collect_garbage_vars,
    // Stage 2:
    passes::postlude::separate_postlude,
    passes::graph::cleanup_jump_blocks,
    passes::postlude::fuse_postlude,
    passes::graph::build_graph,
    // Stage 3:
    passes::dfa::data_flow_analysis,
    passes::postlude::separate_postlude,
    passes::dfa::drop_insertion,
    passes::postlude::fuse_postlude,
    // Stage 4:
    // passes::memcheck::move_checker::check,
    passes::ssa::eliminate_phi,
];

pub fn parse_to_ssa(input: &str) -> Result<ssa::isa::Program, compiler::Error> {
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    let top_level_info = OptCell::new(ssa::visitor::TopLevelInfo::new());
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
