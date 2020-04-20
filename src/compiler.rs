use crate::ast::Visitor;
use crate::codegen::backend;
use crate::codegen::settings::Settings;
use crate::compiler;
use crate::lexer;
use crate::parser;
use crate::runtime;
use crate::ssa;
use crate::ssa::isa::Context;
use crate::ssa::passes;
use crate::utils::optcell::OptCell;
use crate::utils::pipeline::*;
use lalrpop_util;

struct Location {
    pub row: usize,
    pub col: usize,
}

impl Location {
    pub fn new() -> Location {
        Location { row: 0, col: 0 }
    }
}

pub struct Error {
    pub error: Box<dyn std::fmt::Display>,
    pub span: (usize, usize),
}

impl Error {
    pub fn print(&self, source: &String) {
        println!("{}", self.error);
        let mut line_contents = vec![];
        let mut row = 0;
        let mut col = 0;
        let mut row_start = 0;
        let mut current_span = (Location::new(), Location::new());
        for (idx, ch) in source.chars().enumerate() {
            if idx == self.span.0 {
                current_span.0.row = row;
                current_span.0.col = col;
            } else if idx == self.span.1 {
                current_span.1.row = row;
                current_span.1.col = col;
            }
            match ch {
                '\n' => {
                    line_contents.push(&source[row_start..idx]);
                    col = 0;
                    row += 1;
                    row_start = idx + 1;
                }
                _ => {
                    col += 1;
                }
            }
        }
        line_contents.push(&source[row_start..source.len()]);
        {
            let row = current_span.0.row;
            println!(" {}:{} | {}", row + 1, col + 1, line_contents[row]);
        }
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.error)
    }
}

type LalrParseError = lalrpop_util::ParseError<usize, lexer::Tok, Error>;
impl From<LalrParseError> for Error {
    fn from(error: LalrParseError) -> Error {
        match error {
            LalrParseError::InvalidToken { location } => Error {
                error: Box::new(ParseError::InvalidToken),
                span: (location, location),
            },
            LalrParseError::UnrecognizedEOF { location, expected } => Error {
                error: Box::new(ParseError::UnrecognizedEOF { expected }),
                span: (location, location),
            },
            LalrParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Error {
                error: Box::new(ParseError::UnrecognizedToken { token, expected }),
                span: (start, end),
            },
            LalrParseError::ExtraToken {
                token: (start, token, end),
            } => Error {
                error: Box::new(ParseError::ExtraToken(token)),
                span: (start, end),
            },
            LalrParseError::User { error } => error,
        }
    }
}

enum ParseError {
    InvalidToken,
    UnrecognizedEOF {
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: lexer::Tok,
        expected: Vec<String>,
    },
    ExtraToken(lexer::Tok),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidToken => write!(f, "Invalid token"),
            ParseError::UnrecognizedEOF { expected } => write!(
                f,
                "Unexpected end of file, expected {}",
                expected.join(", ")
            ),
            ParseError::UnrecognizedToken { token, expected } => write!(
                f,
                "Unexpected token (got {}, expected {})",
                token,
                expected.join(", ")
            ),
            ParseError::ExtraToken(token) => write!(f, "Invalid token (got {})", token),
        }
    }
}

pub fn ssa_pipeline() -> Pipeline<Context, fn(&mut Context) -> Flow> {
    Pipeline::new(
        [
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
            passes::postlude::separate_postlude,
            passes::dfa::data_flow_analysis,
            passes::postlude::fuse_postlude,
            // TODO: drop insertion pass
            // Stage 4:
            passes::ssa::eliminate_phi,
        ]
        .to_vec(),
    )
}

pub fn parse_to_ssa(input: &str) -> Result<ssa::isa::Program, compiler::Error> {
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer)?;
    let top_level_info = OptCell::new(ssa::visitor::TopLevelInfo::new());
    let mut visitor = ssa::visitor::SSAVisitor::new(&top_level_info);
    visitor.visit_program(&ast)?;
    let mut program = visitor.into_program();
    let ssa_pipeline = compiler::ssa_pipeline();
    for (_, context) in &mut program.contexts {
        ssa_pipeline.apply(context);
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
