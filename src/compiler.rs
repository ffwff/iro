use crate::lexer;
use crate::ssa::isa::Context;
use crate::ssa::opt;
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
                    line_contents.push(&source[(row_start + 1)..idx]);
                    col = 0;
                    row += 1;
                    row_start = idx;
                }
                _ => {
                    col += 1;
                }
            }
        }
        line_contents.push(&source[row_start..source.len()]);
        {
            let row = current_span.0.row;
            println!(" {} | {}", row + 1, line_contents[row]);
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
            LalrParseError::ExtraToken { token: _ } => unimplemented!(),
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
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidToken => write!(f, "Invalid token"),
            ParseError::UnrecognizedEOF { expected } => write!(
                f,
                "Unexpected end of file, expected {}",
                if expected.len() >= 3 {
                    expected[0..3].join(", ") + "..."
                } else {
                    expected.join(", ")
                }
            ),
            ParseError::UnrecognizedToken { token, expected } => write!(
                f,
                "Unexpected token (got {}, expected {})",
                token,
                if expected.len() >= 3 {
                    expected[0..3].join(", ") + "..."
                } else {
                    expected.join(", ")
                }
            ),
        }
    }
}

pub fn ssa_pipeline() -> Pipeline<Context, fn(&mut Context) -> Flow> {
    Pipeline::new(
        [
            opt::insert_jmps,
            opt::build_graph_and_rename_vars,
            opt::fold_constants,
            opt::collect_garbage_vars,
            opt::separate_postlude,
            opt::cleanup_blocks,
            opt::fuse_postlude,
            opt::data_flow_analysis,
            opt::eliminate_phi,
        ]
        .to_vec(),
    )
}
