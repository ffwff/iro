use crate::lexer;
use crate::ssa::isa;
use std::rc::Rc;
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

pub enum Code {
    InvalidToken,
    UnrecognizedEOF {
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: lexer::Tok,
        expected: Vec<String>,
    },
    ExtraToken(lexer::Tok),
    UnexpectedCharacter(char),
    InvalidIndent,
    InvalidLHS,
    IncompatibleType { got: isa::Type, expected: isa::Type },
    CannotInfer,
    UnknownIdentifier(Rc<str>),
    UnknownType(Rc<str>),
    UnknownMemberRef(Rc<str>),
    UnknownStructField(Rc<str>),
    UnknownAttribute(String),
    UnknownStatic(String),
    NotEnoughArguments,
    InvalidArguments,
    InvalidReturnType,
    MutatingImmutable(Rc<str>),
}

impl std::fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Code::InvalidLHS => write!(f, "Invalid left-hand-side expression"),
            Code::IncompatibleType { got, expected } => {
                write!(f, "Incompatible type (got {}, expected {})", got, expected)
            }
            Code::CannotInfer => write!(f, "Cannot infer type for value"),
            Code::UnknownIdentifier(id) => write!(f, "Unknown identifier {:?}", id),
            Code::UnknownType(id) => write!(f, "Unknown type {:?}", id),
            Code::UnknownMemberRef(id) => write!(f, "Unknown member {:?}", id),
            Code::UnknownStructField(id) => write!(f, "Unknown struct field {:?}", id),
            Code::UnknownAttribute(id) => write!(f, "Unknown attribute {:?}", id),
            Code::UnknownStatic(id) => write!(f, "Unknown external function {:?}", id),
            Code::NotEnoughArguments => write!(f, "Not enough arguments for function"),
            Code::InvalidArguments => write!(f, "Invalid arguments for function"),
            Code::InvalidReturnType => write!(f, "Invalid return type"),
            Code::MutatingImmutable(id) => write!(f, "Mutating immutable variable {:?}", id),
            Code::UnexpectedCharacter(ch) => write!(f, "Unexpected character '{}'", ch),
            Code::InvalidIndent => write!(f, "Invalid indentation"),
            Code::InvalidToken => write!(f, "Invalid token"),
            Code::UnrecognizedEOF { expected } => if expected.is_empty() {
                write!(
                    f,
                    "Unexpected end of file",
                )
            } else {
                write!(
                    f,
                    "Unexpected end of file, expected {}",
                    expected.join(", ")
                )
            },
            Code::UnrecognizedToken { token, expected } => write!(
                f,
                "Unexpected token (got {}, expected {})",
                token,
                expected.join(", ")
            ),
            Code::ExtraToken(token) => write!(f, "Invalid token (got {})", token),
        }
    }
}

pub struct Error {
    pub error: Code,
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
                error: Code::InvalidToken,
                span: (location, location),
            },
            LalrParseError::UnrecognizedEOF { location, expected } => Error {
                error: Code::UnrecognizedEOF { expected },
                span: (location, location),
            },
            LalrParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Error {
                error: Code::UnrecognizedToken { token, expected },
                span: (start, end),
            },
            LalrParseError::ExtraToken {
                token: (start, token, end),
            } => Error {
                error: Code::ExtraToken(token),
                span: (start, end),
            },
            LalrParseError::User { error } => error,
        }
    }
}
