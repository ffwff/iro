use crate::compiler::sources::{SourceSpan, SpanIndex};
use crate::lexer;
use crate::ssa::isa;
use lalrpop_util;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryErrorType {
    Move,
    Borrow,
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
    IncompatibleType {
        got: isa::Type,
        expected: isa::Type,
    },
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
    BackendNoSupport,
    IoError(std::io::Error),
    MemoryError {
        position: SpanIndex,
        last_used: SpanIndex,
        var: isa::Variable,
        typed: MemoryErrorType,
    },
}

impl Code {
    pub fn diagnostics(&self) -> Vec<(SpanIndex, &'static str)> {
        match self {
            Code::MemoryError { last_used, .. } => vec![(*last_used, "last used here")],
            _ => vec![],
        }
    }
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
            Code::UnrecognizedEOF { expected } => {
                if expected.is_empty() {
                    write!(f, "Unexpected end of file",)
                } else {
                    write!(
                        f,
                        "Unexpected end of file, expected {}",
                        expected.join(", ")
                    )
                }
            }
            Code::UnrecognizedToken { token, expected } => write!(
                f,
                "Unexpected token (got {}, expected {})",
                token,
                expected.join(", ")
            ),
            Code::ExtraToken(token) => write!(f, "Invalid token (got {})", token),
            Code::BackendNoSupport => {
                write!(f, "Current backend doesn't support this functionality")
            }
            Code::IoError(error) => write!(f, "IO error: {}", error),
            Code::MemoryError { typed, .. } => write!(
                f,
                "Trying to {} a variable which has already been {}",
                match typed {
                    MemoryErrorType::Move => "move",
                    MemoryErrorType::Borrow => "borrow",
                },
                match typed {
                    MemoryErrorType::Move => "moved",
                    MemoryErrorType::Borrow => "borrowed",
                }
            ),
        }
    }
}

pub struct Error {
    pub error: Code,
    pub span: Option<SourceSpan>,
}

impl Error {
    pub fn io_error(error: std::io::Error) -> Self {
        Self {
            error: Code::IoError(error),
            span: None,
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
        unimplemented!()
        /* match error {
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
        }*/
    }
}
