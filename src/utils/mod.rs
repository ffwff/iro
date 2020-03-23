use crate::ast;
use crate::lexer;
use crate::parser;

mod rc_wrapper;
pub use rc_wrapper::RcWrapper;

pub type ParseError = lalrpop_util::ParseError<usize, lexer::Tok, lexer::Error>;
pub type ParseResult = Result<ast::Program, ParseError>;

pub fn parse_input(input: &str) -> ParseResult {
    let tokenizer = lexer::Lexer::new(input);
    parser::TopParser::new().parse(tokenizer)
}

#[macro_use]
macro_rules! dbg_println {
    ($first:expr, $($last:expr),+) => {{
        if cfg!(debug_assertions) {
            print!("\x1b[1m\x1b[38;5;11m{}:{}:{}:\x1b[0m ", file!(), line!(), column!());
            println!($first, $($last),+)
        }
    }};
}