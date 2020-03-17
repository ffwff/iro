use crate::ast;
use crate::lexer;
use crate::parser;
use crate::ast::Visitor;
mod rc_wrapper;
pub use rc_wrapper::RcWrapper;

pub type ParseError = lalrpop_util::ParseError<usize, lexer::Tok, lexer::Error>;
pub type ParseResult = Result<ast::Program, ParseError>;

pub fn parse_input(input: &str) -> ParseResult {
    let tokenizer = lexer::Lexer::new(input);
    parser::TopParser::new().parse(tokenizer)
}
