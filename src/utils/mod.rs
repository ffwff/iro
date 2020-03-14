use crate::ast;
use crate::lexer;
use crate::parser;
use crate::ast::Visitor;
use crate::types::type_visitor::TypeVisitor;
use crate::ssa::visitor::SSAVisitor;

#[cfg(test)]
use crate::types::types::{TypeInfo, FunctionData};

mod rc_wrapper;
pub use rc_wrapper::RcWrapper;

pub type ParseError = lalrpop_util::ParseError<usize, lexer::Tok, lexer::Error>;
pub type ParseResult = Result<ast::Program, ParseError>;

pub fn parse_input(input: &str) -> ParseResult {
    let tokenizer = lexer::Lexer::new(input);
    parser::TopParser::new().parse(tokenizer)
}

pub fn type_visitor(ast : &ast::Program) -> ast::VisitorResult {
    let mut visitor = TypeVisitor::new();
    visitor.visit_program(&ast)
}

pub fn ssa_visitor(ast : &ast::Program) -> Result<SSAVisitor, ast::Error> {
    let mut visitor = SSAVisitor::new();
    visitor.visit_program(&ast)?;
    Ok(visitor)
}

#[cfg(test)]
pub fn check_function_data<T>(boxed : &iro::ast::NodeBox, callback : T) where T : Fn(&FunctionData) {
    use std::cell::RefCell;
    use std::borrow::Borrow;
    let type_info : &TypeInfo = &boxed.type_info().borrow();
    let function = type_info.as_function().unwrap();
    let data_rc : &RefCell<FunctionData> = function.borrow();
    let data : &FunctionData = &data_rc.borrow();
    callback(data);
}