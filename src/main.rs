#![feature(bind_by_move_pattern_guards)]
#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use iro::ast;
use iro::lexer;
use iro::ast::Visitor;
use iro::types::visitor::TypeVisitor;

fn main() {
    let input = "x = 10
    x + 1
    x=\"y\"
    x + \"y\"";
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer).unwrap();
    println!("{:#?}", ast.exprs);
    TypeVisitor::new().visit_program(&ast).unwrap();
    println!("{:#?}", ast.exprs);
}