#![feature(bind_by_move_pattern_guards)]
#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub parser);
use iro::ast;
use iro::lexer;
use iro::ast::Visitor;
use iro::types::visitor::TypeVisitor;

fn main() {
    let input = "
    if 10 == 10
        a = 1
        a + 2
    else
        if 10 == 10
            a = \"a\"
            a + \"a\"
        end
        b = 1
    end
    ";
    let tokenizer = lexer::Lexer::new(input);
    let ast = parser::TopParser::new().parse(tokenizer).unwrap();
    println!("{:#?}", ast.exprs);
    if let Err(err) = TypeVisitor::new().visit_program(&ast) {
        println!("error: {:#?}", err);
    }
    println!("{:#?}", ast.exprs);
}