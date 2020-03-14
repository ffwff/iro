#![feature(weak_ptr_eq)]
#![feature(bind_by_move_pattern_guards)]

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub parser);

#[macro_use] extern crate maplit;

pub mod utils;
pub mod types;
pub mod lexer;
pub mod ast;
pub mod env;
pub mod ssa;