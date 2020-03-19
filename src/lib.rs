#![feature(is_sorted)]
#![feature(weak_ptr_eq)]
#![feature(bind_by_move_pattern_guards)]

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub parser);

#[macro_use] extern crate maplit;
#[macro_use] extern crate downcast_rs;

pub mod utils;
pub mod lexer;
pub mod ast;
pub mod ssa;
pub mod opt;