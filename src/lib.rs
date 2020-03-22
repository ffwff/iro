#![feature(option_flattening)]
#![feature(bind_by_move_pattern_guards)]
#![feature(vec_remove_item)]

#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub parser);

#[macro_use] extern crate maplit;
#[macro_use] extern crate downcast_rs;
extern crate libc;

pub mod utils;
pub mod lexer;
pub mod ast;
pub mod ssa;
pub mod arch;