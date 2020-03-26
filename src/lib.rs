#![feature(option_flattening)]
#![feature(bind_by_move_pattern_guards)]
#![feature(vec_remove_item)]
#![feature(asm)]

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate downcast_rs;
extern crate libc;

#[macro_use]
pub mod utils;
pub mod arch;
pub mod ast;
pub mod lexer;
pub mod runtime;
pub mod ssa;
