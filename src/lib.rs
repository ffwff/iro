#![feature(vec_remove_item)]
#![feature(ptr_offset_from)]

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate downcast_rs;
extern crate cranelift;
extern crate cranelift_module;
extern crate cranelift_frontend;
extern crate cranelift_codegen;
extern crate cranelift_simplejit;

#[macro_use]
pub mod utils;
pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod runtime;
pub mod ssa;
pub mod codegen;
