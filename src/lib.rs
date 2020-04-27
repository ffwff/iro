#![feature(vec_remove_item)]
#![feature(ptr_offset_from)]
#![feature(trait_alias)]
#![feature(move_ref_pattern)]
#![feature(cell_update)]

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate downcast_rs;
extern crate cranelift;
extern crate cranelift_codegen;
extern crate cranelift_frontend;
extern crate cranelift_module;
extern crate cranelift_native;
extern crate cranelift_simplejit;
extern crate target_lexicon;
#[macro_use]
extern crate derivative;
#[macro_use]
extern crate codespan_reporting;

#[macro_use]
pub mod utils;
pub mod ast;
pub mod codegen;
pub mod compiler;
pub mod lexer;
pub mod runtime;
pub mod ssa;
