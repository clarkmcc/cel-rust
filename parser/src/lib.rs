#![feature(error_reporter)]

extern crate core;

use lalrpop_util::lalrpop_mod;

pub mod ast;
mod chumsky;

pub use ast::*;

lalrpop_mod!(#[allow(clippy::all)] pub parser, "/cel.rs");
