#![feature(lazy_cell)]
#![feature(let_chains)]

mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use compiler::main_codegen::compile_yep;
use std::{fs, path::Path};

fn main() {
    let tests_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests");
    let tests_dir_path = Path::new(tests_dir);

    let input = fs::read_to_string(tests_dir_path.join("test.yep")).unwrap();
    let input_ref: &'static str = Box::leak(input.into_boxed_str());

    let compiled_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/compiled");
    let res = compile_yep(input_ref, compiled_dir, "test");
    match res {
        Ok(_) => {}
        Err(err) => eprintln!("{}", err),
    }
}
