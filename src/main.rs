#![feature(lazy_cell)]
#![feature(let_chains)]

mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use compiler::{helpers::YepTarget, main_codegen::compile_yep};
use std::{fs, path::Path};

const TARGET: &str = env!("TARGET");

fn main() {
    let tests_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests");
    let tests_dir_path = Path::new(tests_dir);

    let input = fs::read_to_string(tests_dir_path.join("test.yep")).unwrap();
    let input_ref: &'static str = Box::leak(input.into_boxed_str());

    let compiled_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/compiled");
    let target = YepTarget {
        target_triple: TARGET.to_string(),
        nostd: false,
    };
    let res = compile_yep(input_ref, compiled_dir, "test", target);
    match res {
        Ok(_) => {}
        Err(err) => eprintln!("{}", err),
    }
}
