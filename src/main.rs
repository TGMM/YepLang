#![feature(lazy_cell)]
#![feature(let_chains)]

use yep_lang::compiler;

use compiler::{helpers::YepTarget, main_codegen::compile_yep};
use std::{fs, path::Path};

const TARGET: &str = env!("TARGET");

fn main() {
    let project_dir = env!("CARGO_MANIFEST_DIR");
    let tests_dir_path = Path::new(project_dir).join("tests");

    let input = fs::read_to_string(tests_dir_path.join("test.yep")).unwrap();
    let input_ref: &'static str = Box::leak(input.into_boxed_str());

    let compiled_dir = tests_dir_path.join("compiled");
    let target = YepTarget {
        target_triple: TARGET.to_string(),
        nostd: false,
    };
    let res = compile_yep(input_ref, compiled_dir.to_str().unwrap(), "test", target);
    match res {
        Ok(_) => {}
        Err(err) => eprintln!("{}", err),
    }
}
