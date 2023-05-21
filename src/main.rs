#![feature(lazy_cell)]
#![feature(let_chains)]

mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use compiler::main_codegen::compile_yep;

fn main() {
    let input = r#"
    extern i32 printf(*i8, ...);

    let x: u8[5] = [1, 2, 3, 4, 5];
    for(let i: u32 = 0; i < 5; i += 1) {
        printf("%dth element is %d\n", i, x[i]);
    }
    "#;

    let project_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/compiled");
    let res = compile_yep(input, project_dir, "test");
    match res {
        Ok(_) => {}
        Err(err) => eprintln!("{}", err),
    }
}
