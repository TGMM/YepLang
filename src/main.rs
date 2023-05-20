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

    let x = [1, 2, 3];
    printf("Num: %d\n", x[0]);
    "#;

    let res = compile_yep(
        input,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests/compiled",
        "test",
    );
    match res {
        Ok(_) => {}
        Err(err) => eprintln!("{}", err),
    }
}
