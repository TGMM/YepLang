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

    let x = 1;
    if(x > 3) {
        printf("> 3\n");
    } else if(x > 2) {
        printf("> 2\n");
    } else if(x > 1) {
        printf("> 1\n");
    } else {
        printf("None\n");
    }

    printf("End\n");
    "#;

    compile_yep(
        input,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests/compiled",
        "test",
    );
}
