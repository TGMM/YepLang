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

    let x = 2147483647;
    printf("Max int is %d\n", x);
    printf("Overflow add is %d\n", x + 1);
    "#;

    compile_yep(
        input,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests/compiled",
        "test",
    );
}
