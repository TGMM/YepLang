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

    let arr: i32[3][1] = [[1], [2], [3]];
    let one: u32 = 1, three: u32 = 3;
    for(let i: u32 = 0; i < three; i = i + one) {
        for(let j: u32 = 0; j < one; j = j + one) {
            printf("Array element %d, %d is %d\n", i, j, arr[i][j]);
        }   
    }
    "#;

    compile_yep(
        input,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests/compiled",
        "test",
    );
}
