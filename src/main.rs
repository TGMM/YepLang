#![feature(lazy_cell)]

mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use compiler::helpers::Compiler;
use inkwell::{context::Context, passes::PassManager};
use parser::main_parser::parse;
use std::collections::HashMap;

fn main() {
    let input = r#"
    extern i32 printf(*u8, ...);
    let x: i32 = 10;
    {
        let y: i32 = 20;
        let z: boolean = x > y;

        printf("X is %d and y is %d\n", x, y);
        printf("Is x greater than y? %d\n", z);
    }

    function test_func(arg: i64) {
        printf("Test func: %d\n", arg);
    }

    test_func(10);
    test_func(20);
    test_func(30);

    printf("Out again\n");"#;
    let top_block = parse(input, "input.file").expect("Invalid code");

    let context = Context::create();
    let module = context.create_module("TODO_file_name");
    let builder = context.create_builder();

    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    let mut compiler = Compiler {
        builder: &builder,
        module: &module,
        context: &context,
        fpm: &fpm,
        curr_scope_vars: HashMap::new(),
        basic_block_stack: Vec::new(),
        scope_stack: Vec::new(),
    };

    compiler.codegen_top_block(top_block);
    Compiler::compile_to_x86(
        &compiler,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests",
        "test",
    );
}
