#![feature(lazy_cell)]
#![feature(let_chains)]

mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use compiler::helpers::Compiler;
use inkwell::{context::Context, passes::PassManager};
use parser::main_parser::parse;
use std::{cell::OnceCell, collections::HashMap};

fn main() {
    let input = r#"
    extern i32 printf(*i8, ...);

    let arr: i32[5] = [1, 2, 3, 4, 5];
    let one: u32 = 1, five: u32 = 5;
    for(let i: u32 = 0; i < five; i = i + one) {
        printf("Array element %d is %d\n", i, arr[i]);
    }
    "#;
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
        curr_func_ret_val: None,
        func_ret_val_stack: vec![],
        target_data: OnceCell::new(),
    };

    Compiler::compile_to_x86(
        &mut compiler,
        top_block,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests",
        "test",
    );
}
