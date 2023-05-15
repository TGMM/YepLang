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
use std::collections::HashMap;

fn main() {
    let input = r#"
    extern i32 printf(*i8, ...);

    function iterative_fib(n: i32): i32
    {
        let i = 0, first = 0, second = 1, result: i32;

        while(i <= n) {
            if(i <= 1) {
                result = i;
            } else {
                result = first + second;
                first = second;
                second = result;
            }

            i = i + 1;
        }

        return result;
    }

    function rec_fib(n: i32): i32
    {
        if (n <= 1) {
            return n;
        }
        
        return rec_fib(n - 1) + rec_fib(n - 2);
    }

    let n = 14;
    printf("Iterative: Fib of %d is %d\n", n, iterative_fib(n));
    printf("Recursive: Fib of %d is %d\n", n, rec_fib(n));
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
    };

    compiler.codegen_top_block(top_block);
    Compiler::compile_to_x86(
        &compiler,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests",
        "test",
    );
}
