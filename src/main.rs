#![feature(lazy_cell)]

mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use crate::parser::main_parser::top_block_parser;
use chumsky::prelude::SimpleSpan;
use chumsky::Parser;
use chumsky::{input::Stream, prelude::Input};
use compiler::codegen::Compiler;
use inkwell::{context::Context, passes::PassManager};
use lexer::Token;
use logos::Logos;
use std::collections::HashMap;

fn main() {
    let input = r#"
    extern i32 printf(*u8, ...);
    let x: i64 = 10;
    {
        let y: i64 = 20;
        let x: i32 = 30;
        x = 50;

        printf("X is %d and y is %d\n", x, y);
        printf("Some text\n");
    }

    function test_func(arg: i64) {
        printf("Test func: %d\n", arg);
    }

    test_func(10);
    test_func(20);
    test_func(30);

    printf("Out again\n");"#;
    let tokens = Token::lexer(input)
        .spanned()
        .map(|(token, span)| (token.unwrap(), SimpleSpan::from(span)))
        .collect::<Vec<_>>();
    let length = tokens.len();
    let tokens_stream = Stream::from_iter(tokens).spanned((length..length).into());

    let top_block = top_block_parser()
        .parse(tokens_stream)
        .into_result()
        .unwrap();

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
