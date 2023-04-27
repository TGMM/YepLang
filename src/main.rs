mod ast;
mod ast_display;
mod compiler;
mod lexer;
mod parser;

use compiler::codegen::Compiler;
use inkwell::{context::Context, passes::PassManager};
use lexer::Token;
use logos::Logos;
use parser::{
    main_parser::top_block_parser,
    token::{TokenSpan, Tokens},
};

fn main() {
    let input = r#"let x: i32 = 10;"#;
    let token_vec = Token::lexer(input)
        .spanned()
        .map(|(token, span)| TokenSpan { span, token })
        .collect::<Vec<_>>();
    let tokens = Tokens::new(&token_vec);

    let (remaining, top_block) = top_block_parser(tokens).unwrap();
    assert!(remaining.tok_span.is_empty());

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

    let compiler = Compiler {
        builder: &builder,
        module: &module,
        context: &context,
        fpm: &fpm,
    };
    compiler.codegen_top_block(top_block);
    Compiler::compile_to_x86(
        &compiler,
        "C:/Users/TGMM/Documents/Tareas/Compiladores/yep_lang/tests",
        "test",
    );
}
