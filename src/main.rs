mod ast;
mod ast_display;
mod lexer;
mod parser;

use lexer::Token;
use logos::Logos;
use parser::{
    main_parser::top_block_parser,
    token::{TokenSpan, Tokens},
};

fn main() {
    let input = r#"
    x = 5 + - 5;
    "#;
    let token_vec = Token::lexer(input)
        .spanned()
        .map(|(token, span)| TokenSpan { span, token })
        .collect::<Vec<_>>();
    let tokens = Tokens::new(&token_vec);

    let result = top_block_parser(tokens);
    match result {
        Ok((_remaining, block)) => {
            dbg!(block);
        }
        Err(err) => println!("{err}"),
    }
}
