mod ast;
mod ast_display;
mod lexer;
mod parser;

use lexer::Token;
use logos::Logos;

fn main() {
    let input = r#"
    x = 5 + - 5;
    "#;
    let token_vec = Token::lexer(input).spanned().collect::<Vec<_>>();
}
