mod ast;
mod ast_display;
mod lexer;
mod parser;

use parser::main_parser::parse;

fn main() {
    let input = r#"const x = -1e+10;
    let y = 9223372036854775808;
    var "test xd" = 10;"#;
    parse(input);
}
