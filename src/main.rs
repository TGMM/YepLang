use parser::parse;
mod ast;
mod ast_display;
mod lexer;
mod parser;

fn main() {
    let input = r#"const x = -1e+10; 
    let y = 9223372036854775808;
    var z = 10.0"#;
    parse(input);
}
