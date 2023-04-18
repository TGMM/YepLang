mod ast;
mod ast_display;
mod lexer;
mod parser;

use parser::main_parser::parse;

fn main() {
    let input = r#"
    const x = -1e+10;
    let y = 9223372036854775808;
    var z = 10;

    function test(x: i32) {

    }

    if(z > y) {
        print("Test");
    }

    for(;;) {

    }

    while(true) {

    }

    do {

    } while(false);

    class MyClass extends MyOtherClass {
        test_func() {

        }
    }

    let c = 10;
    c.my_prop = 10;
    let [x, y] = x;
    let {"lol xd": x, y: alias} = y;
    "#;
    parse(input);
}
