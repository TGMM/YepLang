use std::fs;
use std::{path::Path, process::Command};
use yep_lang::compiler::helpers::YepTarget;
use yep_lang::compiler::main_codegen::compile_yep;

const TARGET: &str = env!("TARGET");

macro_rules! compiler_test {
    ($test_name:ident, input: $input:literal, output: $expected_out:literal) => {
        #[test]
        fn $test_name() {
            let input = $input;

            let project_dir = env!("CARGO_MANIFEST_DIR");
            let compiled_tests_dir = Path::new(project_dir).join("tests").join("compiled");

            // Create directory if it doesn't exist
            if !compiled_tests_dir.is_dir() {
                fs::create_dir(compiled_tests_dir.clone()).unwrap();
            }

            let test_name = stringify!($test_name);
            let target = YepTarget {
                target_triple: TARGET.to_string(),
                /// If you can compile this, then you definitely don't
                /// have a nostd environment
                nostd: false,
            };
            compile_yep(
                input,
                compiled_tests_dir.to_str().unwrap(),
                test_name,
                target,
            )
            .unwrap();
            let out_path = compiled_tests_dir.join(format!("{}.ll", test_name));

            let output = Command::new("lli")
                .arg(out_path)
                .output()
                .expect("Could not run lli");

            let str_out = String::from_utf8_lossy(&output.stdout).replace("\r\n", "\n");
            let expected_out = $expected_out;

            assert_eq!(str_out, expected_out);
        }
    };
}

compiler_test!(
    matrix,
    input: r#"
    extern i32 printf(*i8, ...);

    let arr: i32[3][1] = [[1], [2], [3]];
    for(let i: u32 = 0; i < 3; i+= 1) {
        for(let j: u32 = 0; j < 1; j += 1) {
            printf("Array element %d, %d is %d\n" as *i8, i, j, arr[i][j]);
        }   
    }
    "#,
    output: "Array element 0, 0 is 1\nArray element 1, 0 is 2\nArray element 2, 0 is 3\n"
);

compiler_test!(
    array,
    input: r#"
    extern i32 printf(*i8, ...);

    let arr: i32[5] = [1, 2, 3, 4, 5];
    for(let i: u32 = 0; i < 5; i += 1) {
        printf("Array element %d is %d\n" as *i8, i, arr[i]);
    }
    "#,
    output: "Array element 0 is 1\nArray element 1 is 2\nArray element 2 is 3\nArray element 3 is 4\nArray element 4 is 5\n"
);

compiler_test!(
    if_true,
    input: r#"
    extern i32 printf(*i8, ...);

    if(true) {
        printf("Condition is true\n" as *i8);
    }
    "#,
    output: "Condition is true\n"
);

compiler_test!(
    if_false,
    input: r#"
    extern i32 printf(*i8, ...);

    if(false) {
        
    } else {
        printf("Condition is false\n" as *i8);
    }
    "#,
    output: "Condition is false\n"
);

compiler_test!(
    else_if,
    input: r#"
    extern i32 printf(*i8, ...);

    if(false) {

    } else if(true) {
        printf("Second condition is true\n" as *i8);
    } else {

    }
    "#,
    output: "Second condition is true\n"
);

compiler_test!(
    else_if_2,
    input: r#"
    extern i32 printf(*i8, ...);

    if(false) {

    } else if(false) {
        
    } else if(true) {
        printf("Third condition is true\n" as *i8);
    } else {

    }
    "#,
    output: "Third condition is true\n"
);

compiler_test!(
    int_add,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = 10 + 10;
    printf("x is %d\n" as *i8, x);
    "#,
    output: "x is 20\n"
);

compiler_test!(
    int_sub,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = 0 - 10;
    printf("x is %d\n" as *i8, x);
    "#,
    output: "x is -10\n"
);

compiler_test!(
    int_mul,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = -10 * 10;
    printf("x is %d\n" as *i8, x);
    "#,
    output: "x is -100\n"
);

compiler_test!(
    int_div,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = 10 / 3;
    printf("x is %d\n" as *i8, x);
    "#,
    output: "x is 3\n"
);

compiler_test!(
    float_add,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = 1.1 + 1.1;
    printf("x is %.1f\n" as *i8, x);
    "#,
    output: "x is 2.2\n"
);

compiler_test!(
    float_sub,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = 1.1 - 1.1;
    printf("x is %.0f\n" as *i8, x);
    "#,
    output: "x is 0\n"
);

compiler_test!(
    float_mul,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = 1.5 * 1.5;
    printf("x is %.2f\n" as *i8, x);
    "#,
    output: "x is 2.25\n"
);

compiler_test!(
    float_div,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = -1.5 / 2.0;
    printf("x is %.2f\n" as *i8, x);
    "#,
    output: "x is -0.75\n"
);

compiler_test!(
    while_true,
    input: r#"
    extern i32 printf(*i8, ...);

    let x = 0;
    while(x <= 5) {
        printf("x is %d\n" as *i8, x);
        x += 1; 
    }
    "#,
    output: "x is 0\nx is 1\nx is 2\nx is 3\nx is 4\nx is 5\n"
);

compiler_test!(
    while_false,
    input: r#"
    extern i32 printf(*i8, ...);

    let x = 5;
    while(x < 5) {
        printf("x is %d\n" as *i8, x);
        x += 1; 
    }
    "#,
    output: ""
);

compiler_test!(
    do_while_true,
    input: r#"
    extern i32 printf(*i8, ...);

    let x = 0;
    do {
        printf("x is %d\n" as *i8, x);
        x += 1; 
    }
    while(x <= 5);
    "#,
    output: "x is 0\nx is 1\nx is 2\nx is 3\nx is 4\nx is 5\n"
);

compiler_test!(
    do_while_false,
    input: r#"
    extern i32 printf(*i8, ...);

    let x = 5;
    do {
        printf("x is %d\n" as *i8, x);
        x += 1; 
    }
    while(x < 5);
    "#,
    output: "x is 5\n"
);

compiler_test!(
    for_true,
    input: r#"
    extern i32 printf(*i8, ...);

    for(let x: u32 = 0; x < 5; x += 1) {
        printf("x is %d\n" as *i8, x);
    }
    "#,
    output: "x is 0\nx is 1\nx is 2\nx is 3\nx is 4\n"
);

compiler_test!(
    for_false,
    input: r#"
    extern i32 printf(*i8, ...);

    for(let x: u32 = 5; x < 5; x += 1) {
        printf("x is %d\n" as *i8, x);
    }
    "#,
    output: ""
);

compiler_test!(
    for_array,
    input: r#"
    extern i32 printf(*i8, ...);

    let arr = [10, 20, 30];
    for(let i: u32 = 0; i < 3; i += 1) {
        printf("i is %d\n" as *i8, arr[i]);
    }
    "#,
    output: "i is 10\ni is 20\ni is 30\n"
);

compiler_test!(
    for_array_no_postfix,
    input: r#"
    extern i32 printf(*i8, ...);

    let arr = [10, 20, 30];
    for(let i: u32 = 0; i < 3;) {
        printf("i is %d\n" as *i8, arr[i]);
        i += 1;
    }
    "#,
    output: "i is 10\ni is 20\ni is 30\n"
);

compiler_test!(
    for_array_no_decl,
    input: r#"
    extern i32 printf(*i8, ...);

    let arr = [10, 20, 30];
    let i: u32 = 0;
    for(; i < 3; i += 1) {
        printf("i is %d\n" as *i8, arr[i]);
    }
    "#,
    output: "i is 10\ni is 20\ni is 30\n"
);

compiler_test!(
    for_array_no_decl_no_postfix,
    input: r#"
    extern i32 printf(*i8, ...);

    let arr = [10, 20, 30];
    let i: u32 = 0;
    for(; i < 3;) {
        printf("i is %d\n" as *i8, arr[i]);
        i += 1;
    }
    "#,
    output: "i is 10\ni is 20\ni is 30\n"
);

compiler_test!(
    for_if_else,
    input: r#"
    extern i32 printf(*i8, ...);

    let i: u32 = 1;
    for(; i < 3;) {
        if(i == 1){
            printf("i is 1\n" as *i8);
            i += 1;
        } else {
            printf("i is 2\n" as *i8);
            i += 1;
        }
    }
    "#,
    output: "i is 1\ni is 2\n"
);

compiler_test!(
    fib,
    input: r#"
    extern i32 printf(*i8, ...);

    function fib(val: i32): i32 {
        if(val < 1) {
            return 0;
        }
        if(val < 2) {
            return 1;
        }
        return fib(val - 2) + fib(val - 1);
    }

    for(let i = 0; i < 10; i += 1){
        let val = fib(i);
        printf("fib is %d\n" as *i8, val);
    }
    "#,
    output: "fib is 0\nfib is 1\nfib is 1\nfib is 2\nfib is 3\nfib is 5\nfib is 8\nfib is 13\nfib is 21\nfib is 34\n"
);

compiler_test!(
    arithmetic_expr_mul,
    input: r#"
    extern i32 printf(*i8, ...);
    
    let x = 5 + 3 * 2;
    printf("x is %d\n" as *i8, x);

    "#,
    output: "x is 11\n"
);

compiler_test!(
    arithmetic_expr_div,
    input: r#"
    extern i32 printf(*i8, ...);
    
    let x = 5 + 3 / 3;
    printf("x is %d\n" as *i8, x);

    "#,
    output: "x is 6\n"
);

compiler_test!(
    arithmetic_expr_parenthesis,
    input: r#"
    extern i32 printf(*i8, ...);
    
    let x = (5 + 3) * 2;
    printf("x is %d\n" as *i8, x);

    "#,
    output: "x is 16\n"
);

compiler_test!(
    hard_arithmetic_expr_parenthesis,
    input: r#"
    extern i32 printf(*i8, ...);
    
    let x = -10 / (20 / 4 * 5 / 5) * 8 - 2;
    printf("x is %d\n" as *i8, x);

    "#,
    output: "x is -18\n"
);
