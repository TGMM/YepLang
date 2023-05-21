use std::{path::Path, process::Command};
use yep_lang::compiler::main_codegen::compile_yep;

macro_rules! compiler_test {
    ($test_name:ident, input: $input:literal, output: $expected_out:literal) => {
        #[test]
        fn $test_name() {
            let input = $input;

            let project_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/compiled");
            let test_name = stringify!($test_name);
            compile_yep(input, project_dir, test_name).unwrap();
            let out_path = Path::new(project_dir).join(format!("{}.ll", test_name));

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
            printf("Array element %d, %d is %d\n", i, j, arr[i][j]);
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
        printf("Array element %d is %d\n", i, arr[i]);
    }
    "#,
    output: "Array element 0 is 1\nArray element 1 is 2\nArray element 2 is 3\nArray element 3 is 4\nArray element 4 is 5\n"
);

compiler_test!(
    if_true,
    input: r#"
    extern i32 printf(*i8, ...);

    if(true) {
        printf("Condition is true\n");
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
        printf("Condition is false\n");
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
        printf("Second condition is true\n");
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
        printf("Third condition is true\n");
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
    printf("Result is %d\n", x);
    "#,
    output: "Result is 20\n"
);

compiler_test!(
    int_sub,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = 0 - 10;
    printf("Result is %d\n", x);
    "#,
    output: "Result is -10\n"
);

compiler_test!(
    int_mul,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = -10 * 10;
    printf("Result is %d\n", x);
    "#,
    output: "Result is -100\n"
);

compiler_test!(
    int_div,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: i32 = 10 / 3;
    printf("Result is %d\n", x);
    "#,
    output: "Result is 3\n"
);

compiler_test!(
    float_add,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = 1.1 + 1.1;
    printf("Result is %.1f\n", x);
    "#,
    output: "Result is 2.2\n"
);

compiler_test!(
    float_sub,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = 1.1 - 1.1;
    printf("Result is %.0f\n", x);
    "#,
    output: "Result is 0\n"
);

compiler_test!(
    float_mul,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = 1.5 * 1.5;
    printf("Result is %.2f\n", x);
    "#,
    output: "Result is 2.25\n"
);

compiler_test!(
    float_div,
    input: r#"
    extern i32 printf(*i8, ...);

    let x: f64 = -1.5 / 2.0;
    printf("Result is %.2f\n", x);
    "#,
    output: "Result is -0.75\n"
);

compiler_test!(
    while_true,
    input: r#"
    extern i32 printf(*i8, ...);

    let x = 0;
    while(x <= 5) {
        printf("Result is %d\n", x);
        x = x + 1; 
    }
    "#,
    output: "Result is 0\nResult is 1\nResult is 2\nResult is 3\nResult is 4\nResult is 5\n"
);

compiler_test!(
    while_false,
    input: r#"
    extern i32 printf(*i8, ...);

    let x = 5;
    while(x < 5) {
        printf("Result is %d\n", x);
        x = x + 1; 
    }
    "#,
    output: ""
);
