use super::{
    expr_parser::{expr_parser, EXPR_PARSER},
    main_parser::{
        block_parser, destructure_parser, stmt_end_parser, ParserError, ParserInput,
        DESTRUCTURE_PARSER,
    },
    primitive_parser::{id_parser, type_specifier_parser},
};
use crate::{
    ast::{ClassBlock, ClassDecl, ClassStmt, FnDef, LlvmFn, MethodDecl, PropertyDecl},
    lexer::Token,
};
use crate::{
    ast::{NativeFn, Return},
    parser::main_parser::BLOCK_PARSER,
};
use chumsky::{primitive::just, select, IterParser, Parser};

pub fn method_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, MethodDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let destructure = DESTRUCTURE_PARSER.read().unwrap().clone();
    let block = BLOCK_PARSER.read().unwrap().clone();

    // Main definition
    let args = destructure
        .clone()
        .then(type_specifier_parser())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>();
    let method_decl = id_parser()
        .then_ignore(just(Token::LParen))
        .then(args)
        .then_ignore(just(Token::RParen))
        .then(type_specifier_parser().or_not())
        .then(block.clone())
        .map(|(((method_id, args), ret_type), block)| MethodDecl {
            method_id,
            args,
            ret_type,
            block,
        });

    // Definitions
    if !destructure.is_defined() {
        destructure_parser();
    }
    if !block.is_defined() {
        block_parser();
    }

    method_decl
}

pub fn llvm_fn_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, LlvmFn<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let destructure = DESTRUCTURE_PARSER.read().unwrap().clone();

    // Main definition
    let llvm_decorator =
        just(Token::At).then(select! { Token::Id(id) => id }.filter(|id| *id == "llvm"));

    let args = destructure
        .clone()
        .then(type_specifier_parser())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>();
    let llvm_fn = llvm_decorator
        .ignore_then(just(Token::Function))
        .ignore_then(id_parser())
        .then_ignore(just(Token::LParen))
        .then(args)
        .then_ignore(just(Token::RParen))
        .then(type_specifier_parser().or_not())
        .then(
            select! { Token::LlvmIr(ir) => ir }
                .delimited_by(just(Token::LBracket), just(Token::RBracket)),
        )
        .map(|(((fn_id, args), ret_type), ir)| LlvmFn {
            fn_id,
            args,
            ret_type,
            ir: ir.to_string(),
        });

    // Definitions
    if !destructure.is_defined() {
        destructure_parser();
    }

    llvm_fn
}

pub fn native_fn_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, NativeFn<'i>, ParserError<'i, Token<'i>>> + Clone {
    just(Token::Function)
        .ignore_then(method_decl_parser())
        .map(|md| md.into())
}

pub fn fn_def_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, FnDef<'i>, ParserError<'i, Token<'i>>> + Clone {
    let native = native_fn_parser().map(FnDef::Native);
    let llvm = llvm_fn_parser().map(FnDef::InlineLlvm);

    llvm.or(native)
}

pub fn class_prop_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, PropertyDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let class_prop = id_parser()
        .then(type_specifier_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(expr.clone().or_not())
        .map(|((id, vtype), assigned_expr)| PropertyDecl {
            id,
            vtype,
            assigned_expr,
        });

    // Definitions
    if !expr.is_defined() {
        expr_parser();
    }

    class_prop
}

pub fn class_stmt_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ClassStmt<'i>, ParserError<'i, Token<'i>>> + Clone {
    let method = method_decl_parser().map(ClassStmt::Method);
    let _accessor = {}; // TODO?: Maybe won't be doing this
    let property = class_prop_decl_parser()
        .then_ignore(stmt_end_parser())
        .map(ClassStmt::Property);

    method.or(property)
}

pub fn class_block_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ClassBlock<'i>, ParserError<'i, Token<'i>>> + Clone {
    class_stmt_parser()
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(|class_stmts| ClassBlock { class_stmts })
}

pub fn class_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ClassDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    let class_decl = just(Token::Class)
        .ignore_then(id_parser())
        .then(just(Token::Extends).ignore_then(id_parser()).or_not())
        .then(class_block_parser())
        .map(|((class_id, extended_class_id), block)| ClassDecl {
            class_id,
            extended_class_id,
            block,
        });

    class_decl
}

pub fn return_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Return<'i>, ParserError<'i, Token<'i>>> + Clone {
    just(Token::Return)
        .ignore_then(expr_parser().or_not())
        .map(Return)
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;

    use chumsky::Parser;

    use crate::{
        ast::{
            Block, ClassBlock, ClassDecl, ClassStmt, Destructure, Expr, FnDef, LlvmFn, MethodDecl,
            NativeFn, NumericLiteral, PrimitiveVal, PropertyDecl, ValueVarType, VarType,
        },
        lexer::Token,
        parser::{
            class_parser::{
                class_block_parser, class_decl_parser, fn_def_parser, method_decl_parser,
            },
            helpers::test::stream_token_vec,
        },
    };

    #[test]
    fn method_decl_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("myMethod".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = method_decl_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let method_decl = res.unwrap();
        assert_eq!(
            method_decl,
            MethodDecl {
                method_id: "myMethod".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_def_test() {
        let tokens = stream_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = fn_def_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_def = res.unwrap();
        assert_eq!(
            fn_def,
            FnDef::Native(NativeFn {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0
                }),
                block: Block { stmts: vec![] }
            })
        )
    }

    #[test]
    fn fn_def_void_test() {
        let tokens = stream_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = fn_def_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_def = res.unwrap();
        assert_eq!(
            fn_def,
            FnDef::Native(NativeFn {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: None,
                block: Block { stmts: vec![] }
            })
        )
    }

    #[test]
    fn fn_def_args_test() {
        let tokens = stream_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::Id("x".into()),
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = fn_def_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_def = res.unwrap();
        assert_eq!(
            fn_def,
            FnDef::Native(NativeFn {
                fn_id: "myFunction".into(),
                args: vec![(
                    Destructure::Id("x".into()),
                    ValueVarType {
                        vtype: VarType::I32,
                        array_dimensions: VecDeque::new(),
                        pointer_nesting_level: 0
                    }
                )],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0
                }),
                block: Block { stmts: vec![] }
            })
        )
    }

    #[test]
    fn class_block_test() {
        let tokens = stream_token_vec(vec![
            Token::LBracket,
            Token::Id("myClassMethod".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
            Token::Id("myClassProp".into()),
            Token::Colon,
            Token::VarType(VarType::F32),
            Token::AssignmentEq,
            Token::IntVal("10"),
            Token::StmtEnd,
            Token::RBracket,
        ]);

        let res = class_block_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let class_block = res.unwrap();
        assert_eq!(
            class_block,
            ClassBlock {
                class_stmts: vec![
                    ClassStmt::Method(MethodDecl {
                        method_id: "myClassMethod".into(),
                        args: vec![],
                        ret_type: Some(ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }),
                        block: Block { stmts: vec![] }
                    }),
                    ClassStmt::Property(PropertyDecl {
                        id: "myClassProp".into(),
                        vtype: Some(ValueVarType {
                            vtype: VarType::F32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }),
                        assigned_expr: Some(Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))),
                    })
                ]
            }
        )
    }

    #[test]
    fn class_decl_test() {
        let tokens = stream_token_vec(vec![
            Token::Class,
            Token::Id("MyClass"),
            Token::LBracket,
            Token::Id("myClassMethod".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
            Token::Id("myClassProp".into()),
            Token::Colon,
            Token::VarType(VarType::F32),
            Token::AssignmentEq,
            Token::IntVal("10"),
            Token::StmtEnd,
            Token::RBracket,
        ]);

        let res = class_decl_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let class_decl = res.unwrap();
        assert_eq!(
            class_decl,
            ClassDecl {
                class_id: "MyClass".into(),
                extended_class_id: None,
                block: ClassBlock {
                    class_stmts: vec![
                        ClassStmt::Method(MethodDecl {
                            method_id: "myClassMethod".into(),
                            args: vec![],
                            ret_type: Some(ValueVarType {
                                vtype: VarType::I32,
                                array_dimensions: VecDeque::new(),
                                pointer_nesting_level: 0
                            }),
                            block: Block { stmts: vec![] }
                        }),
                        ClassStmt::Property(PropertyDecl {
                            id: "myClassProp".into(),
                            vtype: Some(ValueVarType {
                                vtype: VarType::F32,
                                array_dimensions: VecDeque::new(),
                                pointer_nesting_level: 0
                            }),
                            assigned_expr: Some(Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Int("10")
                            ))),
                        })
                    ]
                }
            }
        )
    }

    #[test]
    fn llvm_ir_test() {
        let tokens = stream_token_vec(vec![
            Token::At,
            Token::Id("llvm".into()),
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::Id("a".into()),
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::Comma,
            Token::Id("b".into()),
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::LlvmIr("%res = add i32 %a, %b\nret i32 %res"),
            Token::RBracket,
        ]);

        let res = fn_def_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_def = res.unwrap();
        assert_eq!(
            fn_def,
            FnDef::InlineLlvm(LlvmFn {
                fn_id: "myFunction".into(),
                args: vec![
                    (
                        Destructure::Id("a".into()),
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                    ),
                    (
                        Destructure::Id("b".into()),
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                    )
                ],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0
                }),
                ir: "%res = add i32 %a, %b\nret i32 %res".to_string()
            })
        )
    }
}
