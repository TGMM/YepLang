use super::{
    expr_parser::{expr_parser, EXPR_PARSER},
    keyword_parser::tag,
    main_parser::{
        block_parser, destructure_parser, stmt_end_parser, ParserError, ParserInput,
        DESTRUCTURE_PARSER,
    },
    primitive_parser::{id_parser, type_specifier_parser},
};
use crate::{
    ast::{ClassBlock, ClassDecl, ClassStmt, FnDef, FnScope, FnSignature, FnType, PropertyDecl},
    lexer::Token,
};
use crate::{
    ast::{InlineLlvmIr, Return},
    parser::main_parser::BLOCK_PARSER,
};
use chumsky::{primitive::just, select, IterParser, Parser};

pub fn fn_signature_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, FnSignature<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let destructure = DESTRUCTURE_PARSER.read().unwrap().clone();

    // Main definition
    let args = destructure
        .clone()
        .then(type_specifier_parser())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>();
    let fn_signature = id_parser()
        .then_ignore(just(Token::LParen))
        .then(args)
        .then_ignore(just(Token::RParen))
        .then(type_specifier_parser().or_not())
        .map(|((fn_id, args), ret_type)| FnSignature {
            fn_id,
            args,
            ret_type,
        });

    // Definitions
    if !destructure.is_defined() {
        destructure_parser();
    }

    fn_signature
}

pub fn fn_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, FnDef<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let block = BLOCK_PARSER.read().unwrap().clone();

    // Main definition
    let fn_def = tag(Token::Function)
        .or_not()
        .then(fn_signature_parser())
        .then(block.clone())
        .map(|((fn_kw, fn_sign), block)| FnDef {
            fn_kw,
            fn_signature: fn_sign,
            fn_type: FnType::Native(block),
            // If function keyword is not present,
            // then this is a method
            fn_scope: if fn_kw.is_some() {
                FnScope::Function
            } else {
                FnScope::Method
            },
        });

    // Definitions
    if !block.is_defined() {
        block_parser();
    }

    fn_def
}

pub fn llvm_fn_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, FnDef<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let destructure = DESTRUCTURE_PARSER.read().unwrap().clone();

    // Main definition
    let llvm_decorator = just(Token::At)
        .then(select! { Token::Id(id) => id }.filter(|id| id.id_str.as_str() == "llvm"));

    let llvm_fn = llvm_decorator
        .ignore_then(tag(Token::Function).or_not())
        .then(fn_signature_parser())
        .then(
            tag(Token::LBracket)
                .then(select! { Token::LlvmIr(ir) => ir })
                .then(tag(Token::RBracket))
                .map(|((lbracket, ir), rbracket)| InlineLlvmIr {
                    lbracket,
                    ir: ir.to_string(),
                    rbracket,
                }),
        )
        .map(|((fn_kw, fn_signature), ir)| FnDef {
            fn_kw,
            fn_signature,
            fn_type: FnType::InlineLlvmIr(ir),
            // If function keyword is not present,
            // then this is a method
            fn_scope: if fn_kw.is_some() {
                FnScope::Function
            } else {
                FnScope::Method
            },
        });

    // Definitions
    if !destructure.is_defined() {
        destructure_parser();
    }

    llvm_fn
}

pub fn fn_def_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, FnDef<'i>, ParserError<'i, Token<'i>>> + Clone {
    let native = fn_parser();
    let llvm = llvm_fn_parser();

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
    let method = fn_def_parser().map(ClassStmt::Method);
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
    tag(Token::Return)
        .then(expr_parser().or_not())
        .map(|(ret_kw, ret_val)| Return { ret_kw, ret_val })
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;

    use chumsky::{span::SimpleSpan, Parser};

    use crate::{
        ast::{
            Block, ClassBlock, ClassDecl, ClassStmt, Destructure, Expr, FnDef, FnScope,
            FnSignature, FnType, InlineLlvmIr, NumericLiteral, PrimitiveVal, PropertyDecl,
            ValueVarType, VarType,
        },
        lexer::Token,
        parser::{
            class_parser::{class_block_parser, class_decl_parser, fn_def_parser},
            helpers::test::{stream_token_vec, IntoSpanned},
        },
    };

    #[test]
    fn method_def_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("myMethod".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = fn_def_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let method_def = res.unwrap();
        assert_eq!(
            method_def,
            FnDef {
                fn_signature: FnSignature {
                    fn_id: "myMethod".into(),
                    args: vec![],
                    ret_type: Some(
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                        .into_spanned()
                    )
                },
                fn_scope: FnScope::Method,
                fn_type: FnType::Native(Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                }),
                fn_kw: None
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
            FnDef {
                fn_signature: FnSignature {
                    fn_id: "myFunction".into(),
                    args: vec![],
                    ret_type: Some(
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                        .into_spanned()
                    )
                },
                fn_scope: FnScope::Function,
                fn_type: FnType::Native(Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                }),
                fn_kw: Some(SimpleSpan::new(0, 0))
            }
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
            FnDef {
                fn_signature: FnSignature {
                    fn_id: "myFunction".into(),
                    args: vec![],
                    ret_type: None
                },
                fn_scope: FnScope::Function,
                fn_type: FnType::Native(Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                }),
                fn_kw: Some(SimpleSpan::new(0, 0))
            }
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
            FnDef {
                fn_signature: FnSignature {
                    fn_id: "myFunction".into(),
                    args: vec![(
                        Destructure::Id("x".into()),
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                        .into_spanned()
                    )],
                    ret_type: Some(
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                        .into_spanned()
                    )
                },
                fn_scope: FnScope::Function,
                fn_type: FnType::Native(Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                }),
                fn_kw: Some(SimpleSpan::new(0, 0))
            }
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
                    ClassStmt::Method(FnDef {
                        fn_signature: FnSignature {
                            fn_id: "myClassMethod".into(),
                            args: vec![],
                            ret_type: Some(
                                ValueVarType {
                                    vtype: VarType::I32,
                                    array_dimensions: VecDeque::new(),
                                    pointer_nesting_level: 0
                                }
                                .into_spanned()
                            )
                        },
                        fn_scope: FnScope::Method,
                        fn_type: FnType::Native(Block {
                            stmts: vec![],
                            lbracket: Some(SimpleSpan::new(0, 0)),
                            rbracket: Some(SimpleSpan::new(0, 0))
                        }),
                        fn_kw: None
                    }),
                    ClassStmt::Property(PropertyDecl {
                        id: "myClassProp".into(),
                        vtype: Some(
                            ValueVarType {
                                vtype: VarType::F32,
                                array_dimensions: VecDeque::new(),
                                pointer_nesting_level: 0
                            }
                            .into_spanned()
                        ),
                        assigned_expr: Some(Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )),
                    })
                ]
            }
        )
    }

    #[test]
    fn class_decl_test() {
        let tokens = stream_token_vec(vec![
            Token::Class,
            Token::Id("MyClass".into()),
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
                        ClassStmt::Method(FnDef {
                            fn_signature: FnSignature {
                                fn_id: "myClassMethod".into(),
                                args: vec![],
                                ret_type: Some(
                                    ValueVarType {
                                        vtype: VarType::I32,
                                        array_dimensions: VecDeque::new(),
                                        pointer_nesting_level: 0
                                    }
                                    .into_spanned()
                                )
                            },
                            fn_scope: FnScope::Method,
                            fn_type: FnType::Native(Block {
                                stmts: vec![],
                                lbracket: Some(SimpleSpan::new(0, 0)),
                                rbracket: Some(SimpleSpan::new(0, 0))
                            }),
                            fn_kw: None
                        }),
                        ClassStmt::Property(PropertyDecl {
                            id: "myClassProp".into(),
                            vtype: Some(
                                ValueVarType {
                                    vtype: VarType::F32,
                                    array_dimensions: VecDeque::new(),
                                    pointer_nesting_level: 0
                                }
                                .into_spanned()
                            ),
                            assigned_expr: Some(Expr::PrimitiveVal(
                                PrimitiveVal::Number(None, NumericLiteral::Int("10"))
                                    .into_spanned()
                            )),
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
            FnDef {
                fn_signature: FnSignature {
                    fn_id: "myFunction".into(),
                    args: vec![
                        (
                            Destructure::Id("a".into()),
                            ValueVarType {
                                vtype: VarType::I32,
                                array_dimensions: VecDeque::new(),
                                pointer_nesting_level: 0
                            }
                            .into_spanned()
                        ),
                        (
                            Destructure::Id("b".into()),
                            ValueVarType {
                                vtype: VarType::I32,
                                array_dimensions: VecDeque::new(),
                                pointer_nesting_level: 0
                            }
                            .into_spanned()
                        )
                    ],
                    ret_type: Some(
                        ValueVarType {
                            vtype: VarType::I32,
                            array_dimensions: VecDeque::new(),
                            pointer_nesting_level: 0
                        }
                        .into_spanned()
                    )
                },
                fn_scope: FnScope::Function,
                fn_type: FnType::InlineLlvmIr(InlineLlvmIr {
                    lbracket: SimpleSpan::new(0, 0),
                    ir: "%res = add i32 %a, %b\nret i32 %res".to_string(),
                    rbracket: SimpleSpan::new(0, 0)
                }),
                fn_kw: Some(SimpleSpan::new(0, 0))
            }
        )
    }
}
