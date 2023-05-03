use super::{
    expr_parser::{expr_parser, EXPR_PARSER},
    main_parser::{
        block_parser, destructure_parser, stmt_end_parser, ParserError, ParserInput,
        DESTRUCTURE_PARSER,
    },
    primitive_parser::{id_parser, type_specifier_parser},
};
use crate::parser::main_parser::BLOCK_PARSER;
use crate::{
    ast::{ClassBlock, ClassDecl, ClassStmt, FnDecl, MethodDecl, PropertyDecl},
    lexer::Token,
};
use chumsky::{primitive::just, IterParser, Parser};

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

pub fn fn_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, FnDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    just(Token::Function)
        .ignore_then(method_decl_parser())
        .map(|md| md.into())
}

pub fn class_prop_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, PropertyDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let class_prop = id_parser()
        .then(type_specifier_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(expr.clone())
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

#[cfg(test)]
mod test {
    use chumsky::Parser;

    use crate::{
        ast::{
            Block, ClassBlock, ClassDecl, ClassStmt, Destructure, Expr, FnDecl, MethodDecl,
            NumericLiteral, PrimitiveVal, PropertyDecl, ValueVarType, VarType,
        },
        lexer::Token,
        parser::{
            class_parser::{
                class_block_parser, class_decl_parser, fn_decl_parser, method_decl_parser,
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
                    array_nesting_level: 0,
                    pointer_nesting_level: 0
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_test() {
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

        let res = fn_decl_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_decl = res.unwrap();
        assert_eq!(
            fn_decl,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    array_nesting_level: 0,
                    pointer_nesting_level: 0
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_void_test() {
        let tokens = stream_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = fn_decl_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_decl = res.unwrap();
        assert_eq!(
            fn_decl,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: None,
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_args_test() {
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

        let res = fn_decl_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let fn_decl = res.unwrap();
        assert_eq!(
            fn_decl,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![(
                    Destructure::Id("x".into()),
                    ValueVarType {
                        vtype: VarType::I32,
                        array_nesting_level: 0,
                        pointer_nesting_level: 0
                    }
                )],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    array_nesting_level: 0,
                    pointer_nesting_level: 0
                }),
                block: Block { stmts: vec![] }
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
                    ClassStmt::Method(MethodDecl {
                        method_id: "myClassMethod".into(),
                        args: vec![],
                        ret_type: Some(ValueVarType {
                            vtype: VarType::I32,
                            array_nesting_level: 0,
                            pointer_nesting_level: 0
                        }),
                        block: Block { stmts: vec![] }
                    }),
                    ClassStmt::Property(PropertyDecl {
                        id: "myClassProp".into(),
                        vtype: Some(ValueVarType {
                            vtype: VarType::F32,
                            array_nesting_level: 0,
                            pointer_nesting_level: 0
                        }),
                        assigned_expr: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
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
                                array_nesting_level: 0,
                                pointer_nesting_level: 0
                            }),
                            block: Block { stmts: vec![] }
                        }),
                        ClassStmt::Property(PropertyDecl {
                            id: "myClassProp".into(),
                            vtype: Some(ValueVarType {
                                vtype: VarType::F32,
                                array_nesting_level: 0,
                                pointer_nesting_level: 0
                            }),
                            assigned_expr: Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Int("10")
                            )),
                        })
                    ]
                }
            }
        )
    }
}
