use super::{
    expr_parser::expr_parser,
    main_parser::{block_parser, destructure_parser, stmt_end_parser, ParseRes},
    primitive_parser::{
        as_eq_tag, class_tag, comma_tag, extends_tag, fn_tag, id_parser, lbracket_tag, lparen_tag,
        rbracket_tag, rparen_tag, type_specifier_parser,
    },
    token::Tokens,
};
use crate::ast::{ClassBlock, ClassDecl, ClassStmt, Destructure, FnDecl, MethodDecl, PropertyDecl};
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
};

pub(crate) fn method_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, MethodDecl<'i>> {
    let (input, method_id) = id_parser(input)?;
    let (input, _) = lparen_tag(input)?;
    let (input, args) =
        separated_list0(comma_tag, pair(destructure_parser, type_specifier_parser))(input)?;
    let (input, _) = rparen_tag(input)?;
    let (input, ret_type) = opt(type_specifier_parser)(input)?;
    let (input, block) = block_parser(input)?;

    Ok((
        input,
        MethodDecl {
            method_id,
            args,
            ret_type,
            block,
        },
    ))
}

pub(crate) fn fn_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, FnDecl<'i>> {
    let (input, _) = fn_tag(input)?;
    let (input, method_decl) = method_decl_parser(input)?;

    Ok((input, method_decl.into()))
}

pub(crate) fn class_prop_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, PropertyDecl<'i>> {
    let (input, id) = id_parser(input)?;
    let (input, vtype) = opt(type_specifier_parser)(input)?;
    let (input, _) = as_eq_tag(input)?;
    let (input, assigned_expr) = expr_parser(input)?;

    Ok((
        input,
        PropertyDecl {
            id,
            vtype,
            assigned_expr,
        },
    ))
}

pub(crate) fn class_stmt_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ClassStmt<'i>> {
    // This includes the constructor too, since it's just a method
    let method = map(method_decl_parser, |md| ClassStmt::Method(md));
    let accessor = {}; // TODO?: Maybe won't be doing this
    let property = map(terminated(class_prop_decl_parser, stmt_end_parser), |pd| {
        ClassStmt::Property(pd)
    });

    let (input, class_stmt) = alt((method, property))(input)?;
    dbg!(&class_stmt);
    Ok((input, class_stmt))
}

pub(crate) fn class_block_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ClassBlock<'i>> {
    map(
        delimited(lbracket_tag, many0(class_stmt_parser), rbracket_tag),
        |class_stmts| ClassBlock { class_stmts },
    )(input)
}

pub(crate) fn class_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ClassDecl<'i>> {
    let (input, _) = class_tag(input)?;
    let (input, class_id) = id_parser(input)?;
    let (input, extended_class_id) = opt(preceded(extends_tag, id_parser))(input)?;
    let (input, block) = class_block_parser(input)?;

    Ok((
        input,
        ClassDecl {
            class_id,
            extended_class_id,
            block,
        },
    ))
}

#[cfg(test)]
mod test {
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
            helpers::test::span_token_vec,
            token::Tokens,
        },
    };

    #[test]
    fn method_decl_test() {
        let token_iter = span_token_vec(vec![
            Token::Id("myMethod".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = method_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, method_decl) = res.unwrap();
        assert_eq!(
            method_decl,
            MethodDecl {
                method_id: "myMethod".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_test() {
        let token_iter = span_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = fn_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, fn_decl) = res.unwrap();
        assert_eq!(
            fn_decl,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_void_test() {
        let token_iter = span_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = fn_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, fn_decl) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
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
        let tokens = Tokens::new(&token_iter);

        let res = fn_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, fn_decl) = res.unwrap();
        assert_eq!(
            fn_decl,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![(
                    Destructure::Id("x".into()),
                    ValueVarType {
                        vtype: VarType::I32,
                        is_array: false
                    }
                )],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn class_block_test() {
        let token_iter = span_token_vec(vec![
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
        let tokens = Tokens::new(&token_iter);

        let res = class_block_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, class_block) = res.unwrap();
        assert_eq!(
            class_block,
            ClassBlock {
                class_stmts: vec![
                    ClassStmt::Method(MethodDecl {
                        method_id: "myClassMethod".into(),
                        args: vec![],
                        ret_type: Some(ValueVarType {
                            vtype: VarType::I32,
                            is_array: false
                        }),
                        block: Block { stmts: vec![] }
                    }),
                    ClassStmt::Property(PropertyDecl {
                        id: "myClassProp".into(),
                        vtype: Some(ValueVarType {
                            vtype: VarType::F32,
                            is_array: false,
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
        let token_iter = span_token_vec(vec![
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
        let tokens = Tokens::new(&token_iter);

        let res = class_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, class_decl) = res.unwrap();
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
                                is_array: false
                            }),
                            block: Block { stmts: vec![] }
                        }),
                        ClassStmt::Property(PropertyDecl {
                            id: "myClassProp".into(),
                            vtype: Some(ValueVarType {
                                vtype: VarType::F32,
                                is_array: false,
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
