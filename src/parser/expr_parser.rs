use super::main_parser::{ParserError, ParserInput};
use super::primitive_parser::{bop_parser, id_parser, primitive_val_parser, PRIMITIVE_VAL_PARSER};
use crate::ast::{BOp, BoolUnaryOp, FnCall, Id, Indexing, MemberAcess, NumericUnaryOp};
use crate::lexer::Token;
use crate::parser::main_parser::{GlobalParser, RecursiveParser};
use crate::{ast::Expr, recursive_parser};
use chumsky::primitive::just;
use chumsky::recursive::Recursive;
use chumsky::{select, IterParser, Parser};
use std::sync::{Arc, LazyLock, RwLock};

recursive_parser!(
    EXPR_PARSER,
    expr_parser,
    Expr<'static>,
    declarations {
        let paren_expr = PAREN_EXPR_PARSER.read().unwrap().clone()
        let primary_expr = PRIMARY_EXPR_PARSER.read().unwrap().clone()
    },
    main_definition {
        paren_expr.clone().or(primary_expr.clone()).pratt(bop_parser()).boxed()
    },
    definitions {
        if !paren_expr.is_defined() {
            paren_expr_parser();
        }
        if !primary_expr.is_defined() {
            primary_expr_parser();
        }
    }
);

pub fn numeric_unary_op_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, NumericUnaryOp, ParserError<'i, Token<'i>>> + Clone {
    let plus = bop_parser()
        .filter(|b| matches!(b, BOp::Add))
        .to(NumericUnaryOp::Plus);
    let minus = bop_parser()
        .filter(|b| matches!(b, BOp::Sub))
        .to(NumericUnaryOp::Minus);

    plus.or(minus)
}

pub fn boolean_unary_op_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, BoolUnaryOp, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::BoolUnaryOp(buop) => buop }
}

recursive_parser!(
    INDEXER_PARSER,
    indexer_parser,
    Expr<'static>,
    declarations {
        let expr = EXPR_PARSER.read().unwrap().clone()
    },
    main_definition {
        let indexer = expr
            .clone()
            .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket));

        indexer
    },
    definitions {
        if !expr.is_defined() {
            expr_parser();
        }
    }
);

recursive_parser!(
    FN_CALL_ARGS_PARSER,
    fn_call_args_parser,
    Vec<Expr<'static>>,
    declarations {
        let expr = EXPR_PARSER.read().unwrap().clone()
    },
    main_definition {
        let fn_call = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen));

        fn_call
    },
    definitions {
        if !expr.is_defined() {
            expr_parser();
        }
    }
);

pub fn member_access_prop_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, Id, ParserError<'i, Token<'i>>> + Clone {
    just(Token::Dot).ignore_then(id_parser())
}

enum PExprSuffix<'i> {
    Index(Expr<'i>),
    FnCall(Vec<Expr<'i>>),
    MemberAccess(Id),
}

recursive_parser!(
    PRIMARY_EXPR_PARSER,
    primary_expr_parser,
    Expr<'static>,
    declarations {
        let paren_expr = PAREN_EXPR_PARSER.read().unwrap().clone()
        let primitive_val = PRIMITIVE_VAL_PARSER.read().unwrap().clone()
        let indexer = INDEXER_PARSER.read().unwrap().clone()
        let fn_call_args = FN_CALL_ARGS_PARSER.read().unwrap().clone()
    },
    main_definition {
        let id = id_parser().map(Expr::Id);
        let primitive = primitive_val.clone().map(Expr::PrimitiveVal);

        let primary_expr = id.or(primitive).or(paren_expr.clone());

        let indexed_suffix = indexer.clone().map(PExprSuffix::Index);
        let fn_call_suffix = fn_call_args.clone().map(PExprSuffix::FnCall);
        let member_access_suffix = member_access_prop_parser().map(PExprSuffix::MemberAccess);
        let pexpr_suffix = indexed_suffix
            .or(fn_call_suffix)
            .or(member_access_suffix)
            .repeated();

        let suffixed_pexpr = primary_expr.foldl(pexpr_suffix, |pexpr, suffix| match suffix {
            PExprSuffix::Index(indexer) => Expr::Indexing(Box::new(Indexing {
                indexed: pexpr,
                indexer,
            })),
            PExprSuffix::FnCall(args) => Expr::FnCall(Box::new(FnCall {
                fn_expr: pexpr,
                args,
            })),
            PExprSuffix::MemberAccess(property) => Expr::MemberAccess(Box::new(MemberAcess {
                accessed: pexpr,
                property,
            })),
        });

        suffixed_pexpr
    },
    definitions {
        if !paren_expr.is_defined() {
            paren_expr_parser();
        }
        if !primitive_val.is_defined() {
            primitive_val_parser();
        }
        if !indexer.is_defined() {
            indexer_parser();
        }
        if !fn_call_args.is_defined() {
            fn_call_args_parser();
        }
    }
);

pub fn assignment_expr_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Expr<'i>, ParserError<'i, Token<'i>>> + Clone {
    let id = id_parser().map(Expr::Id);

    let indexed_suffix = indexer_parser().map(PExprSuffix::Index);
    let member_access_suffix = member_access_prop_parser().map(PExprSuffix::MemberAccess);
    let pexpr_suffix = indexed_suffix.or(member_access_suffix).repeated();

    let suffixed_pexpr = id.foldl(pexpr_suffix, |pexpr, suffix| match suffix {
        PExprSuffix::Index(indexer) => Expr::Indexing(Box::new(Indexing {
            indexed: pexpr,
            indexer,
        })),
        PExprSuffix::MemberAccess(property) => Expr::MemberAccess(Box::new(MemberAcess {
            accessed: pexpr,
            property,
        })),
        _ => unreachable!(),
    });

    suffixed_pexpr
}

recursive_parser!(
    PAREN_EXPR_PARSER,
    paren_expr_parser,
    Expr<'static>,
    declarations {
        let expr = EXPR_PARSER.read().unwrap().clone()
    },
    main_definition {
        let paren_expr = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen));

        paren_expr
    },
    definitions {
        if !expr.is_defined() {
            expr_parser();
        }
    }
);

#[cfg(test)]
mod test {
    use crate::{
        ast::{
            BExpr, BOp, BoolLiteral, BoolUnaryOp, Expr, FnCall, Indexing, MemberAcess,
            NumericLiteral, NumericUnaryOp, PrimitiveVal,
        },
        lexer::Token,
        parser::{
            expr_parser::{expr_parser, primary_expr_parser},
            helpers::test::stream_token_vec,
        },
    };
    use chumsky::Parser;

    #[test]
    fn primary_expr_primitive_test() {
        let tokens = stream_token_vec(vec![Token::IntVal("10")]);

        let res = primary_expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
        )
    }

    #[test]
    fn primary_expr_id_test() {
        let tokens = stream_token_vec(vec![Token::Id("x".into())]);

        let res = primary_expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(expr, Expr::Id("x".into()),)
    }

    #[test]
    fn expr_addition_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                op: BOp::Add,
                rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
            }))
        )
    }

    #[test]
    fn expr_subtraction_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Sub),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                op: BOp::Sub,
                rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
            }))
        )
    }

    #[test]
    fn expr_multiplication_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Mul),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                op: BOp::Mul,
                rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
            }))
        )
    }

    #[test]
    fn expr_division_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Div),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                op: BOp::Div,
                rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
            }))
        )
    }

    #[test]
    fn expr_lbp_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
            Token::BOp(BOp::Mul),
            Token::IntVal("15"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                op: BOp::Add,
                rhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5"))),
                    op: BOp::Mul,
                    rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("15")))
                }))
            }))
        )
    }

    #[test]
    fn expr_rbp_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Mul),
            Token::IntVal("5"),
            Token::BOp(BOp::Sub),
            Token::IntVal("15"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                    op: BOp::Mul,
                    rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
                })),
                op: BOp::Sub,
                rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("15"))),
            }))
        )
    }

    #[test]
    fn expr_cmp_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                    op: BOp::Add,
                    rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
                })),
                op: BOp::Gt,
                rhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
                    op: BOp::Add,
                    rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("5")))
                })),
            }))
        )
    }

    #[test]
    fn expr_fn_call_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("fn"),
            Token::LParen,
            Token::IntVal("10"),
            Token::RParen,
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::FnCall(
                FnCall {
                    fn_expr: Expr::Id("fn".into()),
                    args: vec![Expr::PrimitiveVal(PrimitiveVal::Number(
                        None,
                        NumericLiteral::Int("10")
                    ))]
                }
                .into()
            )
        )
    }

    #[test]
    fn expr_indexing_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("arr"),
            Token::LSqBracket,
            Token::IntVal("10"),
            Token::RSqBracket,
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::Indexing(
                Indexing {
                    indexed: Expr::Id("arr".into()),
                    indexer: Expr::PrimitiveVal(PrimitiveVal::Number(
                        None,
                        NumericLiteral::Int("10")
                    ))
                }
                .into()
            )
        )
    }

    #[test]
    fn expr_member_access_test() {
        let tokens = stream_token_vec(vec![Token::Id("obj"), Token::Dot, Token::Id("prop")]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::MemberAccess(
                MemberAcess {
                    accessed: Expr::Id("obj".into()),
                    property: "prop".into()
                }
                .into()
            )
        )
    }

    #[test]
    fn expr_bool_unary_test() {
        let tokens = stream_token_vec(vec![
            Token::BoolUnaryOp(BoolUnaryOp::Not),
            Token::BoolVal("true"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::PrimitiveVal(PrimitiveVal::Boolean(
                Some(BoolUnaryOp::Not),
                BoolLiteral(true)
            ))
        )
    }

    #[test]
    fn expr_paren_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::LParen,
            Token::BOp(BOp::Add),
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::BOp(BOp::Sub),
            Token::IntVal("5"),
            Token::RParen,
            Token::BOp(BOp::Mul),
            Token::IntVal("15"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                        Some(NumericUnaryOp::Plus),
                        NumericLiteral::Int("10")
                    )),
                    op: BOp::Add,
                    rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                        Some(NumericUnaryOp::Minus),
                        NumericLiteral::Int("5")
                    ))
                })),
                op: BOp::Mul,
                rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("15"))),
            }))
        )
    }
}
