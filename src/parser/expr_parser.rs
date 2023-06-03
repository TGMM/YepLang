use super::keyword_parser::tag;
use super::main_parser::{ParserError, ParserInput};
use super::primitive_parser::{
    bop_parser, id_parser, primitive_val_parser, value_var_type_parser, PRIMITIVE_VAL_PARSER,
};
use crate::ast::{
    BOp, BoolUnaryOp, Casting, FnCall, Id, Indexing, MemberAcess, NumericUnaryOp, ValueVarType,
};
use crate::lexer::Token;
use crate::parser::main_parser::{GlobalParser, RecursiveParser};
use crate::spanned_ast::SpannedAstNode;
use crate::{ast::Expr, recursive_parser};
use chumsky::primitive::just;
use chumsky::recursive::Recursive;
use chumsky::span::SimpleSpan;
use chumsky::{select, IterParser, Parser};
use std::sync::{Arc, LazyLock, RwLock};

recursive_parser!(
    EXPR_PARSER,
    expr_parser,
    Expr<'static>,
    declarations {
        let primary_expr = PRIMARY_EXPR_PARSER.read().unwrap().clone()
    },
    main_definition {
        primary_expr.clone().pratt(bop_parser()).boxed()
    },
    definitions {
        if !primary_expr.is_defined() {
            primary_expr_parser();
        }
    }
);

pub fn numeric_unary_op_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, SpannedAstNode<NumericUnaryOp>, ParserError<'i, Token<'i>>> + Clone
{
    use BOp::*;

    let plus = bop_parser()
        .filter(|b| matches!(b.node, Add))
        .map(|b| SpannedAstNode {
            node: NumericUnaryOp::Plus,
            span: b.span,
        });
    let minus = bop_parser()
        .filter(|b| matches!(b.node, Sub))
        .map(|b| SpannedAstNode {
            node: NumericUnaryOp::Minus,
            span: b.span,
        });

    plus.or(minus)
}

pub fn boolean_unary_op_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, SpannedAstNode<BoolUnaryOp>, ParserError<'i, Token<'i>>> + Clone
{
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

pub fn as_casting_parser<'i>() -> impl Parser<
    'i,
    ParserInput<'i>,
    (SimpleSpan, SpannedAstNode<ValueVarType>),
    ParserError<'i, Token<'i>>,
> + Clone {
    tag(Token::As).then(value_var_type_parser())
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
            }))
        });

        let cast_suffix = as_casting_parser().repeated();
        let casted_pexpr = suffixed_pexpr.foldl(cast_suffix, |pexpr, (as_kw, cast_type)| Expr::Cast(Box::new(Casting {
            casted: pexpr,
            as_kw,
            cast_type
        })));

        casted_pexpr
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
    // Declarations
    let indexer_parser_ = INDEXER_PARSER.read().unwrap().clone();

    // Main definition
    let id = id_parser().map(Expr::Id);

    let indexed_suffix = indexer_parser_.clone().map(PExprSuffix::Index);
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

    // Definitions
    if !indexer_parser_.is_defined() {
        indexer_parser();
    }

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
            helpers::test::{stream_token_vec, IntoSpanned},
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
            Expr::PrimitiveVal(
                PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
            ),
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
            Token::BOp(BOp::Add.into_spanned()),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                ),
                op: BOp::Add.into_spanned(),
                rhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                )
            }))
        )
    }

    #[test]
    fn expr_subtraction_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Sub.into_spanned()),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                ),
                op: BOp::Sub.into_spanned(),
                rhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                )
            }))
        )
    }

    #[test]
    fn expr_multiplication_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Mul.into_spanned()),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                ),
                op: BOp::Mul.into_spanned(),
                rhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                )
            }))
        )
    }

    #[test]
    fn expr_division_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Div.into_spanned()),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                ),
                op: BOp::Div.into_spanned(),
                rhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                )
            }))
        )
    }

    #[test]
    fn expr_lbp_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add.into_spanned()),
            Token::IntVal("5"),
            Token::BOp(BOp::Mul.into_spanned()),
            Token::IntVal("15"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                ),
                op: BOp::Add.into_spanned(),
                rhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                    ),
                    op: BOp::Mul.into_spanned(),
                    rhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("15")).into_spanned()
                    )
                }))
            }))
        )
    }

    #[test]
    fn expr_rbp_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Mul.into_spanned()),
            Token::IntVal("5"),
            Token::BOp(BOp::Sub.into_spanned()),
            Token::IntVal("15"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                    ),
                    op: BOp::Mul.into_spanned(),
                    rhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                    )
                })),
                op: BOp::Sub.into_spanned(),
                rhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("15")).into_spanned()
                ),
            }))
        )
    }

    #[test]
    fn expr_cmp_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add.into_spanned()),
            Token::IntVal("5"),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::BOp(BOp::Add.into_spanned()),
            Token::IntVal("5"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                    ),
                    op: BOp::Add.into_spanned(),
                    rhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                    )
                })),
                op: BOp::Gt.into_spanned(),
                rhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                    ),
                    op: BOp::Add.into_spanned(),
                    rhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("5")).into_spanned()
                    )
                })),
            }))
        )
    }

    #[test]
    fn expr_fn_call_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("fn".into()),
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
                    args: vec![Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                    )]
                }
                .into()
            )
        )
    }

    #[test]
    fn expr_indexing_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("arr".into()),
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
                    indexer: Expr::PrimitiveVal(
                        PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                    )
                }
                .into()
            )
        )
    }

    #[test]
    fn expr_member_access_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("obj".into()),
            Token::Dot,
            Token::Id("prop".into()),
        ]);

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
            Token::BoolUnaryOp(BoolUnaryOp::Not.into_spanned()),
            Token::BoolVal("true"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::PrimitiveVal(
                PrimitiveVal::Boolean(Some(BoolUnaryOp::Not.into_spanned()), BoolLiteral(true))
                    .into_spanned()
            )
        )
    }

    #[test]
    fn expr_paren_precedence_test() {
        let tokens = stream_token_vec(vec![
            Token::LParen,
            Token::BOp(BOp::Add.into_spanned()),
            Token::IntVal("10"),
            Token::BOp(BOp::Add.into_spanned()),
            Token::BOp(BOp::Sub.into_spanned()),
            Token::IntVal("5"),
            Token::RParen,
            Token::BOp(BOp::Mul.into_spanned()),
            Token::IntVal("15"),
        ]);

        let res = expr_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            Expr::BinaryExpr(Box::new(BExpr {
                lhs: Expr::BinaryExpr(Box::new(BExpr {
                    lhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(
                            Some(NumericUnaryOp::Plus.into_spanned()),
                            NumericLiteral::Int("10")
                        )
                        .into_spanned()
                    ),
                    op: BOp::Add.into_spanned(),
                    rhs: Expr::PrimitiveVal(
                        PrimitiveVal::Number(
                            Some(NumericUnaryOp::Minus.into_spanned()),
                            NumericLiteral::Int("5")
                        )
                        .into_spanned()
                    )
                })),
                op: BOp::Mul.into_spanned(),
                rhs: Expr::PrimitiveVal(
                    PrimitiveVal::Number(None, NumericLiteral::Int("15")).into_spanned()
                ),
            }))
        )
    }
}
