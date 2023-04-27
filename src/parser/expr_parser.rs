use super::primitive_parser::{
    comma_tag, dot_tag, lsqbracket_tag, primitive_val_parser, rsqbracket_tag,
};
use super::{
    main_parser::ParseRes,
    primitive_parser::{id_parser, lparen_tag, rparen_tag},
    token::Tokens,
};
use crate::ast::{
    BExpr, BOp, BoolUnaryOp, Expr, FnCall, Id, Indexing, MemberAcess, NumericUnaryOp,
};
use crate::lexer::Token;
use nom::bytes::complete::take;
use nom::combinator::peek;
use nom::error::ErrorKind;
use nom::multi::separated_list0;
use nom::sequence::preceded;
use nom::Err;
use nom::{branch::alt, combinator::map, sequence::delimited};

pub(crate) fn expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    expr_parser_helper(input, 0)
}

pub(crate) fn expr_parser_helper<'i>(input: Tokens<'i>, min_bp: u8) -> ParseRes<'i, Expr<'i>> {
    let pexpr_res = primary_expr_parser(input)?;
    let mut remaining = pexpr_res.0;
    let mut lhs = pexpr_res.1;
    loop {
        let op_res: ParseRes<'i, Tokens<'i>> = peek(take(1usize))(remaining);
        // We ran out of tokens, exit the loop
        if op_res.is_err() {
            break;
        }

        let op_res = op_res.unwrap();
        let op_tok = op_res.1;
        remaining = op_res.0;

        let op = match &op_tok.tok_span[0].token {
            Ok(Token::BOp(bop)) => *bop,
            Ok(Token::RParen) => {
                break;
            }
            _unexpected => {
                break;
            }
        };

        let left_bp = op.infix_binding_power();
        if left_bp < min_bp {
            break;
        }

        // Since we peeked above, we need to consume it now
        let (r, _) = take(1usize)(remaining)?;
        remaining = r;

        let expr_res = expr_parser_helper(remaining, left_bp)?;
        let rhs = expr_res.1;
        remaining = expr_res.0;

        lhs = Expr::BinaryExpr(Box::new(BExpr {
            lhs: lhs.clone(),
            op,
            rhs,
        }));
    }

    Ok((remaining, lhs))
}

pub(crate) fn numeric_unary_op_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, NumericUnaryOp> {
    let (remaining, op) = take(1usize)(input)?;
    let op = match &op.tok_span[0].token {
        Ok(Token::BOp(BOp::Add)) => NumericUnaryOp::Plus,
        Ok(Token::BOp(BOp::Sub)) => NumericUnaryOp::Minus,
        _unexpected => {
            return Err(Err::Error(nom::error::Error {
                input,
                code: ErrorKind::Tag,
            }));
        }
    };

    Ok((remaining, op))
}

pub(crate) fn boolean_unary_op_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, BoolUnaryOp> {
    let (remaining, op) = take(1usize)(input)?;
    let op = match &op.tok_span[0].token {
        Ok(Token::BoolUnaryOp(buop)) => *buop,
        _unexpected => {
            return Err(Err::Error(nom::error::Error {
                input,
                code: ErrorKind::Tag,
            }));
        }
    };

    Ok((remaining, op))
}

pub(crate) fn indexer_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    delimited(lsqbracket_tag, expr_parser, rsqbracket_tag)(input)
}

pub(crate) fn create_indexing_expr<'i>(
    res: ParseRes<'i, Expr<'i>>,
    input: Tokens<'i>,
    primary_expr: Expr<'i>,
) -> (Tokens<'i>, Expr<'i>) {
    if let Ok((idx_input, indexer_expr)) = res {
        return (
            idx_input,
            Expr::Indexing(
                Indexing {
                    indexed: primary_expr,
                    indexer: indexer_expr,
                }
                .into(),
            ),
        );
    }

    (input, primary_expr)
}

pub(crate) fn fn_call_args_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Vec<Expr<'i>>> {
    delimited(
        lparen_tag,
        separated_list0(comma_tag, expr_parser),
        rparen_tag,
    )(input)
}

pub(crate) fn create_fn_call_expr<'i>(
    res: ParseRes<'i, Vec<Expr<'i>>>,
    input: Tokens<'i>,
    primary_expr: Expr<'i>,
) -> (Tokens<'i>, Expr<'i>) {
    if let Ok((fn_call_input, fn_call_args)) = res {
        return (
            fn_call_input,
            Expr::FnCall(
                FnCall {
                    fn_expr: primary_expr,
                    args: fn_call_args,
                }
                .into(),
            ),
        );
    }

    (input, primary_expr)
}

pub(crate) fn member_access_prop_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Id> {
    preceded(dot_tag, id_parser)(input)
}

pub(crate) fn create_member_access_expr<'i>(
    res: ParseRes<'i, Id>,
    input: Tokens<'i>,
    primary_expr: Expr<'i>,
) -> (Tokens<'i>, Expr<'i>) {
    if let Ok((member_access_input, member_access)) = res {
        return (
            member_access_input,
            Expr::MemberAccess(
                MemberAcess {
                    accessed: primary_expr,
                    property: member_access,
                }
                .into(),
            ),
        );
    }

    (input, primary_expr)
}

pub(crate) fn primary_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    let id = map(id_parser, |id| Expr::Id(id));
    let primitive = map(primitive_val_parser, |pv| Expr::PrimitiveVal(pv));
    let paren_expr = paren_expr_parser;

    let (mut input, mut primary_expr) = alt((id, primitive, paren_expr))(input)?;

    loop {
        let indexer_result = indexer_parser(input);
        let fn_call_args_result = fn_call_args_parser(input);
        let member_access_result = member_access_prop_parser(input);

        if indexer_result.is_err() && fn_call_args_result.is_err() && member_access_result.is_err()
        {
            break;
        }

        (input, primary_expr) = create_indexing_expr(indexer_result, input, primary_expr);
        (input, primary_expr) = create_fn_call_expr(fn_call_args_result, input, primary_expr);
        (input, primary_expr) =
            create_member_access_expr(member_access_result, input, primary_expr);
    }

    Ok((input, primary_expr))
}

pub(crate) fn assignment_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    let mut id = map(id_parser, |id| Expr::Id(id));
    let (mut input, mut primary_expr) = id(input)?;

    loop {
        let indexer_result = indexer_parser(input);
        let member_access_result = member_access_prop_parser(input);

        if indexer_result.is_err() && member_access_result.is_err() {
            break;
        }

        (input, primary_expr) = create_indexing_expr(indexer_result, input, primary_expr);
        (input, primary_expr) =
            create_member_access_expr(member_access_result, input, primary_expr);
    }

    Ok((input, primary_expr))
}

pub(crate) fn paren_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    delimited(lparen_tag, expr_parser, rparen_tag)(input)
}

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
            helpers::test::span_token_vec,
            token::Tokens,
        },
    };

    #[test]
    fn primary_expr_primitive_test() {
        let token_iter = span_token_vec(vec![Token::IntVal("10")]);
        let tokens = Tokens::new(&token_iter);

        let res = primary_expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
        assert_eq!(
            expr,
            Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10"))),
        )
    }

    #[test]
    fn primary_expr_id_test() {
        let token_iter = span_token_vec(vec![Token::Id("x".into())]);
        let tokens = Tokens::new(&token_iter);

        let res = primary_expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
        assert_eq!(expr, Expr::Id("x".into()),)
    }

    #[test]
    fn expr_addition_test() {
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Sub),
            Token::IntVal("5"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Mul),
            Token::IntVal("5"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Div),
            Token::IntVal("5"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
            Token::BOp(BOp::Mul),
            Token::IntVal("15"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Mul),
            Token::IntVal("5"),
            Token::BOp(BOp::Sub),
            Token::IntVal("15"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("5"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::Id("fn"),
            Token::LParen,
            Token::IntVal("10"),
            Token::RParen,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::Id("arr"),
            Token::LSqBracket,
            Token::IntVal("10"),
            Token::RSqBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![Token::Id("obj"), Token::Dot, Token::Id("prop")]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
            Token::BoolUnaryOp(BoolUnaryOp::Not),
            Token::BoolVal("true"),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
        let token_iter = span_token_vec(vec![
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
        let tokens = Tokens::new(&token_iter);

        let res = expr_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
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
