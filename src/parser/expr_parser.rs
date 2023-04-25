use super::primitive_parser::primitive_val_parser;
use super::{
    main_parser::ParseRes,
    primitive_parser::{id_parser, lparen_tag, rparen_tag},
    token::Tokens,
};
use crate::ast::{BExpr, BOp, BoolUnaryOp, Expr, NumericUnaryOp};
use crate::lexer::Token;
use nom::bytes::complete::take;
use nom::combinator::peek;
use nom::error::ErrorKind;
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

pub(crate) fn primary_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    let id = map(id_parser, |id| Expr::Id(id));
    let primitive = map(primitive_val_parser, |pv| Expr::PrimitiveVal(pv));
    let paren_expr = paren_expr_parser;

    alt((id, primitive, paren_expr))(input)
}

pub(crate) fn paren_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    delimited(lparen_tag, expr_parser, rparen_tag)(input)
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{BExpr, BOp, Expr, NumericLiteral, NumericUnaryOp, PrimitiveVal},
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
