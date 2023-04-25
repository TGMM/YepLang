use super::primitive_parser::primitive_val_parser;
use super::{
    main_parser::ParseRes,
    primitive_parser::{id_parser, lparen_tag, rparen_tag},
    token::Tokens,
};
use crate::ast::{BExpr, BOp, Expr, NumericUnaryOp};
use crate::lexer::Token;
use nom::bytes::complete::take;
use nom::combinator::peek;
use nom::error::ErrorKind;
use nom::Err;
use nom::{branch::alt, combinator::map, sequence::delimited};

pub(crate) fn expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    expr_parser_helper(input, 0)
}

pub(crate) fn expr_parser_helper<'i>(input: Tokens<'i>, left_bp: u8) -> ParseRes<'i, Expr<'i>> {
    let pexpr_res = primary_expr_parser(input)?;
    let mut remaining = pexpr_res.0;
    let mut lhs = pexpr_res.1;
    loop {
        let op_res = peek(take(1usize))(remaining)?;
        let op_tok = op_res.1;
        remaining = op_res.0;

        let op = match &op_tok.tok_span[0].token {
            Ok(Token::RParen) => {
                // Since we peeked above, we need to consume it now
                let (r, _) = take(1usize)(remaining)?;
                remaining = r;
                break;
            }
            Ok(Token::BOp(bop)) => {
                let (r, _) = take(1usize)(remaining)?;
                remaining = r;
                *bop
            }
            _unexpected => {
                break;
            }
        };

        let right_bp = op.infix_binding_power();
        if right_bp < left_bp {
            break;
        }

        let expr_res = expr_parser_helper(remaining, right_bp)?;
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

pub(crate) fn unary_op_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, NumericUnaryOp> {
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

pub(crate) fn primary_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    let id = map(id_parser, |id| Expr::Id(id));
    let primitive = map(primitive_val_parser, |pv| Expr::PrimitiveVal(pv));

    alt((id, primitive))(input)
}

pub(crate) fn paren_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    delimited(lparen_tag, expr_parser, rparen_tag)(input)
}
