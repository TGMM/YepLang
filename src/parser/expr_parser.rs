use crate::ast::Expr;
use nom::{branch::alt, combinator::map, sequence::delimited};

use super::{
    main_parser::ParseRes,
    primitive_parser::{id_parser, lparen_tag, rparen_tag},
    token::Tokens,
};

pub(crate) fn expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    let id = map(id_parser, |id| Expr::Id(id));
    let paren_expr = paren_expr_parser;

    alt((id, paren_expr))(input)
}

pub(crate) fn paren_expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    delimited(lparen_tag, expr_parser, rparen_tag)(input)
}
