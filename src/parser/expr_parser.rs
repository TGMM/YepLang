use crate::ast::Expr;

use super::{main_parser::ParseRes, token::Tokens};

pub(crate) fn expr_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Expr<'i>> {
    todo!()
}
