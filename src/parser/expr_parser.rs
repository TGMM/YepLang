use crate::ast::{BOp, BoolUnaryOp, NumericUnaryOp};
use crate::lexer::Token;
use crate::parser::main_parser::{GlobalParser, RecursiveParser};
use crate::{ast::Expr, recursive_parser};
use chumsky::primitive::todo;
use chumsky::recursive::Recursive;
use chumsky::{select, Parser};
use std::sync::{Arc, LazyLock, RwLock};

use super::main_parser::{ParserError, ParserInput};
use super::primitive_parser::bop_parser;

recursive_parser!(
    EXPR_PARSER,
    expr_parser,
    Expr<'static>,
    declarations {},
    main_definition {
        todo()
    },
    definitions {}
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
