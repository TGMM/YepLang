use super::main_parser::{ParserError, ParserInput};
use crate::lexer::Token;
use chumsky::{primitive::just, span::SimpleSpan, Parser};

pub fn tag<'i>(
    tok: Token<'i>,
) -> impl Parser<'i, ParserInput<'i>, SimpleSpan, ParserError<'i, Token<'i>>> + Clone {
    just(tok).map_with_span(|_, span| span)
}
