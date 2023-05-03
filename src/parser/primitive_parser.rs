use super::main_parser::{ParserError, ParserInput};
use crate::{
    ast::{Id, ScopeSpecifier},
    lexer::Token,
};
use chumsky::{select, Parser};

pub fn id_parser<'i>() -> impl Parser<'i, ParserInput<'i>, Id, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::Id(id) => id.into() }
}

pub fn scope_specifier_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, ScopeSpecifier, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::ScopeSpecifier(ss) => ss }
}
