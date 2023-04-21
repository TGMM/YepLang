use super::main_parser::ParseRes;
use super::token::Tokens;
use crate::ast::{ArrayVal, BoolLiteral, Id, NumericLiteral, PropertyName, StructVal};
use crate::lexer::Token;
use crate::parser::expr_parser::expr_parser;
use crate::tag_token;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, terminated};
use nom::Err;
use nom::IResult;

tag_token!(as_eq_tag, Ok(Token::AssignmentEq));
tag_token!(stmt_end_tag, Ok(Token::StmtEnd));
tag_token!(lparen_tag, Ok(Token::LParen));
tag_token!(rparen_tag, Ok(Token::RParen));
tag_token!(lsqbracket_tag, Ok(Token::LSqBracket));
tag_token!(rsqbracket_tag, Ok(Token::RSqBracket));
tag_token!(lbracket_tag, Ok(Token::LBracket));
tag_token!(rbracket_tag, Ok(Token::RBracket));
tag_token!(comma_tag, Ok(Token::Comma));
tag_token!(colon_tag, Ok(Token::Colon));

pub(crate) fn id_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Id> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::Id(id)) => Ok((remaining, id.into())),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn number_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, NumericLiteral> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::IntVal(val)) => Ok((remaining, NumericLiteral::Int(val))),
            Ok(Token::FloatVal(val)) => Ok((remaining, NumericLiteral::Float(val))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn string_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, &str> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::Str(string)) => Ok((remaining, string.trim_matches('"'))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn bool_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, BoolLiteral> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::BoolVal(b)) => Ok((remaining, b.into())),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn char_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, char> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::Char(c)) => {
                let c = c
                    .trim_matches('\'')
                    .chars()
                    .next()
                    .expect("Invalid empty char literal");
                Ok((remaining, c))
            }
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn array_val_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ArrayVal> {
    map(
        delimited(
            lsqbracket_tag,
            separated_list0(comma_tag, expr_parser),
            rsqbracket_tag,
        ),
        |exprs| ArrayVal(exprs),
    )(input)
}

pub(crate) fn struct_val_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, StructVal> {
    let id_prop_name = map(id_parser, |id| PropertyName::Id(id));
    let str_prop_name = map(string_parser, |s| PropertyName::String(s.to_string()));
    let prop_name = alt((id_prop_name, str_prop_name));
    let prop = pair(terminated(prop_name, colon_tag), expr_parser);

    map(
        delimited(lbracket_tag, separated_list0(comma_tag, prop), rbracket_tag),
        |props| StructVal(props),
    )(input)
}
