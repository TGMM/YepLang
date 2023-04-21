use super::primitive_parser::{
    as_eq_tag, colon_tag, comma_tag, id_parser, lbracket_tag, lsqbracket_tag, number_parser,
    rbracket_tag, rsqbracket_tag, string_parser,
};
use super::token::Tokens;
use super::{error::ParseError, primitive_parser::stmt_end_tag};
use crate::ast::{
    Assignment, Block, Destructure, Expr, PrimitiveVal, PropertyDestructure, PropertyName,
};
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;

pub(crate) type ParseRes<'a, T> = IResult<Tokens<'a>, T>;

pub(crate) fn stmt_end_parser<'i>(input: Tokens<'i>) -> IResult<Tokens<'i>, ()> {
    let res = stmt_end_tag(input);

    if res.is_err() {
        // TODO: Add an error to the error list
    }

    let (remaining, _) = res.unwrap();

    Ok((remaining, ()))
}

// TODO: This is not complete yet
pub(crate) fn destructure_parser<'i>(input: Tokens<'i>) -> IResult<Tokens<'i>, Destructure<'i>> {
    let id = map(id_parser, |r| r.into());
    let arr = map(
        delimited(
            lsqbracket_tag,
            separated_list0(comma_tag, destructure_parser),
            rsqbracket_tag,
        ),
        |ds| Destructure::Array(ds),
    );
    let obj = {
        let str_prop_name = map(string_parser, |s| PropertyName::String(s.to_string()));
        let id_prop_name = map(id_parser, |id| PropertyName::Id(id));

        fn alias<'i>(input: Tokens<'i>) -> IResult<Tokens<'i>, Destructure> {
            preceded(colon_tag, destructure_parser)(input)
        }

        let str_prop = map(pair(str_prop_name, alias), |(name, alias)| {
            PropertyDestructure {
                name,
                alias: Some(alias),
            }
        });
        let id_prop = map(pair(id_prop_name, opt(alias)), |(name, alias)| {
            PropertyDestructure { name, alias }
        });

        alt((id_prop, str_prop))
    };
    let obj = map(
        delimited(lbracket_tag, separated_list0(comma_tag, obj), rbracket_tag),
        |os| Destructure::Object(os),
    );

    alt((id, arr, obj))(input)
}

pub(crate) fn assignment_parser<'i>(input: Tokens<'i>) -> IResult<Tokens<'i>, Assignment<'i>> {
    let (input, destructure) = destructure_parser(input)?;
    let (input, _) = as_eq_tag(input)?;
    let (input, number) = number_parser(input)?;
    let (input, _) = stmt_end_parser(input)?;

    Ok((
        input,
        Assignment {
            destructure,
            assigned_expr: Expr::PrimitiveVal(PrimitiveVal::Number(None, number)),
        },
    ))
}

pub(crate) fn top_block_parser<'i>(
    input: Tokens<'i>,
) -> IResult<Tokens<'i>, Block<'i>, ParseError> {
    todo!()
}

pub fn parse(input: &str) {
    todo!()
}

#[cfg(test)]
mod test {
    use super::assignment_parser;
    use crate::{
        lexer::Token,
        parser::token::{TokenSpan, Tokens},
    };
    use logos::Logos;

    #[test]
    fn test() {
        let input = r#"{test: {"a": a1, b: b1}} = 10;"#;
        let token_iter = Token::lexer(input)
            .spanned()
            .map(|(token, span)| TokenSpan { span, token })
            .collect::<Vec<_>>();
        let tokens = Tokens::new(&token_iter);

        let res = assignment_parser(tokens);
        assert!(res.is_ok());
    }
}
