use super::expr_parser::expr_parser;
use super::primitive_parser::stmt_end_tag;
use super::primitive_parser::{
    as_eq_tag, colon_tag, comma_tag, id_parser, lbracket_tag, lsqbracket_tag, rbracket_tag,
    rsqbracket_tag, string_parser,
};
use super::token::Tokens;
use crate::ast::{Assignment, Block, Destructure, PropertyDestructure, PropertyName, Stmt};
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::error::ErrorKind;
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;

pub(crate) type ParseRes<'a, T> = IResult<Tokens<'a>, T>;

pub(crate) fn stmt_end_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ()> {
    let res = stmt_end_tag(input);

    if res.is_err() {
        return Err(nom::Err::Error(nom::error::Error {
            input,
            code: ErrorKind::Tag,
        }));
    }

    let (remaining, _) = res.unwrap();

    Ok((remaining, ()))
}

pub(crate) fn destructure_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Destructure<'i>> {
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

        fn alias<'i>(input: Tokens<'i>) -> ParseRes<'i, Destructure<'i>> {
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

pub(crate) fn assignment_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Assignment<'i>> {
    let (input, destructure) = destructure_parser(input)?;
    let (input, _) = as_eq_tag(input)?;
    let (input, expr) = expr_parser(input)?;
    let (input, _) = stmt_end_parser(input)?;

    Ok((
        input,
        Assignment {
            destructure,
            assigned_expr: expr,
        },
    ))
}

pub(crate) fn stmt_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Stmt<'i>> {
    let assignment = map(assignment_parser, |a| Stmt::Assignment(a));
    let expr = map(expr_parser, |e| Stmt::Expr(e));

    alt((assignment, expr))(input)
}

pub(crate) fn top_block_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Block<'i>> {
    map(many0(stmt_parser), |stmts| Block { stmts })(input)
}

pub(crate) fn block_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Block<'i>> {
    delimited(lbracket_tag, top_block_parser, rbracket_tag)(input)
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::Token,
        parser::{
            main_parser::top_block_parser,
            token::{TokenSpan, Tokens},
        },
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

        let res = top_block_parser(tokens);
        assert!(res.is_ok());
    }
}
