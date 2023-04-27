use nom::{branch::alt, combinator::map, multi::separated_list0, sequence::delimited};

use crate::{
    ast::{ExternDecl, ExternType},
    parser::primitive_parser::{comma_tag, extern_tag, id_parser},
};

use super::{
    main_parser::ParseRes,
    primitive_parser::{lparen_tag, rparen_tag, spread_tag, value_var_type_parser},
    token::Tokens,
};

pub(crate) fn extern_type_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ExternType> {
    let arg_type = map(value_var_type_parser, |v| ExternType::Type(v));
    let var_args = map(spread_tag, |_| ExternType::Spread);

    alt((arg_type, var_args))(input)
}

pub(crate) fn extern_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ExternDecl> {
    let (input, _) = extern_tag(input)?;
    let (input, ret_type) = value_var_type_parser(input)?;
    let (input, fn_id) = id_parser(input)?;
    let (input, arg_types) = delimited(
        lparen_tag,
        separated_list0(comma_tag, extern_type_parser),
        rparen_tag,
    )(input)?;

    Ok((
        input,
        ExternDecl {
            ret_type,
            fn_id,
            arg_types,
        },
    ))
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{BOp, ExternDecl, ExternType, ValueVarType, VarType},
        lexer::Token,
        parser::{ffi_parser::extern_decl_parser, helpers::test::span_token_vec, token::Tokens},
    };

    #[test]
    fn extern_decl_test() {
        // extern i32 printf(*u8, ...);
        let token_iter = span_token_vec(vec![
            Token::Extern,
            Token::VarType(VarType::I32),
            Token::Id("printf"),
            Token::LParen,
            Token::BOp(BOp::Mul),
            Token::VarType(VarType::U8),
            Token::Comma,
            Token::Spread,
            Token::RParen,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = extern_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, expr) = res.unwrap();
        assert_eq!(
            expr,
            ExternDecl {
                ret_type: ValueVarType {
                    vtype: VarType::I32,
                    array_nesting_level: 0,
                    pointer_nesting_level: 0
                },
                fn_id: "printf".into(),
                arg_types: vec![
                    ExternType::Type(ValueVarType {
                        vtype: VarType::U8,
                        array_nesting_level: 0,
                        pointer_nesting_level: 1
                    }),
                    ExternType::Spread
                ]
            },
        )
    }
}
