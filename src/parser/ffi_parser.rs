use super::{
    main_parser::{ParserError, ParserInput},
    primitive_parser::{id_parser, value_var_type_parser},
};
use crate::{
    ast::{ExternDecl, ExternType},
    lexer::Token,
};
use chumsky::{primitive::just, IterParser, Parser};

pub fn extern_type_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ExternType, ParserError<'i, Token<'i>>> + Clone {
    let arg_type = value_var_type_parser().map(ExternType::Type);
    let var_args = just(Token::Spread).to(ExternType::Spread);

    arg_type.or(var_args)
}

pub fn extern_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ExternDecl, ParserError<'i, Token<'i>>> + Clone {
    let arg_types_p = extern_type_parser()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen));

    just(Token::Extern)
        .ignore_then(value_var_type_parser())
        .then(id_parser())
        .then(arg_types_p)
        .map(|((ret_type, fn_id), arg_types)| ExternDecl {
            ret_type,
            fn_id,
            arg_types,
        })
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;

    use crate::{
        ast::{BOpType, ExternDecl, ExternType, ValueVarType, VarType},
        lexer::Token,
        parser::{ffi_parser::extern_decl_parser, helpers::test::stream_token_vec},
    };
    use chumsky::Parser;

    #[test]
    fn extern_decl_test() {
        // extern i32 printf(*u8, ...);
        let tokens = stream_token_vec(vec![
            Token::Extern,
            Token::VarType(VarType::I32),
            Token::Id("printf".into()),
            Token::LParen,
            Token::BOp(BOpType::Mul.into()),
            Token::VarType(VarType::U8),
            Token::Comma,
            Token::Spread,
            Token::RParen,
        ]);

        let res = extern_decl_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let expr = res.unwrap();
        assert_eq!(
            expr,
            ExternDecl {
                ret_type: ValueVarType {
                    vtype: VarType::I32,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0
                },
                fn_id: "printf".into(),
                arg_types: vec![
                    ExternType::Type(ValueVarType {
                        vtype: VarType::U8,
                        array_dimensions: VecDeque::new(),
                        pointer_nesting_level: 1
                    }),
                    ExternType::Spread
                ]
            },
        )
    }
}
