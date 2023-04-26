use nom::{combinator::opt, multi::separated_list0, sequence::pair};

use crate::ast::{FnDecl, MethodDecl};

use super::{
    main_parser::{block_parser, destructure_parser, ParseRes},
    primitive_parser::{
        comma_tag, fn_tag, id_parser, lparen_tag, rparen_tag, type_specifier_parser,
    },
    token::Tokens,
};

pub(crate) fn method_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, MethodDecl<'i>> {
    let (input, method_id) = id_parser(input)?;
    let (input, _) = lparen_tag(input)?;
    let (input, args) =
        separated_list0(comma_tag, pair(destructure_parser, type_specifier_parser))(input)?;
    let (input, _) = rparen_tag(input)?;
    let (input, ret_type) = opt(type_specifier_parser)(input)?;
    let (input, block) = block_parser(input)?;

    Ok((
        input,
        MethodDecl {
            method_id,
            args,
            ret_type,
            block,
        },
    ))
}

pub(crate) fn fn_decl_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, FnDecl<'i>> {
    let (input, _) = fn_tag(input)?;
    let (input, method_decl) = method_decl_parser(input)?;

    Ok((input, method_decl.into()))
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{Block, Destructure, FnDecl, MethodDecl, ValueVarType, VarType},
        lexer::Token,
        parser::{
            class_parser::{fn_decl_parser, method_decl_parser},
            helpers::test::span_token_vec,
            token::Tokens,
        },
    };

    #[test]
    fn method_decl_test() {
        let token_iter = span_token_vec(vec![
            Token::Id("myMethod".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = method_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, stmt_end) = res.unwrap();
        assert_eq!(
            stmt_end,
            MethodDecl {
                method_id: "myMethod".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_test() {
        let token_iter = span_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = fn_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, stmt_end) = res.unwrap();
        assert_eq!(
            stmt_end,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_void_test() {
        let token_iter = span_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = fn_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, stmt_end) = res.unwrap();
        assert_eq!(
            stmt_end,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![],
                ret_type: None,
                block: Block { stmts: vec![] }
            }
        )
    }

    #[test]
    fn fn_decl_args_test() {
        let token_iter = span_token_vec(vec![
            Token::Function,
            Token::Id("myFunction".into()),
            Token::LParen,
            Token::Id("x".into()),
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::RParen,
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = fn_decl_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, stmt_end) = res.unwrap();
        assert_eq!(
            stmt_end,
            FnDecl {
                fn_id: "myFunction".into(),
                args: vec![(
                    Destructure::Id("x".into()),
                    ValueVarType {
                        vtype: VarType::I32,
                        is_array: false
                    }
                )],
                ret_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                block: Block { stmts: vec![] }
            }
        )
    }
}
