use super::expr_parser::{boolean_unary_op_parser, numeric_unary_op_parser};
use super::main_parser::ParseRes;
use super::token::Tokens;
use crate::ast::{
    ArrayVal, BOp, BoolLiteral, Id, NumericLiteral, PrimitiveVal, PropertyName, ScopeSpecifier,
    StructVal, ValueVarType, VarType,
};
use crate::lexer::Token;
use crate::parser::expr_parser::expr_parser;
use crate::tag_token;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::Err;
use nom::IResult;
use snailquote::unescape;

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
tag_token!(if_tag, Ok(Token::If));
tag_token!(else_tag, Ok(Token::Else));
tag_token!(while_tag, Ok(Token::While));
tag_token!(do_tag, Ok(Token::Do));
tag_token!(for_tag, Ok(Token::For));
tag_token!(fn_tag, Ok(Token::Function));
tag_token!(class_tag, Ok(Token::Class));
tag_token!(extends_tag, Ok(Token::Extends));
tag_token!(dot_tag, Ok(Token::Dot));

pub(crate) fn bop_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, BOp> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::BOp(bop)) => Ok((remaining, bop)),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn id_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Id> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
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
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::IntVal(val)) => Ok((remaining, NumericLiteral::Int(val))),
            Ok(Token::FloatVal(val)) => Ok((remaining, NumericLiteral::Float(val))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn signed_number_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, PrimitiveVal<'i>> {
    map(
        pair(opt(numeric_unary_op_parser), number_parser),
        |(op, num)| PrimitiveVal::Number(op, num),
    )(input)
}

pub(crate) fn string_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, String> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::Str(string)) => {
                let unescaped = unescape(string).expect("Invalid string");
                Ok((remaining, unescaped))
            }
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn bool_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, BoolLiteral> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::BoolVal(b)) => Ok((remaining, b.into())),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn negated_bool_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, PrimitiveVal<'i>> {
    map(
        pair(opt(boolean_unary_op_parser), bool_parser),
        |(op, num)| PrimitiveVal::Boolean(op, num),
    )(input)
}

pub(crate) fn char_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, char> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::Char(c)) => {
                // TODO: Find if there's a better way to do this
                let c_hack = format!(
                    "\"{}\"",
                    c.strip_suffix("'").unwrap().strip_prefix("'").unwrap()
                );
                let unescaped_ch = unescape(&c_hack).expect("Invalid escaped char");
                let c = unescaped_ch
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

pub(crate) fn primitive_val_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, PrimitiveVal<'i>> {
    let num = signed_number_parser;
    let bool = negated_bool_parser;
    let char = map(char_parser, |c| PrimitiveVal::Char(c));
    // TODO: Check if we can make this zero-copy
    let string = map(string_parser, |s| PrimitiveVal::String(s.to_string()));
    let arr = map(array_val_parser, |a| PrimitiveVal::Array(a));
    let struct_v = map(struct_val_parser, |s| PrimitiveVal::Struct(s));

    alt((num, bool, char, string, arr, struct_v))(input)
}

pub(crate) fn scope_specifier_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ScopeSpecifier> {
    let (remaining, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::ScopeSpecifier(ss)) => Ok((remaining, ss)),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn var_type_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, VarType> {
    let (input, tok_id) = take(1usize)(input)?;

    if tok_id.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        match tok_id.tok_span[0].token.clone() {
            Ok(Token::VarType(ss)) => Ok((input, ss)),
            Ok(Token::Id(id)) => Ok((input, VarType::Custom(id.into()))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub(crate) fn value_var_type_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ValueVarType> {
    let (input, vtype) = var_type_parser(input)?;
    let (input, is_array) = opt(pair(lsqbracket_tag, rsqbracket_tag))(input)?;

    Ok((
        input,
        ValueVarType {
            vtype,
            is_array: is_array.is_some(),
        },
    ))
}

pub(crate) fn type_specifier_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ValueVarType> {
    preceded(colon_tag, value_var_type_parser)(input)
}

#[cfg(test)]
mod test {
    use nom::multi::many1;

    use crate::{
        ast::{
            ArrayVal, BExpr, BOp, BoolLiteral, Expr, Id, NumericLiteral, NumericUnaryOp,
            PrimitiveVal, PropertyName, ScopeSpecifier, StructVal, ValueVarType, VarType,
        },
        lexer::Token,
        parser::{
            helpers::test::span_token_vec,
            primitive_parser::{
                array_val_parser, bool_parser, char_parser, id_parser, number_parser,
                primitive_val_parser, scope_specifier_parser, signed_number_parser, string_parser,
                struct_val_parser, type_specifier_parser,
            },
            token::Tokens,
        },
    };

    #[test]
    fn id_test() {
        let token_iter = span_token_vec(vec![Token::Id("アイヂ")]);
        let tokens = Tokens::new(&token_iter);

        let res = id_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, id) = res.unwrap();
        assert_eq!(id, Id("アイヂ".to_string()))
    }

    #[test]
    fn int_test() {
        let token_iter = span_token_vec(vec![Token::IntVal("10")]);
        let tokens = Tokens::new(&token_iter);

        let res = number_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, int_val) = res.unwrap();
        assert_eq!(int_val, NumericLiteral::Int("10"))
    }

    #[test]
    fn float_test() {
        let token_iter = span_token_vec(vec![Token::FloatVal("10.0")]);
        let tokens = Tokens::new(&token_iter);

        let res = number_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, float_val) = res.unwrap();
        assert_eq!(float_val, NumericLiteral::Float("10.0"))
    }

    #[test]
    fn float_scientific_notation_test() {
        let token_iter = span_token_vec(vec![Token::FloatVal("1e+10")]);
        let tokens = Tokens::new(&token_iter);

        let res = number_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, float_val) = res.unwrap();
        assert_eq!(float_val, NumericLiteral::Float("1e+10"))
    }

    #[test]
    fn signed_number_test() {
        let token_iter = span_token_vec(vec![Token::BOp(BOp::Add), Token::IntVal("10")]);
        let tokens = Tokens::new(&token_iter);

        let res = signed_number_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_val) = res.unwrap();
        assert_eq!(
            primitive_val,
            PrimitiveVal::Number(Some(NumericUnaryOp::Plus), NumericLiteral::Int("10"))
        )
    }

    #[test]
    fn string_test() {
        let token_iter = span_token_vec(vec![Token::Str("\"This is a string\"")]);
        let tokens = Tokens::new(&token_iter);

        let res = string_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, str_val) = res.unwrap();
        assert_eq!(str_val, "This is a string")
    }

    #[test]
    fn string_unicode_test() {
        let token_iter = span_token_vec(vec![Token::Str("\"Japanese: アイヂ and Emoji: 😀\"")]);
        let tokens = Tokens::new(&token_iter);

        let res = string_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, str_val) = res.unwrap();
        assert_eq!(str_val, "Japanese: アイヂ and Emoji: 😀")
    }

    #[test]
    fn bool_test() {
        let token_iter = span_token_vec(vec![Token::BoolVal("true")]);
        let tokens = Tokens::new(&token_iter);

        let res = bool_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, bool_val) = res.unwrap();
        assert_eq!(bool_val, BoolLiteral(true))
    }

    #[test]
    fn char_test() {
        let token_iter = span_token_vec(vec![Token::Char("'\''")]);
        let tokens = Tokens::new(&token_iter);

        let res = char_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, char_val) = res.unwrap();
        assert_eq!(char_val, '\'')
    }

    #[test]
    fn array_test() {
        let token_iter = span_token_vec(vec![
            Token::LSqBracket,
            Token::Id("x"),
            Token::Comma,
            Token::IntVal("10"),
            Token::BOp(BOp::Add),
            Token::IntVal("10"),
            Token::RSqBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = array_val_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, arr_val) = res.unwrap();
        assert_eq!(
            arr_val,
            ArrayVal(vec![
                Expr::Id("x".into()),
                Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        )),
                        op: BOp::Add,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))
                    }
                    .into()
                )
            ])
        )
    }

    #[test]
    fn struct_test() {
        let token_iter = span_token_vec(vec![
            Token::LBracket,
            Token::Id("x".into()),
            Token::Colon,
            Token::IntVal("10"),
            Token::Comma,
            Token::Str("this is a str id".into()),
            Token::Colon,
            Token::FloatVal("10"),
            Token::BOp(BOp::Add),
            Token::FloatVal("10"),
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = struct_val_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, bool_val) = res.unwrap();
        assert_eq!(
            bool_val,
            StructVal(vec![
                (
                    PropertyName::Id("x".into()),
                    Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10")))
                ),
                (
                    PropertyName::String("this is a str id".to_string()),
                    Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Float("10")
                            )),
                            op: BOp::Add,
                            rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Float("10")
                            ))
                        }
                        .into()
                    )
                )
            ])
        )
    }

    #[test]
    fn primitive_val_test() {
        let token_iter = span_token_vec(vec![
            Token::BoolVal("true"),
            Token::Char("'a'"),
            Token::Str("\"String!\""),
            Token::LSqBracket,
            Token::RSqBracket,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = many1(primitive_val_parser)(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            vec![
                PrimitiveVal::Boolean(None, BoolLiteral(true)),
                PrimitiveVal::Char('a'),
                PrimitiveVal::String("String!".to_string()),
                PrimitiveVal::Array(ArrayVal(vec![])),
                PrimitiveVal::Struct(StructVal(vec![])),
            ]
        )
    }

    #[test]
    fn scope_spec_test() {
        let token_iter = span_token_vec(vec![
            Token::ScopeSpecifier(ScopeSpecifier::Const),
            Token::ScopeSpecifier(ScopeSpecifier::Let),
            Token::ScopeSpecifier(ScopeSpecifier::Var),
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = many1(scope_specifier_parser)(tokens);
        assert!(res.is_ok());

        let (_remaining, scope_spec) = res.unwrap();
        assert_eq!(
            scope_spec,
            vec![
                ScopeSpecifier::Const,
                ScopeSpecifier::Let,
                ScopeSpecifier::Var,
            ]
        )
    }

    #[test]
    fn type_spec_test() {
        let token_iter = span_token_vec(vec![Token::Colon, Token::VarType(VarType::I32)]);
        let tokens = Tokens::new(&token_iter);

        let res = type_specifier_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, var_type) = res.unwrap();
        assert_eq!(
            var_type,
            ValueVarType {
                vtype: VarType::I32,
                is_array: false
            }
        )
    }

    #[test]
    fn type_spec_arr_test() {
        let token_iter = span_token_vec(vec![
            Token::Colon,
            Token::VarType(VarType::I32),
            Token::LSqBracket,
            Token::RSqBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = type_specifier_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, var_type) = res.unwrap();
        assert_eq!(
            var_type,
            ValueVarType {
                vtype: VarType::I32,
                is_array: true
            }
        )
    }

    #[test]
    fn type_spec_custom_test() {
        let token_iter = span_token_vec(vec![Token::Colon, Token::Id("MyClass".into())]);
        let tokens = Tokens::new(&token_iter);

        let res = type_specifier_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, var_type) = res.unwrap();
        assert_eq!(
            var_type,
            ValueVarType {
                vtype: VarType::Custom("MyClass".into()),
                is_array: false
            }
        )
    }
}
