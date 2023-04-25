use nom::{combinator::opt, multi::many0};

use super::{
    expr_parser::paren_expr_parser,
    main_parser::{block_parser, ParseRes},
    primitive_parser::{else_tag, if_tag},
    token::Tokens,
};
use crate::ast::{Block, ElseIf, If};

pub(crate) fn else_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, Block<'i>> {
    let (input, _) = else_tag(input)?;
    // Else block
    block_parser(input)
}

pub(crate) fn else_if_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, ElseIf<'i>> {
    let (input, _) = else_tag(input)?;
    let (input, _) = if_tag(input)?;

    let (input, paren_expr) = paren_expr_parser(input)?;
    let (input, block) = block_parser(input)?;

    Ok((
        input,
        ElseIf {
            else_expr: paren_expr,
            else_block: block,
        },
    ))
}

pub(crate) fn if_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, If<'i>> {
    let (input, _) = if_tag(input)?;
    let (input, if_expr) = paren_expr_parser(input)?;
    let (input, if_block) = block_parser(input)?;
    let (input, else_if) = many0(else_if_parser)(input)?;
    let (input, else_b) = opt(else_parser)(input)?;

    Ok((
        input,
        If {
            if_expr,
            if_block,
            else_b,
            else_if,
        },
    ))
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{BExpr, BOp, Block, ElseIf, Expr, If, NumericLiteral, PrimitiveVal},
        lexer::Token,
        parser::{control_flow_parser::if_parser, helpers::test::span_token_vec, token::Tokens},
    };

    #[test]
    fn if_test() {
        let token_iter = span_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = if_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))
                    }
                    .into()
                ),
                if_block: Block { stmts: vec![] },
                else_if: vec![],
                else_b: None
            }
        )
    }

    #[test]
    fn if_else_test() {
        let token_iter = span_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = if_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))
                    }
                    .into()
                ),
                if_block: Block { stmts: vec![] },
                else_if: vec![],
                else_b: Some(Block { stmts: vec![] })
            }
        )
    }

    #[test]
    fn if_else_if_test() {
        let token_iter = span_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gte),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = if_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))
                    }
                    .into()
                ),
                if_block: Block { stmts: vec![] },
                else_if: vec![ElseIf {
                    else_expr: Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::Id("x".into()),
                            op: BOp::Gte,
                            rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Int("10")
                            ))
                        }
                        .into()
                    ),
                    else_block: Block { stmts: vec![] }
                }],
                else_b: None
            }
        )
    }

    #[test]
    fn if_else_if_else_test() {
        let token_iter = span_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gte),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = if_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))
                    }
                    .into()
                ),
                if_block: Block { stmts: vec![] },
                else_if: vec![ElseIf {
                    else_expr: Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::Id("x".into()),
                            op: BOp::Gte,
                            rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Int("10")
                            ))
                        }
                        .into()
                    ),
                    else_block: Block { stmts: vec![] }
                }],
                else_b: Some(Block { stmts: vec![] })
            }
        )
    }
}
