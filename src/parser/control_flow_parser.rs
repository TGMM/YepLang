use super::{
    expr_parser::{expr_parser, paren_expr_parser},
    main_parser::{block_parser, for_stmt_parser, stmt_end_parser, ParseRes},
    primitive_parser::{
        do_tag, else_tag, for_tag, if_tag, lparen_tag, rparen_tag, stmt_end_tag, while_tag,
    },
    token::Tokens,
};
use crate::ast::{Block, DoWhile, ElseIf, For, If, While};
use nom::{combinator::opt, multi::many0};

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

pub(crate) fn while_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, While<'i>> {
    let (input, _) = while_tag(input)?;
    let (input, while_cond) = paren_expr_parser(input)?;
    let (input, block) = block_parser(input)?;

    Ok((input, While { while_cond, block }))
}

pub(crate) fn do_while_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, DoWhile<'i>> {
    let (input, _) = do_tag(input)?;
    let (input, do_block) = block_parser(input)?;
    let (input, _) = while_tag(input)?;
    let (input, while_cond) = paren_expr_parser(input)?;
    let (input, _) = stmt_end_parser(input)?;

    Ok((
        input,
        DoWhile {
            while_cond,
            do_block,
        },
    ))
}

pub(crate) fn for_parser<'i>(input: Tokens<'i>) -> ParseRes<'i, For<'i>> {
    let (input, _) = for_tag(input)?;
    let (input, _) = lparen_tag(input)?;
    let (input, decl_stmt) = opt(for_stmt_parser)(input)?;
    let (input, _) = stmt_end_tag(input)?;
    let (input, cmp_expr) = opt(expr_parser)(input)?;
    let (input, _) = stmt_end_tag(input)?;
    let (input, postfix_stmt) = opt(for_stmt_parser)(input)?;
    let (input, _) = rparen_tag(input)?;
    let (input, block) = block_parser(input)?;

    Ok((
        input,
        For {
            decl_stmt: decl_stmt.map(|s| Box::new(s)),
            cmp_expr,
            postfix_stmt: postfix_stmt.map(|s| Box::new(s)),
            block,
        },
    ))
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{
            Assignment, BExpr, BOp, Block, Destructure, DoWhile, ElseIf, Expr, For, If,
            NumericLiteral, PrimitiveVal, Stmt, While,
        },
        lexer::Token,
        parser::{
            control_flow_parser::{do_while_parser, for_parser, if_parser, while_parser},
            helpers::test::span_token_vec,
            token::Tokens,
        },
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

    #[test]
    fn while_test() {
        let token_iter = span_token_vec(vec![
            Token::While,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = while_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            While {
                while_cond: Expr::BinaryExpr(
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
                block: Block { stmts: vec![] },
            }
        )
    }

    #[test]
    fn do_while_test() {
        let token_iter = span_token_vec(vec![
            Token::Do,
            Token::LBracket,
            Token::RBracket,
            Token::While,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt),
            Token::IntVal("10"),
            Token::RParen,
            Token::StmtEnd,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = do_while_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            DoWhile {
                do_block: Block { stmts: vec![] },
                while_cond: Expr::BinaryExpr(
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
            }
        )
    }

    // TODO: Change this test to use variable declaration
    #[test]
    fn for_test() {
        // for(; x>10; x=x+1){}
        let token_iter = span_token_vec(vec![
            Token::For,
            Token::LParen,
            Token::StmtEnd,
            Token::Id("x".into()),
            Token::BOp(BOp::Lt),
            Token::IntVal("10"),
            Token::StmtEnd,
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::Id("x".into()),
            Token::BOp(BOp::Add),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);
        let tokens = Tokens::new(&token_iter);

        let res = for_parser(tokens);
        assert!(res.is_ok());

        let (_remaining, primitive_vals) = res.unwrap();
        assert_eq!(
            primitive_vals,
            For {
                decl_stmt: None,
                cmp_expr: Some(Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Lt,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        ))
                    }
                    .into()
                )),
                postfix_stmt: Some(Box::new(Stmt::Assignment(Assignment {
                    destructure: Destructure::Id("x".into()),
                    assigned_expr: Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::Id("x".into()),
                            op: BOp::Add,
                            rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                                None,
                                NumericLiteral::Int("10")
                            ))
                        }
                        .into()
                    )
                }))),
                block: Block { stmts: vec![] }
            }
        )
    }
}
