use super::{
    expr_parser::{expr_parser, paren_expr_parser, EXPR_PARSER, PAREN_EXPR_PARSER},
    main_parser::{block_parser, for_stmt_parser, stmt_end_parser, ParserError, ParserInput},
};
use crate::{
    ast::{Block, ElseIf},
    parser::main_parser::BLOCK_PARSER,
};
use crate::{
    ast::{DoWhile, For, If, While},
    lexer::Token,
};
use chumsky::{primitive::just, IterParser, Parser};

pub fn else_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Block<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let block = BLOCK_PARSER.read().unwrap().clone();

    // Main definition
    let else_block = just(Token::Else).ignore_then(block.clone());

    // Definitions
    if !block.is_defined() {
        block_parser();
    }

    else_block
}

pub fn else_if_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ElseIf<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let block = BLOCK_PARSER.read().unwrap().clone();
    let paren_expr = PAREN_EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let else_if = just(Token::Else)
        .ignore_then(just(Token::If))
        .ignore_then(paren_expr.clone())
        .then(block.clone())
        .map(|(else_expr, else_block)| ElseIf {
            else_expr,
            else_block,
        });

    // Definitions
    if !block.is_defined() {
        block_parser();
    }
    if !paren_expr.is_defined() {
        paren_expr_parser();
    }

    else_if
}

pub fn if_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, If<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let block = BLOCK_PARSER.read().unwrap().clone();
    let paren_expr = PAREN_EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let if_p = just(Token::If)
        .ignore_then(paren_expr.clone())
        .then(block.clone())
        .then(else_if_parser().repeated().collect::<Vec<_>>())
        .then(else_parser().or_not())
        .map(|(((if_expr, if_block), else_if), else_b)| If {
            if_expr,
            if_block,
            else_if,
            else_b,
        });

    // Definitions
    if !block.is_defined() {
        block_parser();
    }
    if !paren_expr.is_defined() {
        paren_expr_parser();
    }

    if_p
}

pub fn while_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, While<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let block = BLOCK_PARSER.read().unwrap().clone();
    let paren_expr = PAREN_EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let while_p = just(Token::While)
        .ignore_then(paren_expr.clone())
        .then(block.clone())
        .map(|(while_cond, block)| While { while_cond, block });

    // Definitions
    if !block.is_defined() {
        block_parser();
    }
    if !paren_expr.is_defined() {
        paren_expr_parser();
    }

    while_p
}

pub fn do_while_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, DoWhile<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let block = BLOCK_PARSER.read().unwrap().clone();
    let paren_expr = PAREN_EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let do_while = just(Token::Do)
        .ignore_then(block.clone())
        .then_ignore(just(Token::While))
        .then(paren_expr.clone())
        .map(|(do_block, while_cond)| DoWhile {
            do_block,
            while_cond,
        });

    // Definitions
    if !block.is_defined() {
        block_parser();
    }
    if !paren_expr.is_defined() {
        paren_expr_parser();
    }

    do_while
}

pub fn for_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, For<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();
    let block = BLOCK_PARSER.read().unwrap().clone();

    // Main definition
    let for_p = just(Token::For)
        .ignore_then(just(Token::LParen))
        .ignore_then(for_stmt_parser().map(Box::new).or_not())
        .then_ignore(stmt_end_parser())
        .then(expr.clone().or_not())
        .then_ignore(stmt_end_parser())
        .then(for_stmt_parser().map(Box::new).or_not())
        .then_ignore(just(Token::RParen))
        .then(block.clone())
        .map(|(((decl_stmt, cmp_expr), postfix_stmt), block)| For {
            decl_stmt,
            cmp_expr,
            postfix_stmt,
            block,
        });

    // Definitions
    if !block.is_defined() {
        block_parser();
    }
    if !expr.is_defined() {
        expr_parser();
    }

    for_p
}

#[cfg(test)]
mod test {
    use chumsky::{span::SimpleSpan, Parser};

    use crate::{
        ast::{
            Assignment, BExpr, BOp, Block, Destructure, DoWhile, ElseIf, Expr, For, If,
            NumericLiteral, PrimitiveVal, ScopeSpecifier, Stmt, VarDecl, VarDeclAssignment, While,
        },
        lexer::Token,
        parser::{
            control_flow_parser::{do_while_parser, for_parser, if_parser, while_parser},
            helpers::test::{stream_token_vec, IntoSpanned},
        },
    };

    #[test]
    fn if_test() {
        let tokens = stream_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);

        let parse_res = if_parser().parse(tokens);
        for err in parse_res.errors() {
            dbg!(err);
        }
        let res = parse_res.into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                ),
                if_block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                },
                else_if: vec![],
                else_b: None
            }
        )
    }

    #[test]
    fn if_else_test() {
        let tokens = stream_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = if_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                ),
                if_block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                },
                else_if: vec![],
                else_b: Some(Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                })
            }
        )
    }

    #[test]
    fn if_else_if_test() {
        let tokens = stream_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gte.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = if_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                ),
                if_block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                },
                else_if: vec![ElseIf {
                    else_expr: Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::Id("x".into()),
                            op: BOp::Gte.into_spanned(),
                            rhs: Expr::PrimitiveVal(
                                PrimitiveVal::Number(None, NumericLiteral::Int("10"))
                                    .into_spanned()
                            )
                        }
                        .into()
                    ),
                    else_block: Block {
                        stmts: vec![],
                        lbracket: Some(SimpleSpan::new(0, 0)),
                        rbracket: Some(SimpleSpan::new(0, 0))
                    }
                }],
                else_b: None
            }
        )
    }

    #[test]
    fn if_else_if_else_test() {
        let tokens = stream_token_vec(vec![
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gte.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Else,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = if_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            If {
                if_expr: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                ),
                if_block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                },
                else_if: vec![ElseIf {
                    else_expr: Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::Id("x".into()),
                            op: BOp::Gte.into_spanned(),
                            rhs: Expr::PrimitiveVal(
                                PrimitiveVal::Number(None, NumericLiteral::Int("10"))
                                    .into_spanned()
                            )
                        }
                        .into()
                    ),
                    else_block: Block {
                        stmts: vec![],
                        lbracket: Some(SimpleSpan::new(0, 0)),
                        rbracket: Some(SimpleSpan::new(0, 0))
                    }
                }],
                else_b: Some(Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                })
            }
        )
    }

    #[test]
    fn while_test() {
        let tokens = stream_token_vec(vec![
            Token::While,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = while_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            While {
                while_cond: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                ),
                block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                },
            }
        )
    }

    #[test]
    fn do_while_test() {
        let tokens = stream_token_vec(vec![
            Token::Do,
            Token::LBracket,
            Token::RBracket,
            Token::While,
            Token::LParen,
            Token::Id("x".into()),
            Token::BOp(BOp::Gt.into_spanned()),
            Token::IntVal("10"),
            Token::RParen,
        ]);

        let res = do_while_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            DoWhile {
                do_block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                },
                while_cond: Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Gt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                ),
            }
        )
    }

    #[test]
    fn for_test() {
        // for(; x>10; x=x+1){}
        let tokens = stream_token_vec(vec![
            Token::For,
            Token::LParen,
            Token::ScopeSpecifier(ScopeSpecifier::Let),
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::IntVal("0"),
            Token::StmtEnd,
            Token::Id("x".into()),
            Token::BOp(BOp::Lt.into_spanned()),
            Token::IntVal("10"),
            Token::StmtEnd,
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::Id("x".into()),
            Token::BOp(BOp::Add.into_spanned()),
            Token::IntVal("1"),
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
        ]);

        let res = for_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let primitive_vals = res.unwrap();
        assert_eq!(
            primitive_vals,
            For {
                decl_stmt: Some(
                    Stmt::VarDecl(VarDecl {
                        scope_spec: ScopeSpecifier::Let,
                        decl_assignments: vec![VarDeclAssignment {
                            destructure: Destructure::Id("x".into()),
                            var_type: None,
                            expr: Some(Expr::PrimitiveVal(
                                PrimitiveVal::Number(None, NumericLiteral::Int("0")).into_spanned()
                            ))
                        }]
                    })
                    .into()
                ),
                cmp_expr: Some(Expr::BinaryExpr(
                    BExpr {
                        lhs: Expr::Id("x".into()),
                        op: BOp::Lt.into_spanned(),
                        rhs: Expr::PrimitiveVal(
                            PrimitiveVal::Number(None, NumericLiteral::Int("10")).into_spanned()
                        )
                    }
                    .into()
                )),
                postfix_stmt: Some(Box::new(Stmt::Assignment(Assignment {
                    assignee_expr: Expr::Id("x".into()),
                    bop: None,
                    assigned_expr: Expr::BinaryExpr(
                        BExpr {
                            lhs: Expr::Id("x".into()),
                            op: BOp::Add.into_spanned(),
                            rhs: Expr::PrimitiveVal(
                                PrimitiveVal::Number(None, NumericLiteral::Int("1")).into_spanned()
                            )
                        }
                        .into()
                    )
                }))),
                block: Block {
                    stmts: vec![],
                    lbracket: Some(SimpleSpan::new(0, 0)),
                    rbracket: Some(SimpleSpan::new(0, 0))
                }
            }
        )
    }
}
