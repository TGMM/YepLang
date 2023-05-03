use crate::ast::{BOp, BoolUnaryOp, FnCall, Id, Indexing, MemberAcess, NumericUnaryOp};
use crate::lexer::Token;
use crate::parser::main_parser::{GlobalParser, RecursiveParser};
use crate::{ast::Expr, recursive_parser};
use chumsky::primitive::{just, todo};
use chumsky::recursive::Recursive;
use chumsky::{select, IterParser, Parser};
use std::sync::{Arc, LazyLock, RwLock};

use super::main_parser::{ParserError, ParserInput};
use super::primitive_parser::{bop_parser, id_parser, primitive_val_parser};

recursive_parser!(
    EXPR_PARSER,
    expr_parser,
    Expr<'static>,
    declarations {},
    main_definition {
        todo()
    },
    definitions {}
);

pub fn numeric_unary_op_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, NumericUnaryOp, ParserError<'i, Token<'i>>> + Clone {
    let plus = bop_parser()
        .filter(|b| matches!(b, BOp::Add))
        .to(NumericUnaryOp::Plus);
    let minus = bop_parser()
        .filter(|b| matches!(b, BOp::Sub))
        .to(NumericUnaryOp::Minus);

    plus.or(minus)
}

pub fn boolean_unary_op_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, BoolUnaryOp, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::BoolUnaryOp(buop) => buop }
}

pub fn indexer_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Expr<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let indexer = expr.delimited_by(just(Token::LSqBracket), just(Token::LSqBracket));

    // Definitions
    expr_parser();

    indexer
}

pub fn fn_call_args_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Vec<Expr<'i>>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let fn_call = expr
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen));

    // Definitions
    expr_parser();

    fn_call
}

pub fn member_access_prop_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, Id, ParserError<'i, Token<'i>>> + Clone {
    just(Token::Dot).ignore_then(id_parser())
}

enum PExprSuffix<'i> {
    Index(Expr<'i>),
    FnCall(Vec<Expr<'i>>),
    MemberAccess(Id),
}

pub fn primary_expr_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Expr<'i>, ParserError<'i, Token<'i>>> + Clone {
    let id = id_parser().map(Expr::Id);
    let primitive = primitive_val_parser().map(Expr::PrimitiveVal);
    let paren_expr = todo();

    let primary_expr = id.or(primitive).or(paren_expr);

    let indexed_suffix = indexer_parser().map(PExprSuffix::Index);
    let fn_call_suffix = fn_call_args_parser().map(PExprSuffix::FnCall);
    let member_access_suffix = member_access_prop_parser().map(PExprSuffix::MemberAccess);
    let pexpr_suffix = indexed_suffix
        .or(fn_call_suffix)
        .or(member_access_suffix)
        .repeated();

    let suffixed_pexpr = primary_expr.foldl(pexpr_suffix, |pexpr, suffix| match suffix {
        PExprSuffix::Index(indexer) => Expr::Indexing(Box::new(Indexing {
            indexed: pexpr,
            indexer,
        })),
        PExprSuffix::FnCall(args) => Expr::FnCall(Box::new(FnCall {
            fn_expr: pexpr,
            args,
        })),
        PExprSuffix::MemberAccess(property) => Expr::MemberAccess(Box::new(MemberAcess {
            accessed: pexpr,
            property,
        })),
    });

    suffixed_pexpr
}
