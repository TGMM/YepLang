use crate::ast::{
    Assignment, PropertyDestructure, PropertyDestructureName, PropertyName, ValueVarType,
};
use crate::parser::value_parser::primitive_val_parser;
use crate::{
    ast::{Block, Destructure, Id, Stmt, VarDecl, VarType},
    lexer::Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

use super::expr_parser::expr_parser;
use super::value_parser::string_parser;

pub fn id_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Id, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    select! {
        Token::Id(id) => id
    }
    .labelled("id")
    .map(|id| Id(id.to_string()))
}

pub fn destructure_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Destructure, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    recursive(|destructure| {
        let id = id_parser().map(|id| Destructure::Id(id));
        let array = destructure
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
            .map(|destructures| Destructure::Array(destructures));

        let str_prop_name = string_parser()
            .map(|s| PropertyName::String(s.to_string()))
            .map(|pn| PropertyDestructureName::PropName(pn));
        let id_prop_name = id_parser()
            .map(|id| PropertyName::Id(id))
            .map(|pn| PropertyDestructureName::PropName(pn));
        let destructure_prop_name = destructure.map(|d| PropertyDestructureName::Destructure(d));
        let alias = just(Token::Colon).ignore_then(id_parser());
        let property = str_prop_name
            .or(id_prop_name)
            .or(destructure_prop_name)
            .then(alias.or_not())
            .map(|(name, alias)| PropertyDestructure { name, alias });

        let object = property
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|o| Destructure::Object(o));

        id.or(array).or(object)
    })
}

pub fn destructure_assignment_parser<
    'a,
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
>() -> impl Parser<'a, I, Assignment<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    destructure_parser()
        .then_ignore(just(Token::AssignmentEq))
        .then(expr_parser())
        .map(|(destructure, assigned_expr)| Assignment {
            destructure,
            assigned_expr,
        })
}

pub fn type_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, ValueVarType, extra::Err<Rich<'a, Token<'a>>>> {
    let arr_type_decl = just(Token::LSqBracket).ignore_then(just(Token::RSqBracket).ignored());

    just(Token::Colon)
        .ignore_then(
            select! { Token::VarType(t) => t }.or(id_parser().map(|id| VarType::Custom(id))),
        )
        .then(arr_type_decl.or_not())
        .labelled("type declaration")
        .map(|(vtype, arr_decl)| ValueVarType {
            vtype,
            is_array: arr_decl.is_some(),
        })
}

fn stmt_end_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Token<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::StmtEnd).recover_with(via_parser(empty().to(Token::StmtEnd)))
}

fn var_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, VarDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::ScopeSpecifier(ss) => ss }
        .then(destructure_parser())
        .then(type_decl_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(primitive_val_parser())
        .then_ignore(stmt_end_parser())
        .map(|(((scope_spec, destruct), var_type), val)| VarDecl {
            scope_spec,
            destructure: destruct,
            var_type,
            expr: val.into(),
        })
}

pub fn stmt_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Stmt<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let var_decl = var_decl_parser().map(|vd| Stmt::VarDecl(vd));

    var_decl
}

pub fn block_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Block<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    top_block_parser().delimited_by(just(Token::LBracket), just(Token::RBracket))
}

pub fn top_block_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Block<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    recursive(|block| {
        let block_stmt = just(Token::LBracket)
            .ignore_then(block)
            .then_ignore(just(Token::RBracket))
            .map(|b| Stmt::Block(b));

        block_stmt
            .or(stmt_parser().boxed())
            .repeated()
            .collect::<Vec<Stmt>>()
            .map(|stmts| Block { stmts })
    })
}

pub fn parse(input: &str) {
    let token_iter = Token::lexer(input)
        .spanned()
        .map::<(Token, SimpleSpan), _>(|(tok, span)| (tok, span.into()));

    let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

    match block_parser().parse(token_stream).into_result() {
        Ok(block) => {
            dbg!(block);
        }
        Err(errs) => {
            dbg!(&errs);

            errs.into_iter().for_each(|e| {
                Report::build(ReportKind::Error, "test.file", e.span().start)
                    .with_code(3)
                    .with_message(e.to_string())
                    .with_label(
                        Label::new(("test.file", e.span().into_range()))
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(("test.file", Source::from(input)))
                    .unwrap()
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::{stmt_end_parser, type_decl_parser, var_decl_parser};
    use crate::{
        ast::{
            Destructure, NumericLiteral, PrimitiveVal, ScopeSpecifier, ValueVarType, VarDecl,
            VarType,
        },
        lexer::Token,
    };
    use chumsky::{
        error::RichPattern, input::Stream, prelude::Input, span::SimpleSpan, util::Maybe, Parser,
    };

    #[test]
    pub fn type_decl_test() {
        let token_iter = vec![
            (Token::Colon, SimpleSpan::new(1usize, 1usize)),
            (
                Token::VarType(VarType::I32),
                SimpleSpan::new(2usize, 2usize),
            ),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = type_decl_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&ValueVarType {
                vtype: VarType::I32,
                is_array: false
            })
        );
    }

    #[test]
    pub fn stmt_end_test() {
        let token_iter = vec![(Token::StmtEnd, SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = stmt_end_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&Token::StmtEnd));
    }

    #[test]
    pub fn stmt_end_recovery_test() {
        let token_iter: Vec<(Token, SimpleSpan)> = vec![];
        let token_stream = Stream::from_iter(token_iter).spanned((0..0).into());
        let res = stmt_end_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(res.has_errors());

        assert_eq!(res.output(), Some(&Token::StmtEnd));

        let err = res.errors().next().unwrap();
        // Expected ;
        assert_eq!(
            err.expected().next(),
            Some(&RichPattern::Token(Maybe::Val(Token::StmtEnd)))
        );
        // Found end of input
        assert_eq!(err.reason().found(), None);
    }

    #[test]
    pub fn var_decl_test() {
        // const x: i32 = 10;
        let token_iter = vec![
            (
                Token::ScopeSpecifier(ScopeSpecifier::Const),
                SimpleSpan::new(1usize, 1usize),
            ),
            (Token::Id("x".into()), SimpleSpan::new(1usize, 1usize)),
            (Token::Colon, SimpleSpan::new(1usize, 1usize)),
            (
                Token::VarType(VarType::I32),
                SimpleSpan::new(1usize, 1usize),
            ),
            (Token::AssignmentEq, SimpleSpan::new(1usize, 1usize)),
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
            (Token::StmtEnd, SimpleSpan::new(1usize, 1usize)),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = var_decl_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&VarDecl {
                scope_spec: ScopeSpecifier::Const,
                destructure: Destructure::Id("x".into()),
                var_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                expr: PrimitiveVal::Number(None, NumericLiteral::Int("10")).into()
            })
        );
    }
}
