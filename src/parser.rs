use crate::{
    ast::{Block, Destructure, PrimitiveVal, Stmt, VarDecl},
    lexer::Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::str::FromStr;

fn stmt_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Stmt, extra::Err<Rich<'a, Token<'a>>>> {
    let var_decl = select! { Token::ScopeSpecifier(ss) => ss }
        .then(select! {
            Token::Id(id) => id
        })
        .then_ignore(just(Token::AssignmentEq))
        .then(
            select! {
                Token::IntVal(i) => i
            }
            .labelled("value"),
        )
        .then_ignore(just(Token::StmtEnd))
        .map(|((scope_spec, id), val)| VarDecl {
            scope_spec,
            destructure: Destructure::Id(id.into()),
            var_type: None,
            expr: PrimitiveVal::I64(i64::from_str(val).unwrap()).into(),
        })
        .map(|vd| Stmt::VarDecl(vd));

    var_decl
}

fn block_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Block, extra::Err<Rich<'a, Token<'a>>>> {
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
        Err(errs) => errs.into_iter().for_each(|e| {
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
        }),
    }
}

#[cfg(test)]
mod test {
    use super::parse;

    #[test]
    pub fn test() {
        let input = r#"const x = 10; 
        let y = 20;"#;
        parse(input);
    }
}
