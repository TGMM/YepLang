use crate::{
    ast::{Block, Destructure, PrimitiveVal, Stmt, VarDecl, VarType},
    lexer::Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::str::FromStr;

fn int_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::IntVal(i) => i
    }
    .labelled("int value")
    .try_map(|str_num, span| i64::from_str(str_num).map_err(|e| Rich::custom(span, e)))
    .map(|i| PrimitiveVal::I64(i))
}

fn float_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::FloatVal(f) => f
    }
    .labelled("int value")
    .try_map(|str_num, span| f64::from_str(str_num).map_err(|e| Rich::custom(span, e)))
    .map(|i| PrimitiveVal::F64(i))
}

fn primitive_val_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal, extra::Err<Rich<'a, Token<'a>>>> {
    int_parser().or(float_parser())
}

fn type_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, VarType, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::Colon)
        .ignore_then(select! { Token::VarType(t) => t })
        .labelled("type declaration")
}

fn stmt_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Stmt, extra::Err<Rich<'a, Token<'a>>>> {
    let var_decl = select! { Token::ScopeSpecifier(ss) => ss }
        // TODO: This should be a destructure
        .then(select! {
            Token::Id(id) => id
        })
        .then(type_decl_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(primitive_val_parser())
        .then_ignore(just(Token::StmtEnd))
        .map(|(((scope_spec, id), var_type), val)| VarDecl {
            scope_spec,
            destructure: Destructure::Id(id.into()),
            var_type,
            expr: val.into(),
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
        let y = 9223372036854775807;"#;
        parse(input);
    }
}
