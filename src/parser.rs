use crate::{
    ast::{
        ArrayVal, Block, Destructure, PrimitiveVal, PropertyName, Stmt, StructVal, VarDecl, VarType,
    },
    lexer::Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

fn int_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::IntVal(i) => i
    }
    .labelled("int value")
    .map(|i| PrimitiveVal::Int(i))
}

fn float_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::FloatVal(f) => f
    }
    .labelled("float value")
    .map(|f| PrimitiveVal::Float(f))
}

fn bool_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::BoolVal(b) => b
    }
    .labelled("boolean value")
    .map(|f| match f {
        "true" => PrimitiveVal::Boolean(true),
        "false" => PrimitiveVal::Boolean(false),
        _ => unreachable!(),
    })
}

fn primitive_val_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    int_parser().or(float_parser()).or(bool_parser())
}

fn array_var_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::LSqBracket)
        .ignore_then(
            primitive_val_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RSqBracket))
        .map(|vals| PrimitiveVal::Array(ArrayVal(vals)))
}

fn struct_var_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, StructVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    fn id_or_str<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, PropertyName, extra::Err<Rich<'a, Token<'a>>>> {
        select! { Token::Id(i) => i }
            .map(|i| PropertyName::Id(i.into()))
            .or(select! { Token::Str(s) => s.trim_matches('"') }
                .map(|p| PropertyName::String(p.to_string())))
    }

    fn property<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, (PropertyName, PrimitiveVal<'a>), extra::Err<Rich<'a, Token<'a>>>> {
        id_or_str()
            .then_ignore(just(Token::Colon))
            .then(primitive_val_parser())
    }

    just(Token::LBracket)
        .ignore_then(
            property()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RBracket))
        .map(|s| StructVal(s))
}

fn type_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, VarType, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::Colon)
        .ignore_then(select! { Token::VarType(t) => t })
        .labelled("type declaration")
}

fn stmt_end_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Token<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::StmtEnd).recover_with(via_parser(empty().to(Token::StmtEnd)))
}

fn var_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, VarDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::ScopeSpecifier(ss) => ss }
        // TODO: This should be a destructure
        .then(select! {
            Token::Id(id) => id
        })
        .then(type_decl_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(primitive_val_parser())
        .then_ignore(stmt_end_parser())
        .map(|(((scope_spec, id), var_type), val)| VarDecl {
            scope_spec,
            destructure: Destructure::Id(id.into()),
            var_type,
            expr: val.into(),
        })
}

fn stmt_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Stmt<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let var_decl = var_decl_parser().map(|vd| Stmt::VarDecl(vd));

    var_decl
}

fn block_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
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
    use super::parse;

    #[test]
    pub fn test() {
        let input = r#"const x = 10; 
        let y = 9223372036854775807;"#;
        parse(input);
    }
}
