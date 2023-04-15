use crate::{
    ast::{ArrayVal, Expr, PrimitiveVal, PropertyName, StructVal},
    lexer::Token,
    parser::main_parser::id_parser,
};
use chumsky::{input::ValueInput, prelude::*};

fn int_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::IntVal(i) => i
    }
    .labelled("int")
    .map(|i| PrimitiveVal::Int(i))
}

fn float_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::FloatVal(f) => f
    }
    .labelled("float")
    .map(|f| PrimitiveVal::Float(f))
}

fn bool_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! {
        Token::BoolVal(b) => b
    }
    .labelled("boolean")
    .map(|f| match f {
        "true" => PrimitiveVal::Boolean(true),
        "false" => PrimitiveVal::Boolean(false),
        _ => unreachable!(),
    })
}

fn string_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::Str(s) => s.trim_matches('"')}
        .labelled("string")
        .map(|s| PrimitiveVal::String(s.to_string()))
}

fn array_var_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::LSqBracket)
        .ignore_then(
            // TODO: This should be an Expr
            primitive_val_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RSqBracket))
        .map(|vals| {
            PrimitiveVal::Array(ArrayVal(
                vals.iter()
                    .map(|pv| Into::<Expr>::into(pv.clone()))
                    .collect(),
            ))
        })
}

fn struct_var_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, StructVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    fn id_or_str<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, PropertyName, extra::Err<Rich<'a, Token<'a>>>> {
        id_parser()
            .map(|i| PropertyName::Id(i.into()))
            .or(string_parser().map(|p| match p {
                PrimitiveVal::String(s) => PropertyName::String(s),
                _ => unreachable!(),
            }))
    }

    fn property<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, (PropertyName, PrimitiveVal<'a>), extra::Err<Rich<'a, Token<'a>>>> {
        id_or_str()
            .then_ignore(just(Token::Colon))
            // TODO: This should be an Expr
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

pub fn primitive_val_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    int_parser().or(float_parser()).or(bool_parser())
}

#[cfg(test)]
mod test {
    use super::{bool_parser, float_parser, int_parser, primitive_val_parser};
    use crate::{ast::PrimitiveVal, lexer::Token};
    use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};

    #[test]
    pub fn int_test() {
        let token_iter = vec![(Token::IntVal("10"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = int_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&PrimitiveVal::Int("10")));
    }

    #[test]
    pub fn float_test() {
        let token_iter = vec![(Token::FloatVal("10.0"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = float_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&PrimitiveVal::Float("10.0")));
    }

    #[test]
    pub fn bool_true_test() {
        let token_iter = vec![(Token::BoolVal("true"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = bool_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&PrimitiveVal::Boolean(true)));
    }

    #[test]
    pub fn bool_false_test() {
        let token_iter = vec![(Token::BoolVal("false"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = bool_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&PrimitiveVal::Boolean(false)));
    }

    #[test]
    pub fn primitive_val_test() {
        // Parse bool
        let token_iter = vec![(Token::BoolVal("false"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = primitive_val_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());
        assert_eq!(res.output(), Some(&PrimitiveVal::Boolean(false)));

        // Parse int
        let token_iter = vec![(Token::IntVal("10"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = primitive_val_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());
        assert_eq!(res.output(), Some(&PrimitiveVal::Int("10")));

        // Parse float
        let token_iter = vec![(Token::FloatVal("10.0"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = primitive_val_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());
        assert_eq!(res.output(), Some(&PrimitiveVal::Float("10.0")));
    }
}
