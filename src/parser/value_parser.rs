use super::expr_parser::expr_parser;
use crate::{
    ast::{
        ArrayVal, BoolLiteral, Expr, FnCall, Indexing, NumericLiteral, NumericUnaryOp,
        PrimitiveVal, PropertyName, StructVal,
    },
    lexer::Token,
    parser::main_parser::id_parser,
};
use chumsky::{input::ValueInput, prelude::*};

fn int_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    select! { Token::ExpOp(op) => op }
        .map(|op| Into::<NumericUnaryOp>::into(op))
        .or_not()
        .then(select! {
            Token::IntVal(i) => i
        })
        .labelled("int")
        .map(|(uop, i)| PrimitiveVal::Number(uop, NumericLiteral::Int(i)))
}

fn float_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    select! { Token::ExpOp(op) => op }
        .map(|op| Into::<NumericUnaryOp>::into(op))
        .or_not()
        .then(select! {
            Token::FloatVal(f) => f
        })
        .labelled("float")
        .map(|(uop, f)| PrimitiveVal::Number(uop, NumericLiteral::Float(f)))
}

fn bool_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    select! { Token::BoolUnaryOp(op) => op }
        .or_not()
        .then(select! {
            Token::BoolVal(b) => b
        })
        .labelled("boolean")
        .map(|(uop, b)| match b {
            "true" => PrimitiveVal::Boolean(uop, BoolLiteral(true)),
            "false" => PrimitiveVal::Boolean(uop, BoolLiteral(false)),
            _ => unreachable!(),
        })
}

pub fn string_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, &'a str, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    select! { Token::Str(s) => s.trim_matches('"')}.labelled("string")
}

pub fn string_val_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    string_parser().map(|s| PrimitiveVal::String(s.to_string()))
}

fn array_var_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    just(Token::LSqBracket)
        .ignore_then(
            expr_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RSqBracket))
        .map(|vals| PrimitiveVal::Array(ArrayVal(vals.into_iter().collect::<Vec<_>>())))
}

fn struct_var_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, StructVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    fn id_or_str<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, PropertyName, extra::Err<Rich<'a, Token<'a>>>> + Clone {
        id_parser()
            .map(|i| PropertyName::Id(i.into()))
            .or(string_val_parser().map(|p| match p {
                PrimitiveVal::String(s) => PropertyName::String(s),
                _ => unreachable!(),
            }))
    }

    fn property<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, (PropertyName, Expr<'a>), extra::Err<Rich<'a, Token<'a>>>> + Clone {
        id_or_str()
            .then_ignore(just(Token::Colon))
            .then(expr_parser())
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
) -> impl Parser<'a, I, PrimitiveVal<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    int_parser().or(float_parser()).or(bool_parser())
}

pub fn indexing_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Indexing<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    expr_parser()
        .then_ignore(just(Token::LSqBracket))
        .then(expr_parser())
        .then_ignore(just(Token::RSqBracket))
        .map(|(indexed, indexer)| Indexing { indexed, indexer })
}

pub fn fn_call_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, FnCall<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    expr_parser()
        .then_ignore(just(Token::LParen))
        .then(
            expr_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RParen))
        .map(|(fn_expr, args)| FnCall { fn_expr, args })
}

#[cfg(test)]
mod test {
    use super::{bool_parser, float_parser, int_parser, primitive_val_parser};
    use crate::{
        ast::{BoolLiteral, NumericLiteral, PrimitiveVal},
        lexer::Token,
    };
    use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};

    #[test]
    pub fn int_test() {
        let token_iter = vec![(Token::IntVal("10"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = int_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Number(None, NumericLiteral::Int("10")))
        );
    }

    #[test]
    pub fn float_test() {
        let token_iter = vec![(Token::FloatVal("10.0"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = float_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Number(None, NumericLiteral::Float("10.0")))
        );
    }

    #[test]
    pub fn bool_true_test() {
        let token_iter = vec![(Token::BoolVal("true"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = bool_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Boolean(None, BoolLiteral(true)))
        );
    }

    #[test]
    pub fn bool_false_test() {
        let token_iter = vec![(Token::BoolVal("false"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = bool_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Boolean(None, BoolLiteral(false)))
        );
    }

    #[test]
    pub fn primitive_val_test() {
        // Parse bool
        let token_iter = vec![(Token::BoolVal("false"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = primitive_val_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());
        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Boolean(None, BoolLiteral(false)))
        );

        // Parse int
        let token_iter = vec![(Token::IntVal("10"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = primitive_val_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());
        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Number(None, NumericLiteral::Int("10")))
        );

        // Parse float
        let token_iter = vec![(Token::FloatVal("10.0"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = primitive_val_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());
        assert_eq!(
            res.output(),
            Some(&PrimitiveVal::Number(None, NumericLiteral::Float("10.0")))
        );
    }
}
