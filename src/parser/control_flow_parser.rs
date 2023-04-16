use crate::{ast::Expr, lexer::Token};
use chumsky::{extra, input::ValueInput, prelude::Rich, primitive::just, span::SimpleSpan, Parser};

use super::expr_parser::expr_parser;

pub fn paren_expr_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::LParen)
        .ignore_then(expr_parser())
        .then_ignore(just(Token::RParen))
}

#[cfg(test)]
mod test {
    use super::paren_expr_parser;
    use crate::lexer::Token;
    use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
    use logos::Logos;

    #[test]
    fn test() {
        let input = "(10 + 20 + (50 + 7) + 68 + 5)";
        let token_iter = Token::lexer(input)
            .spanned()
            .map::<(Token, SimpleSpan), _>(|(tok, span)| (tok, span.into()));
        let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

        let res = paren_expr_parser().parse(token_stream);
        dbg!(&res.output());
        for err in res.errors() {
            dbg!(err);
        }
        assert!(res.has_output());
        assert!(!res.has_errors());
    }
}
