use super::{main_parser::id_parser, value_parser::primitive_val_parser};
use crate::{
    ast::{CmpOp, Exp, ExpBOp, ExpOp, Expr, Factor, Term, TermBOp, TermOp},
    lexer::Token,
};
use chumsky::{
    extra::{self},
    input::ValueInput,
    prelude::Rich,
    select,
    span::SimpleSpan,
    IterParser, Parser,
};

fn factor_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Factor<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let id = id_parser().map(|id| Factor::Id(id));
    let literal = primitive_val_parser().map(|pv| Factor::PrimitiveVal(pv));

    literal.or(id)
}

fn term_op_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, TermOp, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::TermOp(term_op) => term_op }
}

fn term_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Term<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let factor = factor_parser();
    let term_rhs = term_op_parser().then(factor_parser());

    factor
        .then(term_rhs.repeated().collect::<Vec<_>>())
        .map(|(lhs, rhs)| {
            if rhs.is_empty() {
                return Term::Factor(lhs);
            }

            if rhs.len() == 1 {
                let (op, rhs) = rhs.into_iter().next().unwrap();
                return Term::BOp(Box::new(TermBOp {
                    lhs: Term::Factor(lhs),
                    op,
                    rhs: Term::Factor(rhs),
                }));
            }

            let mut lhs = Term::Factor(lhs);
            for (op, rhs) in rhs {
                lhs = Term::BOp(Box::new(TermBOp {
                    lhs,
                    op,
                    rhs: Term::Factor(rhs),
                }));
            }

            return lhs;
        })
}

fn exp_op_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, ExpOp, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::ExpOp(exp_op) => exp_op }
}

fn exp_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Exp<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let term = term_parser();
    let exp_rhs = exp_op_parser().then(term_parser());

    term.then(exp_rhs.repeated().collect::<Vec<_>>())
        .map(|(lhs, rhs)| {
            if rhs.is_empty() {
                return Exp::Term(lhs);
            }

            if rhs.len() == 1 {
                let (op, rhs) = rhs.into_iter().next().unwrap();
                return Exp::BOp(Box::new(ExpBOp {
                    lhs: Exp::Term(lhs),
                    op,
                    rhs: Exp::Term(rhs),
                }));
            }

            let mut lhs = Exp::Term(lhs);
            for (op, rhs) in rhs {
                lhs = Exp::BOp(Box::new(ExpBOp {
                    lhs,
                    op,
                    rhs: Exp::Term(rhs),
                }));
            }

            return lhs;
        })
}

fn cmp_op_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, CmpOp, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::CmpOp(cmp_op) => cmp_op }
}

pub fn expr_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    fn expr_rhs_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    ) -> impl Parser<'a, I, (CmpOp, Exp<'a>), extra::Err<Rich<'a, Token<'a>>>> {
        cmp_op_parser().then(exp_parser())
    }

    exp_parser()
        .then(expr_rhs_parser().or_not())
        .map(|(lhs, rhs)| Expr { lhs, rhs })
}

#[cfg(test)]
mod test {
    use super::expr_parser;
    use crate::{
        ast::{
            CmpOp, Exp, ExpBOp, ExpOp, Expr, Factor, NumericLiteral, PrimitiveVal, Term, TermBOp,
            TermOp,
        },
        lexer::Token,
        parser::expr_parser::{exp_parser, factor_parser, term_parser},
    };
    use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
    use logos::Logos;

    #[test]
    fn factor_test() {
        let token_iter = vec![(Token::Id("test"), SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = factor_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&Factor::Id("test".into())));
    }

    #[test]
    fn expr_test() {
        let token_iter = vec![
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
            (Token::CmpOp(CmpOp::Gt), SimpleSpan::new(1usize, 1usize)),
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = expr_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        let lhs = Exp::Term(Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Number(
            None,
            NumericLiteral::Int("10"),
        ))));
        let op = CmpOp::Gt;
        let rhs = Exp::Term(Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Number(
            None,
            NumericLiteral::Int("10"),
        ))));
        assert_eq!(
            res.output(),
            Some(&Expr {
                lhs,
                rhs: Some((op, rhs))
            })
        );
    }

    #[test]
    fn exp_test() {
        let token_iter = vec![
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
            (Token::ExpOp(ExpOp::Add), SimpleSpan::new(1usize, 1usize)),
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = exp_parser().parse(token_stream);

        for err in res.errors() {
            dbg!(err);
        }

        assert!(res.has_output());
        assert!(!res.has_errors());

        let lhs = Exp::Term(Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Number(
            None,
            NumericLiteral::Int("10"),
        ))));
        let op = ExpOp::Add;
        let rhs = Exp::Term(Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Number(
            None,
            NumericLiteral::Int("10"),
        ))));
        assert_eq!(
            res.output(),
            Some(&Exp::BOp(Box::new(ExpBOp { lhs, op, rhs })))
        );
    }

    #[test]
    fn term_test() {
        let token_iter = vec![
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
            (Token::TermOp(TermOp::Mul), SimpleSpan::new(1usize, 1usize)),
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = term_parser().parse(token_stream);

        for err in res.errors() {
            dbg!(err);
        }

        assert!(res.has_output());
        assert!(!res.has_errors());

        let lhs = Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Number(
            None,
            NumericLiteral::Int("10"),
        )));
        let op = TermOp::Mul;
        let rhs = Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Number(
            None,
            NumericLiteral::Int("10"),
        )));
        assert_eq!(
            res.output(),
            Some(&Term::BOp(Box::new(TermBOp { lhs, op, rhs })))
        );
    }

    #[test]
    fn ultra_mega_test() {
        let input = "10 + 20 + 50 + 7 + 68 + 5";
        let token_iter = Token::lexer(input)
            .spanned()
            .map::<(Token, SimpleSpan), _>(|(tok, span)| (tok, span.into()));

        let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

        let res = expr_parser().parse(token_stream);
        dbg!(&res.output());
        assert!(res.has_output());
        assert!(!res.has_errors());
    }
}
