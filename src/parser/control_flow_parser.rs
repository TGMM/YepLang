use super::{expr_parser::expr_parser, main_parser::master_parser};
use crate::{
    ast::{DoWhile, Expr, For, If, While},
    lexer::Token,
};
use chumsky::{
    extra, input::ValueInput, prelude::Rich, primitive::just, span::SimpleSpan, IterParser, Parser,
};

pub fn paren_expr_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    just(Token::LParen)
        .ignore_then(expr_parser())
        .then_ignore(just(Token::RParen))
}

pub fn while_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, While<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.while_d
}

pub fn do_while_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, DoWhile<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.do_while_d
}

pub fn if_else_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, If<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.if_d
}

pub fn for_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, For<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.for_d
}

#[cfg(test)]
mod test {
    use super::paren_expr_parser;
    use crate::{
        ast::{
            Block, BoolLiteral, Exp, Expr, Factor, Id, If, NumericLiteral, PrimitiveVal,
            ScopeSpecifier, Stmt, Term, VarDecl,
        },
        lexer::Token,
        parser::control_flow_parser::if_else_parser,
        span_token,
    };
    use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
    use logos::Logos;

    #[test]
    pub fn if_test() {
        // if(true) { let x = 10; }
        let token_iter = vec![
            span_token!(Token::If),
            span_token!(Token::LParen),
            span_token!(Token::BoolVal("true")),
            span_token!(Token::RParen),
            span_token!(Token::LBracket),
            span_token!(Token::ScopeSpecifier(ScopeSpecifier::Let)),
            span_token!(Token::Id("x")),
            span_token!(Token::AssignmentEq),
            span_token!(Token::IntVal("10")),
            span_token!(Token::StmtEnd),
            span_token!(Token::RBracket),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = if_else_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&If {
                if_expr: Expr {
                    lhs: Exp::Term(Term::Factor(Factor::PrimitiveVal(PrimitiveVal::Boolean(
                        None,
                        BoolLiteral(true)
                    )))),
                    rhs: None
                },
                if_block: Block {
                    stmts: vec![Stmt::VarDecl(VarDecl {
                        scope_spec: ScopeSpecifier::Let,
                        var_type: None,
                        destructure: Id("x".to_string()).into(),
                        expr: Expr {
                            lhs: Exp::Term(Term::Factor(Factor::PrimitiveVal(
                                PrimitiveVal::Number(None, NumericLiteral::Int("10"))
                            ))),
                            rhs: None
                        }
                    })]
                },
                else_if: vec![],
                else_b: None
            })
        );
    }

    pub fn if_else_test() {
        let token_iter = vec![
            span_token!(Token::If),
            span_token!(Token::LParen),
            span_token!(Token::BoolVal("true")),
            span_token!(Token::RParen),
            span_token!(Token::LBracket),
            span_token!(Token::ScopeSpecifier(ScopeSpecifier::Let)),
            span_token!(Token::Id("x")),
            span_token!(Token::AssignmentEq),
            span_token!(Token::IntVal("10")),
            span_token!(Token::StmtEnd),
            span_token!(Token::RBracket),
            span_token!(Token::Else),
            span_token!(Token::If),
            span_token!(Token::LParen),
            span_token!(Token::BoolVal("fals")),
            span_token!(Token::RParen),
            span_token!(Token::LBracket),
            span_token!(Token::ScopeSpecifier(ScopeSpecifier::Let)),
            span_token!(Token::Id("x")),
            span_token!(Token::AssignmentEq),
            span_token!(Token::IntVal("15")),
            span_token!(Token::StmtEnd),
            span_token!(Token::RBracket),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = if_else_parser().parse(token_stream);
        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), todo!());
    }

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
