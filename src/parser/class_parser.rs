use super::{
    expr_parser::expr_parser,
    main_parser::{id_parser, master_parser},
};
use crate::{
    ast::{ClassDecl, FnDecl, MemberAcess, MethodDecl},
    lexer::Token,
};
use chumsky::{extra, input::ValueInput, prelude::Rich, primitive::just, span::SimpleSpan, Parser};

pub fn class_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, ClassDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.class_decl
}

pub fn member_accesss_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, MemberAcess<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    expr_parser()
        .then_ignore(just(Token::Dot))
        .then(id_parser())
        .map(|(accessed, property)| MemberAcess { accessed, property })
}

pub fn method_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, MethodDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.method_decl
}

pub fn fn_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, FnDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().0.fn_decl
}
