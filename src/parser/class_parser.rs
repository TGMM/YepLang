use super::{
    expr_parser::expr_parser,
    main_parser::{block_parser, destructure_parser, id_parser, type_decl_parser},
};
use crate::{
    ast::{ClassDecl, FnDecl, MemberAcess, MethodDecl},
    lexer::Token,
};
use chumsky::{
    extra, input::ValueInput, prelude::Rich, primitive::just, span::SimpleSpan, IterParser, Parser,
};

pub fn class_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, ClassDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let extends_parser = just(Token::Extends).ignore_then(id_parser());

    just(Token::Class)
        .ignore_then(id_parser())
        .then(extends_parser.or_not())
        .then(block_parser())
        .map(|((class_id, extended_class_id), block)| ClassDecl {
            class_id,
            extended_class_id,
            block,
        })
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
    let return_type = type_decl_parser();
    let args = destructure_parser()
        .then(type_decl_parser())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>();

    id_parser()
        .then_ignore(just(Token::LParen))
        .then(args)
        .then_ignore(just(Token::RParen))
        .then(return_type.or_not())
        .then(block_parser())
        .map(|(((method_id, args), ret_type), block)| MethodDecl {
            method_id,
            args,
            ret_type,
            block,
        })
}

pub fn fn_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, FnDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    just(Token::Function)
        .ignore_then(method_decl_parser())
        .map(|md| FnDecl {
            fn_id: md.method_id,
            args: md.args,
            ret_type: md.ret_type,
            block: md.block,
        })
}
