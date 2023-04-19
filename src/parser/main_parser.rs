use crate::ast::{
    Assignment, ClassDecl, DoWhile, ElseIf, FnDecl, For, If, MethodDecl, PropertyDestructure,
    PropertyDestructureName, PropertyName, ValueVarType, While,
};
use crate::{
    ast::{Block, Destructure, Id, Stmt, VarDecl, VarType},
    lexer::Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::extra::Full;
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

use super::class_parser::member_accesss_parser;
use super::control_flow_parser::paren_expr_parser;
use super::expr_parser::expr_parser;
use super::value_parser::{fn_call_parser, string_parser};

pub type RichTokenErr<'a> = extra::Err<Rich<'a, Token<'a>>>;
macro_rules! token_parser {
    ($name:ident, $ret_type:ty, $b:block) => {
        pub fn $name<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
        ) -> impl Parser<'a, I, $ret_type, RichTokenErr<'a>> + Clone $b
    };
}

token_parser!(id_parser, Id, {
    select! {
        Token::Id(id) => id
    }
    .labelled("id")
    .map(|id| Id(id.to_string()))
});

pub fn destructure_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Destructure<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    recursive(|destructure| {
        let id = id_parser().map(|id| Destructure::Id(id));
        let array = destructure
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
            .map(|destructures| Destructure::Array(destructures));

        let str_prop_name = string_parser()
            .map(|s| PropertyName::String(s.to_string()))
            .map(|pn| PropertyDestructureName::PropName(pn));
        let id_prop_name = id_parser()
            .map(|id| PropertyName::Id(id))
            .map(|pn| PropertyDestructureName::PropName(pn));
        let destructure_prop_name = destructure.map(|d| PropertyDestructureName::Destructure(d));
        let alias = just(Token::Colon).ignore_then(id_parser());
        let property = str_prop_name
            .or(id_prop_name)
            .or(destructure_prop_name)
            .then(alias.or_not())
            .map(|(name, alias)| PropertyDestructure { name, alias });

        let member_access = member_accesss_parser().map(|ma| Destructure::MemberAccess(ma));

        let object = property
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|o| Destructure::Object(o));

        member_access.or(id).or(array).or(object)
    })
}

pub fn destructure_assignment_parser<
    'a,
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
>() -> impl Parser<'a, I, Assignment<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    destructure_parser()
        .then_ignore(just(Token::AssignmentEq))
        .then(expr_parser())
        .then_ignore(stmt_end_parser())
        .map(|(destructure, assigned_expr)| Assignment {
            destructure,
            assigned_expr,
        })
}

pub fn type_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, ValueVarType, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    let arr_type_decl = just(Token::LSqBracket).ignore_then(just(Token::RSqBracket).ignored());

    just(Token::Colon)
        .ignore_then(
            select! { Token::VarType(t) => t }.or(id_parser().map(|id| VarType::Custom(id))),
        )
        .then(arr_type_decl.or_not())
        .labelled("type declaration")
        .map(|(vtype, arr_decl)| ValueVarType {
            vtype,
            is_array: arr_decl.is_some(),
        })
}

pub fn stmt_end_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Token<'a>, extra::Err<Rich<'a, Token<'a>>>> + Clone {
    just(Token::StmtEnd).recover_with(via_parser(empty().to(Token::StmtEnd)))
}

fn var_decl_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, VarDecl<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    select! { Token::ScopeSpecifier(ss) => ss }
        .then(destructure_parser())
        .then(type_decl_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(expr_parser())
        .then_ignore(stmt_end_parser())
        .map(|(((scope_spec, destruct), var_type), val)| VarDecl {
            scope_spec,
            destructure: destruct,
            var_type,
            expr: val.into(),
        })
}

pub fn non_rec_stmt_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Stmt<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    let assignment = destructure_assignment_parser().map(|a| Stmt::Assignment(a));
    let expr = expr_parser().map(|e| Stmt::Expr(e));
    let var_decl = var_decl_parser().map(|vd| Stmt::VarDecl(vd));
    let fn_call = fn_call_parser().map(|fc| Stmt::FnCall(fc));

    assignment.or(fn_call).or(var_decl).or(expr)
}

pub fn block_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Block<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    top_block_parser().delimited_by(just(Token::LBracket), just(Token::RBracket))
}

pub struct RecursiveParsers<'a, 'b, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> {
    pub class_decl: Boxed<'a, 'b, I, ClassDecl<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
    pub fn_decl: Boxed<'a, 'b, I, FnDecl<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
    pub for_d: Boxed<'a, 'b, I, For<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
    pub while_d: Boxed<'a, 'b, I, While<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
    pub do_while_d: Boxed<'a, 'b, I, DoWhile<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
    pub if_d: Boxed<'a, 'b, I, If<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
    pub method_decl: Boxed<'a, 'b, I, MethodDecl<'a>, Full<Rich<'a, Token<'a>>, (), ()>>,
}

pub fn top_block_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Block<'a>, extra::Err<Rich<'a, Token<'a>>>> {
    master_parser().1
}

// Yes, it's so godly it deserves this name
pub fn master_parser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>() -> (
    RecursiveParsers<'a, 'a, I>,
    impl Parser<'a, I, Block<'a>, extra::Err<Rich<'a, Token<'a>>>>,
) {
    let mut top_block = Recursive::declare();
    let block = just(Token::LBracket)
        .ignore_then(top_block.clone())
        .then_ignore(just(Token::RBracket));
    let block_stmt = block.clone().map(|b| Stmt::Block(b));

    let class_decl_parser = {
        let extends_parser = just(Token::Extends).ignore_then(id_parser());

        just(Token::Class)
            .ignore_then(id_parser())
            .then(extends_parser.or_not())
            .then(block.clone())
            .map(|((class_id, extended_class_id), block)| ClassDecl {
                class_id,
                extended_class_id,
                block,
            })
    };

    let method_decl_parser = {
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
            .then(block.clone())
            .map(|(((method_id, args), ret_type), block)| MethodDecl {
                method_id,
                args,
                ret_type,
                block,
            })
    };

    let fn_decl_parser = {
        just(Token::Function)
            .ignore_then(method_decl_parser.clone())
            .map(|md| FnDecl {
                fn_id: md.method_id,
                args: md.args,
                ret_type: md.ret_type,
                block: md.block,
            })
    };

    let for_parser = {
        let for_conditions_parser = just(Token::LParen)
            .ignore_then(expr_parser().or_not())
            .then_ignore(just(Token::StmtEnd))
            .then(expr_parser().or_not())
            .then_ignore(just(Token::StmtEnd))
            .then(expr_parser().or_not())
            .then_ignore(just(Token::RParen))
            .map(|((e1, e2), e3)| (e1, e2, e3));

        just(Token::For)
            .ignore_then(for_conditions_parser)
            .then(block.clone())
            .map(|((decl_expr, cmp_expr, postfix_expr), block)| For {
                decl_expr,
                cmp_expr,
                postfix_expr,
                block,
            })
    };

    let while_parser = {
        just(Token::While)
            .ignore_then(paren_expr_parser())
            .then(block.clone())
            .map(|(while_cond, block)| While { while_cond, block })
    };

    let do_while_parser = {
        just(Token::Do)
            .ignore_then(block.clone())
            .then_ignore(just(Token::While))
            .then(paren_expr_parser())
            .then_ignore(stmt_end_parser())
            .map(|(do_block, while_cond)| DoWhile {
                do_block,
                while_cond,
            })
    };

    let if_else_parser = {
        let else_if_parser = {
            just(Token::Else)
                .ignore_then(just(Token::If))
                .ignore_then(paren_expr_parser())
                .then(block.clone())
                .map(|(else_expr, else_block)| ElseIf {
                    else_expr,
                    else_block,
                })
                .repeated()
                .collect::<Vec<_>>()
        };

        let else_parser = just(Token::Else).ignore_then(block.clone());

        just(Token::If)
            .ignore_then(paren_expr_parser())
            .then(block.clone())
            .then(else_if_parser)
            .then(else_parser.or_not())
            .map(|(((if_expr, if_block), else_if), else_b)| If {
                if_expr,
                if_block,
                else_if,
                else_b,
            })
    };

    let rec_parser_struct = RecursiveParsers {
        class_decl: class_decl_parser.clone().boxed(),
        fn_decl: fn_decl_parser.clone().boxed(),
        for_d: for_parser.clone().boxed(),
        while_d: while_parser.clone().boxed(),
        do_while_d: do_while_parser.clone().boxed(),
        if_d: if_else_parser.clone().boxed(),
        method_decl: method_decl_parser.clone().boxed(),
    };

    let class_decl = class_decl_parser.map(|cd| Stmt::ClassDecl(cd));
    let fn_decl = fn_decl_parser.map(|fd| Stmt::FnDecl(fd));
    let for_d = for_parser.map(|f| Stmt::For(f));
    let while_d = while_parser.map(|w| Stmt::While(w));
    let do_while_d = do_while_parser.map(|dw| Stmt::DoWhile(dw));
    let if_d = if_else_parser.map(|ie| Stmt::If(ie));
    let method_decl = method_decl_parser.map(|md| Stmt::MethodDecl(md));
    let rec_stms_parser = class_decl
        .or(fn_decl)
        .or(method_decl)
        .or(for_d)
        .or(while_d)
        .or(do_while_d)
        .or(if_d);

    top_block.define(
        block_stmt
            .or(rec_stms_parser.boxed())
            .or(non_rec_stmt_parser().boxed())
            .repeated()
            .collect::<Vec<Stmt>>()
            .map(|stmts| Block { stmts }),
    );

    (rec_parser_struct, top_block)
}

pub fn parse(input: &str) {
    let tokens = Token::lexer(input).spanned().collect::<Vec<_>>();
    let token_iter = tokens
        .into_iter()
        .map::<(Token, SimpleSpan), _>(|(tok, span)| (tok, span.into()));

    let token_stream = Stream::from_iter(token_iter).spanned((input.len()..input.len()).into());

    match top_block_parser().parse(token_stream).into_result() {
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
    use super::{stmt_end_parser, type_decl_parser, var_decl_parser};
    use crate::{
        ast::{
            Destructure, NumericLiteral, PrimitiveVal, ScopeSpecifier, ValueVarType, VarDecl,
            VarType,
        },
        lexer::Token,
    };
    use chumsky::{
        error::RichPattern, input::Stream, prelude::Input, span::SimpleSpan, util::Maybe, Parser,
    };

    #[test]
    pub fn type_decl_test() {
        let token_iter = vec![
            (Token::Colon, SimpleSpan::new(1usize, 1usize)),
            (
                Token::VarType(VarType::I32),
                SimpleSpan::new(2usize, 2usize),
            ),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = type_decl_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&ValueVarType {
                vtype: VarType::I32,
                is_array: false
            })
        );
    }

    #[test]
    pub fn stmt_end_test() {
        let token_iter = vec![(Token::StmtEnd, SimpleSpan::new(1usize, 1usize))];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = stmt_end_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(res.output(), Some(&Token::StmtEnd));
    }

    #[test]
    pub fn stmt_end_recovery_test() {
        let token_iter: Vec<(Token, SimpleSpan)> = vec![];
        let token_stream = Stream::from_iter(token_iter).spanned((0..0).into());
        let res = stmt_end_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(res.has_errors());

        assert_eq!(res.output(), Some(&Token::StmtEnd));

        let err = res.errors().next().unwrap();
        // Expected ;
        assert_eq!(
            err.expected().next(),
            Some(&RichPattern::Token(Maybe::Val(Token::StmtEnd)))
        );
        // Found end of input
        assert_eq!(err.reason().found(), None);
    }

    #[test]
    pub fn var_decl_test() {
        // const x: i32 = 10;
        let token_iter = vec![
            (
                Token::ScopeSpecifier(ScopeSpecifier::Const),
                SimpleSpan::new(1usize, 1usize),
            ),
            (Token::Id("x".into()), SimpleSpan::new(1usize, 1usize)),
            (Token::Colon, SimpleSpan::new(1usize, 1usize)),
            (
                Token::VarType(VarType::I32),
                SimpleSpan::new(1usize, 1usize),
            ),
            (Token::AssignmentEq, SimpleSpan::new(1usize, 1usize)),
            (Token::IntVal("10"), SimpleSpan::new(1usize, 1usize)),
            (Token::StmtEnd, SimpleSpan::new(1usize, 1usize)),
        ];
        let token_stream = Stream::from_iter(token_iter).spanned((1..1).into());
        let res = var_decl_parser().parse(token_stream);

        assert!(res.has_output());
        assert!(!res.has_errors());

        assert_eq!(
            res.output(),
            Some(&VarDecl {
                scope_spec: ScopeSpecifier::Const,
                destructure: Destructure::Id("x".into()),
                var_type: Some(ValueVarType {
                    vtype: VarType::I32,
                    is_array: false
                }),
                expr: PrimitiveVal::Number(None, NumericLiteral::Int("10")).into()
            })
        );
    }
}
