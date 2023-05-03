use crate::{
    ast::{
        Block, Destructure, PropertyDestructure, PropertyName, Stmt, VarDecl, VarDeclAssignment,
    },
    lexer::Token,
    recursive_parser,
};
use chumsky::{
    extra,
    input::{SpannedInput, Stream},
    prelude::Rich,
    primitive::{just, todo},
    recursive::{Indirect, Recursive},
    span::SimpleSpan,
    IterParser, Parser,
};
use std::{
    sync::{Arc, LazyLock, RwLock},
    vec::IntoIter,
};

use super::{
    expr_parser::{expr_parser, EXPR_PARSER},
    primitive_parser::{
        id_parser, scope_specifier_parser, string_parser, type_specifier_parser, var_type_parser,
    },
};

pub type ParserError<'a, T> = extra::Err<Rich<'a, T>>;
pub type ParserInput<'a> =
    SpannedInput<Token<'a>, SimpleSpan, Stream<IntoIter<(Token<'a>, SimpleSpan)>>>;

pub type RecursiveParser<'a, O> =
    Recursive<Indirect<'a, 'a, ParserInput<'a>, O, ParserError<'a, Token<'a>>>>;
pub type GlobalParser<'a, O> = LazyLock<Arc<RwLock<RecursiveParser<'a, O>>>>;

pub fn stmt_end_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, (), ParserError<'i, Token<'i>>> + Clone {
    just(Token::StmtEnd).ignored()
}

recursive_parser!(
    DESTRUCTURE_PARSER,
    destructure_parser,
    Destructure<'static>,
    declarations {
        let destructure = DESTRUCTURE_PARSER.read().unwrap().clone()
    },
    main_definition {
        let id = id_parser().map(|id| Destructure::Id(id));
        let arr = destructure
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
            .map(Destructure::Array);
        let obj = {
            let str_prop_name = string_parser()
                .map(|s| s.to_string())
                .map(PropertyName::String);
            let id_prop_name = id_parser().map(PropertyName::Id);

            let alias = just(Token::Colon).ignore_then(destructure.clone());

            let str_prop = str_prop_name
                .then(alias.clone())
                .map(|(name, alias)| PropertyDestructure {
                    name,
                    alias: Some(alias),
                });
            let id_prop = id_prop_name
                .then(alias.or_not())
                .map(|(name, alias)| PropertyDestructure { name, alias });

            str_prop.or(id_prop)
        };
        let obj = obj
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(Destructure::Object);

        id.or(arr).or(obj)
    },
    definitions {
        if destructure.is_defined() {
            destructure_parser();
        }
    }
);

pub fn var_decl_assignment_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, VarDeclAssignment<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let destructure = DESTRUCTURE_PARSER.read().unwrap().clone();
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let var_decl_as = destructure
        .clone()
        .then(type_specifier_parser().or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(expr.clone())
        .map(|((destructure, var_type), expr)| VarDeclAssignment {
            destructure,
            var_type,
            expr,
        });

    // Definitions
    if destructure.is_defined() {
        destructure_parser();
    }
    if expr.is_defined() {
        expr_parser();
    }

    var_decl_as
}

pub fn var_decl_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, VarDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    let decl_assignments_p = var_decl_assignment_parser()
        .separated_by(just(Token::Comma))
        .at_least(1)
        .collect::<Vec<_>>();

    scope_specifier_parser()
        .then(decl_assignments_p)
        .map(|(scope_spec, decl_assignments)| VarDecl {
            scope_spec,
            decl_assignments,
        })
}

recursive_parser!(
    STMT_PARSER,
    stmt_parser,
    Stmt<'static>,
    declarations {
        let block = BLOCK_PARSER.read().unwrap().clone().map(|b| Stmt::Block(b))
    },
    main_definition {
        block
    },
    definitions {

    }
);

recursive_parser!(
    TOP_BLOCK_PARSER,
    top_block_parser,
    Block<'static>,
    declarations {
        let stmt = STMT_PARSER.read().unwrap().clone()
    },
    main_definition {
        stmt.clone()
            .repeated()
            .collect::<Vec<_>>()
            .map(|stmts| Block { stmts })
    },
    definitions {
        if !stmt.is_defined() {
            stmt_parser();
        }
    }
);

recursive_parser!(
    BLOCK_PARSER,
    block_parser,
    Block<'static>,
    declarations {
        let top_block = TOP_BLOCK_PARSER.read().unwrap().clone()
    },
    main_definition {
        top_block.clone().delimited_by(just(Token::LBracket), just(Token::RBracket))
    },
    definitions {
        if !top_block.is_defined() {
            top_block_parser();
        }
    }
);

#[cfg(test)]
mod test {
    use chumsky::Parser;

    use crate::{
        lexer::Token,
        parser::{helpers::test::stream_token_vec, main_parser::stmt_end_parser},
    };

    #[test]
    fn stmt_end_parser_test() {
        let tokens = stream_token_vec(vec![Token::StmtEnd]);
        let res = stmt_end_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let stmt_end = res.unwrap();
        assert_eq!(stmt_end, ())
    }

    #[test]
    fn stmt_end_parser_err_test() {
        let tokens = stream_token_vec(vec![]);

        let res = stmt_end_parser().parse(tokens).into_result();
        assert!(res.is_err());
    }
}
