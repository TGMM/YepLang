use crate::{
    ast::{Block, Destructure, Stmt, VarDecl},
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

use super::primitive_parser::id_parser;

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

pub fn var_decl_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, VarDecl<'i>, ParserError<'i, Token<'i>>> + Clone {
    todo()
}

recursive_parser!(
    DESTRUCTURE_PARSER,
    destructure_parser,
    Destructure<'static>,
    declarations {},
    main_definition {
        let id = id_parser().map(|id| Destructure::Id(id));

        id
    },
    definitions {}
);

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
