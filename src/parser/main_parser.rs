use crate::{
    ast::{
        Assignment, BOp, Block, Destructure, Expr, PropertyDestructure, PropertyName, Stmt,
        TopBlock, VarDecl, VarDeclAssignment,
    },
    lexer::Token,
    recursive_parser,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    extra,
    input::{SpannedInput, Stream},
    prelude::{Input, Rich},
    primitive::just,
    recursive::{Indirect, Recursive},
    span::SimpleSpan,
    IterParser, Parser,
};
use logos::Logos;
use std::{
    sync::{Arc, LazyLock, RwLock},
    vec::IntoIter,
};

use super::{
    class_parser::{class_decl_parser, fn_decl_parser, return_parser},
    control_flow_parser::{do_while_parser, for_parser, if_parser, while_parser},
    expr_parser::{assignment_expr_parser, expr_parser, EXPR_PARSER},
    ffi_parser::extern_decl_parser,
    primitive_parser::{
        bop_parser, id_parser, scope_specifier_parser, string_parser, type_specifier_parser,
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
        if !destructure.is_defined() {
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
        .then(just(Token::AssignmentEq).ignore_then(expr.clone()).or_not())
        .map(|((destructure, var_type), expr)| VarDeclAssignment {
            destructure,
            var_type,
            expr,
        });

    // Definitions
    if !destructure.is_defined() {
        destructure_parser();
    }
    if !expr.is_defined() {
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

// TODO: Make this chainable with a right-associative operator
// Ex. x = y = z
// TODO: Make this be able to have an operator before eq
// Ex. x += 10
pub fn assignment_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Assignment<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let non_cmp_op = bop_parser().filter(|bop| {
        matches!(
            bop,
            BOp::Add | BOp::Sub | BOp::Mul | BOp::Div | BOp::Mod | BOp::Pow
        )
    });
    let assignment = assignment_expr_parser()
        .filter(|e| matches!(e, Expr::Id(_) | Expr::Indexing(_) | Expr::MemberAccess(_)))
        .then(non_cmp_op.or_not())
        .then_ignore(just(Token::AssignmentEq))
        .then(expr.clone())
        .map(|((assignee_expr, bop), assigned_expr)| Assignment {
            assignee_expr,
            bop,
            assigned_expr,
        });

    // Definitions
    if !expr.is_defined() {
        expr_parser();
    }

    assignment
}

pub fn for_stmt_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, Stmt<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let assignment = assignment_parser().map(Stmt::Assignment);
    let expr_s = expr.clone().map(Stmt::Expr);
    let var_decl = var_decl_parser().map(Stmt::VarDecl);

    let for_stmt = assignment.or(expr_s).or(var_decl);

    // Definitions
    if !expr.is_defined() {
        expr_parser();
    }

    for_stmt
}

recursive_parser!(
    STMT_PARSER,
    stmt_parser,
    Stmt<'static>,
    declarations {
        let block = BLOCK_PARSER.read().unwrap().clone()
    },
    main_definition {
        let fn_decl = fn_decl_parser().map(Stmt::FnDecl);
        let class_decl = class_decl_parser().map(Stmt::ClassDecl);
        let for_ = for_parser().map(Stmt::For);
        let while_ = while_parser().map(Stmt::While);
        let do_while = do_while_parser().map(Stmt::DoWhile);
        let if_ = if_parser().map(Stmt::If);
        let block_s = block.clone().map(Stmt::Block);
        let extern_decl = extern_decl_parser().map(Stmt::ExternDecl);
        let return_stmt = return_parser().map(Stmt::Return);

        let stmt_nt = fn_decl.or(class_decl).or(for_).or(while_).or(if_).or(block_s);
        let stmt_t = do_while.or(extern_decl).or(return_stmt).or(for_stmt_parser());
        let stmt = stmt_nt.or(stmt_t.then_ignore(stmt_end_parser()));

        stmt
    },
    definitions {
        if !block.is_defined() {
            block_parser();
        }
    }
);

recursive_parser!(
    TOP_BLOCK_PARSER,
    top_block_parser,
    TopBlock<'static>,
    declarations {
        let stmt = STMT_PARSER.read().unwrap().clone()
    },
    main_definition {
        stmt.clone()
            .repeated()
            .collect::<Vec<_>>()
            .map(|stmts| Block { stmts })
            .map(TopBlock)
    },
    definitions {
        if !stmt.is_defined() {
            stmt_parser();
        }
    }
);

pub fn parse(input: &'static str, file_name: &'static str) -> Option<TopBlock<'static>> {
    let tokens = Token::lexer(input)
        .spanned()
        .map(|(token, span)| (token.unwrap(), SimpleSpan::from(span)))
        .collect::<Vec<_>>();
    let length = tokens.len();
    let tokens_stream = Stream::from_iter(tokens).spanned((length..length).into());

    let top_block = top_block_parser().parse(tokens_stream);

    let errs = top_block.errors();
    errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, file_name, e.span().start)
            .with_code(3)
            .with_message(e.to_string())
            .with_label(
                Label::new((file_name, e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((file_name, Source::from(input)))
            .unwrap()
    });

    top_block.into_result().ok()
}

recursive_parser!(
    BLOCK_PARSER,
    block_parser,
    Block<'static>,
    declarations {
        let top_block = TOP_BLOCK_PARSER.read().unwrap().clone()
    },
    main_definition {
        top_block.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)).map(|tb| tb.0)
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
        ast::{
            Assignment, BExpr, BOp, Block, Destructure, Expr, NumericLiteral, PrimitiveVal,
            PropertyDestructure, PropertyName, ScopeSpecifier, Stmt, TopBlock, VarDecl,
            VarDeclAssignment,
        },
        lexer::Token,
        parser::{
            helpers::test::stream_token_vec,
            main_parser::{
                assignment_parser, block_parser, destructure_parser, for_stmt_parser,
                stmt_end_parser, top_block_parser, var_decl_parser,
            },
        },
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

    #[test]
    fn destructure_arr_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::LSqBracket,
            Token::Id("x".into()),
            Token::RSqBracket,
        ]);

        let res = destructure_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let destructure = res.unwrap();
        assert_eq!(
            destructure,
            Destructure::Array(vec![Destructure::Id("x".into())])
        )
    }

    #[test]
    fn destructure_id_parser_test() {
        let tokens = stream_token_vec(vec![Token::Id("x".into())]);

        let res = destructure_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let destructure = res.unwrap();
        assert_eq!(destructure, Destructure::Id("x".into()))
    }

    #[test]
    fn destructure_obj_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::LBracket,
            Token::Str(r#""x""#),
            Token::Colon,
            Token::Id("x".into()),
            Token::Comma,
            Token::Id("y".into()),
            Token::Colon,
            Token::Id("z".into()),
            Token::RBracket,
        ]);

        let res = destructure_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let destructure = res.unwrap();
        assert_eq!(
            destructure,
            Destructure::Object(vec![
                PropertyDestructure {
                    name: PropertyName::String("x".to_string()),
                    alias: Some(Destructure::Id("x".into()))
                },
                PropertyDestructure {
                    name: PropertyName::Id("y".into()),
                    alias: Some(Destructure::Id("z".into()))
                }
            ])
        )
    }

    #[test]
    fn assignment_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::IntVal("10"),
        ]);

        let res = assignment_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let assignment = res.unwrap();
        assert_eq!(
            assignment,
            Assignment {
                assignee_expr: Expr::Id("x".into()),
                bop: None,
                assigned_expr: PrimitiveVal::Number(None, NumericLiteral::Int("10")).into()
            }
        )
    }

    #[test]
    fn var_decl_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::ScopeSpecifier(ScopeSpecifier::Let),
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::IntVal("10"),
        ]);

        let parse_res = var_decl_parser().parse(tokens);
        for err in parse_res.errors() {
            dbg!(err);
        }
        let res = parse_res.into_result();
        assert!(res.is_ok());

        let assignment = res.unwrap();
        assert_eq!(
            assignment,
            VarDecl {
                scope_spec: ScopeSpecifier::Let,
                decl_assignments: vec![VarDeclAssignment {
                    destructure: Destructure::Id("x".into()),
                    var_type: None,
                    expr: Some(Expr::PrimitiveVal(PrimitiveVal::Number(
                        None,
                        NumericLiteral::Int("10")
                    )))
                }]
            }
        )
    }

    #[test]
    fn top_block_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::IntVal("10"),
            Token::StmtEnd,
            Token::Id("y".into()),
            Token::BOp(BOp::Add),
            Token::IntVal("10"),
            Token::StmtEnd,
        ]);

        let res = top_block_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let block = res.unwrap();
        assert_eq!(
            block,
            TopBlock(Block {
                stmts: vec![
                    Stmt::Assignment(Assignment {
                        assignee_expr: Expr::Id("x".into()),
                        bop: None,
                        assigned_expr: PrimitiveVal::Number(None, NumericLiteral::Int("10")).into()
                    }),
                    Stmt::Expr(Expr::BinaryExpr(Box::new(BExpr {
                        lhs: Expr::Id("y".into()),
                        op: BOp::Add,
                        rhs: Expr::PrimitiveVal(PrimitiveVal::Number(
                            None,
                            NumericLiteral::Int("10")
                        )),
                    })))
                ]
            })
        )
    }

    #[test]
    fn empty_block_parser_test() {
        let tokens = stream_token_vec(vec![Token::LBracket, Token::RBracket]);

        let res = block_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let block = res.unwrap();
        assert_eq!(block, Block { stmts: vec![] });
    }

    #[test]
    fn for_stmt_assignment_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::IntVal("10"),
        ]);

        let res = for_stmt_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let block = res.unwrap();
        assert_eq!(
            block,
            Stmt::Assignment(Assignment {
                assignee_expr: Expr::Id("x".into()),
                bop: None,
                assigned_expr: Expr::PrimitiveVal(PrimitiveVal::Number(
                    None,
                    NumericLiteral::Int("10")
                ))
            })
        )
    }

    #[test]
    fn for_stmt_var_decl_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::ScopeSpecifier(ScopeSpecifier::Let),
            Token::Id("x".into()),
            Token::AssignmentEq,
            Token::IntVal("10"),
        ]);

        let res = for_stmt_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let block = res.unwrap();
        assert_eq!(
            block,
            Stmt::VarDecl(VarDecl {
                scope_spec: ScopeSpecifier::Let,
                decl_assignments: vec![VarDeclAssignment {
                    destructure: Destructure::Id("x".into()),
                    var_type: None,
                    expr: Some(Expr::PrimitiveVal(PrimitiveVal::Number(
                        None,
                        NumericLiteral::Int("10")
                    )))
                }],
            })
        )
    }

    #[test]
    fn for_stmt_expr_parser_test() {
        let tokens = stream_token_vec(vec![
            Token::Id("x".into()),
            Token::BOp(BOp::Add),
            Token::IntVal("10"),
        ]);

        let res = for_stmt_parser().parse(tokens).into_result();
        assert!(res.is_ok());

        let block = res.unwrap();
        assert_eq!(
            block,
            Stmt::Expr(Expr::BinaryExpr(
                BExpr {
                    lhs: Expr::Id("x".into()),
                    op: BOp::Add,
                    rhs: Expr::PrimitiveVal(PrimitiveVal::Number(None, NumericLiteral::Int("10")))
                }
                .into()
            ))
        )
    }
}
