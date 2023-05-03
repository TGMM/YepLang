use super::{
    expr_parser::{boolean_unary_op_parser, expr_parser, numeric_unary_op_parser, EXPR_PARSER},
    main_parser::{ParserError, ParserInput},
};
use crate::{
    ast::{
        ArrayVal, BOp, BoolLiteral, Id, NumericLiteral, PrimitiveVal, PropertyName, ScopeSpecifier,
        StructVal, ValueVarType, VarType,
    },
    lexer::Token,
};
use chumsky::{
    primitive::{just, todo},
    select, IterParser, Parser,
};
use snailquote::unescape;

pub fn bop_parser<'i>() -> impl Parser<'i, ParserInput<'i>, BOp, ParserError<'i, Token<'i>>> + Clone
{
    select! { Token::BOp(bop) => bop }
}

pub fn id_parser<'i>() -> impl Parser<'i, ParserInput<'i>, Id, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::Id(id) => id.into() }
}

pub fn number_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, NumericLiteral<'i>, ParserError<'i, Token<'i>>> + Clone {
    let int = select! { Token::IntVal(i) => NumericLiteral::Int(i) };
    let float = select! { Token::FloatVal(f) => NumericLiteral::Float(f) };

    int.or(float)
}

pub fn signed_number_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, PrimitiveVal<'i>, ParserError<'i, Token<'i>>> + Clone {
    numeric_unary_op_parser()
        .or_not()
        .then(number_parser())
        .map(|(op, num)| PrimitiveVal::Number(op, num))
}

pub fn string_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, String, ParserError<'i, Token<'i>>> + Clone {
    // TODO: We could try_map the unescape
    select! { Token::Str(s) => s}.map(|string| {
        let unescaped = unescape(string).expect("Invalid string");
        unescaped
    })
}

pub fn bool_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, BoolLiteral, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::BoolVal(b) => b.into() }
}

pub fn negated_bool_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, PrimitiveVal<'i>, ParserError<'i, Token<'i>>> + Clone {
    boolean_unary_op_parser()
        .or_not()
        .then(bool_parser())
        .map(|(op, num)| PrimitiveVal::Boolean(op, num))
}

pub fn char_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, char, ParserError<'i, Token<'i>>> + Clone {
    // TODO: We could try_map the char unescape
    select! { Token::Char(c) => c }.map(|c| {
        // TODO: Find if there's a better way to do this
        let c_hack = format!(
            "\"{}\"",
            c.strip_suffix("'").unwrap().strip_prefix("'").unwrap()
        );
        let unescaped_ch = unescape(&c_hack).expect("Invalid escaped char");
        let c = unescaped_ch
            .chars()
            .next()
            .expect("Invalid empty char literal");
        c
    })
}

pub fn array_val_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, ArrayVal<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    // Main definition
    let arr = expr
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
        .map(ArrayVal);

    // Definitions
    expr_parser();

    arr
}

pub fn struct_val_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, StructVal<'i>, ParserError<'i, Token<'i>>> + Clone {
    // Declarations
    let expr = EXPR_PARSER.read().unwrap().clone();

    let id_prop_name = id_parser().map(PropertyName::Id);
    let str_prop_name = string_parser().map(PropertyName::String);
    let prop_name = id_prop_name.or(str_prop_name);
    let prop = prop_name.then_ignore(just(Token::Colon)).then(expr);

    // Main definition
    let struct_p = prop
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(StructVal);

    // Definitions
    expr_parser();

    struct_p
}

pub fn primitive_val_parser<'i: 'static>(
) -> impl Parser<'i, ParserInput<'i>, PrimitiveVal<'i>, ParserError<'i, Token<'i>>> + Clone {
    let num = signed_number_parser();
    let bool = negated_bool_parser();
    let char = char_parser().map(PrimitiveVal::Char);
    let string = string_parser().map(PrimitiveVal::String);
    let arr = array_val_parser().map(PrimitiveVal::Array);
    let struct_v = struct_val_parser().map(PrimitiveVal::Struct);

    num.or(bool).or(char).or(string).or(arr).or(struct_v)
}

pub fn scope_specifier_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, ScopeSpecifier, ParserError<'i, Token<'i>>> + Clone {
    select! { Token::ScopeSpecifier(ss) => ss }
}

pub fn var_type_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, VarType, ParserError<'i, Token<'i>>> + Clone {
    let vt = select! { Token::VarType(vt) => vt };
    let custom = select! { Token::Id(id) => id }.map(|id| VarType::Custom(id.into()));

    vt.or(custom)
}

pub fn pointer_symbol_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, BOp, ParserError<'i, Token<'i>>> + Clone {
    bop_parser().filter(|b| matches!(b, BOp::Mul))
}

pub fn value_var_type_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, ValueVarType, ParserError<'i, Token<'i>>> + Clone {
    // TODO: We could try_map in count to convert usize to u8
    let ptr_nesting = pointer_symbol_parser().repeated().count();
    let vtype = var_type_parser();
    let array_nesting = just(Token::LSqBracket)
        .then(just(Token::RSqBracket))
        .repeated()
        .count();

    ptr_nesting
        .then(vtype)
        .then(array_nesting)
        .map(|((ptr_nl, vtype), arr_nl)| ValueVarType {
            array_nesting_level: arr_nl.try_into().expect("Array nesting too deep"),
            vtype,
            pointer_nesting_level: ptr_nl.try_into().expect("Pointer nesting too deep"),
        })
}

pub fn type_specifier_parser<'i>(
) -> impl Parser<'i, ParserInput<'i>, ValueVarType, ParserError<'i, Token<'i>>> + Clone {
    just(Token::Colon).ignore_then(value_var_type_parser())
}
