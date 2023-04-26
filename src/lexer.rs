use crate::ast::{str_to_bool_uop, str_to_bop, str_to_scope_spec, str_to_var_type, BoolUnaryOp};
use crate::ast::{BOp, ScopeSpecifier, VarType};
use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[\s\f\r]+")]
pub enum Token<'input> {
    #[regex(r#"[\p{L}_][\p{L}\d_]*"#)]
    Id(&'input str),
    #[regex(r#""(?:[^"\\]|\\t|\\u|\\n|\\")*""#)]
    Str(&'input str),
    #[regex(r#"'(?:[^'\\]|\\t|\\u(?:[0-9a-fA-F]{4})|\\n|\\r|\\'|\\\\)'"#)]
    Char(&'input str),
    #[regex(r#"[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?"#)]
    FloatVal(&'input str),
    #[regex(r#"\d+"#, priority = 2)]
    IntVal(&'input str),
    #[token("true")]
    #[token("false")]
    BoolVal(&'input str),
    #[token("i8", str_to_var_type)]
    #[token("u8", str_to_var_type)]
    #[token("i16", str_to_var_type)]
    #[token("u16", str_to_var_type)]
    #[token("i32", str_to_var_type)]
    #[token("u32", str_to_var_type)]
    #[token("i64", str_to_var_type)]
    #[token("u64", str_to_var_type)]
    #[token("i128", str_to_var_type)]
    #[token("u128", str_to_var_type)]
    #[token("f32", str_to_var_type)]
    #[token("f64", str_to_var_type)]
    #[token("void", str_to_var_type)]
    #[token("boolean", str_to_var_type)]
    #[token("char", str_to_var_type)]
    #[token("string", str_to_var_type)]
    VarType(VarType),
    #[token("var", str_to_scope_spec)]
    #[token("const", str_to_scope_spec)]
    #[token("let", str_to_scope_spec)]
    ScopeSpecifier(ScopeSpecifier),
    #[token("!", str_to_bool_uop)]
    BoolUnaryOp(BoolUnaryOp),
    #[token("+", str_to_bop)]
    #[token("-", str_to_bop)]
    #[token("*", str_to_bop)]
    #[token("**", str_to_bop)]
    #[token("/", str_to_bop)]
    #[token("%", str_to_bop)]
    #[token(">", str_to_bop)]
    #[token(">=", str_to_bop)]
    #[token("<", str_to_bop)]
    #[token("<=", str_to_bop)]
    #[token("!=", str_to_bop)]
    #[token("==", str_to_bop)]
    BOp(BOp),
    #[token("=")]
    AssignmentEq,
    #[token(";")]
    StmtEnd,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("{")]
    LBracket,
    #[token("}")]
    RBracket,
    #[token("[")]
    LSqBracket,
    #[token("]")]
    RSqBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(".")]
    Dot,
    #[token("class")]
    Class,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("for")]
    For,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("do")]
    Do,
    #[token("while")]
    While,
    #[token("function")]
    Function,
    #[token("extends")]
    Extends,
    #[token("return")]
    Return,
    #[token("extern")]
    Extern,
    #[token("...")]
    Spread,
}

#[cfg(test)]
mod test {
    use super::Token;
    use crate::{
        ast::BoolUnaryOp,
        lexer::{BOp, ScopeSpecifier, VarType},
    };
    use logos::Logos;
    use Token::*;

    #[test]
    fn type_lexing() {
        let input = "i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 f32 f64 void boolean char string";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Ok(VarType(VarType::I8)),
                Ok(VarType(VarType::U8)),
                Ok(VarType(VarType::I16)),
                Ok(VarType(VarType::U16)),
                Ok(VarType(VarType::I32)),
                Ok(VarType(VarType::U32)),
                Ok(VarType(VarType::I64)),
                Ok(VarType(VarType::U64)),
                Ok(VarType(VarType::I128)),
                Ok(VarType(VarType::U128)),
                Ok(VarType(VarType::F32)),
                Ok(VarType(VarType::F64)),
                Ok(VarType(VarType::Void)),
                Ok(VarType(VarType::Boolean)),
                Ok(VarType(VarType::Char)),
                Ok(VarType(VarType::String)),
            ]
        );
    }

    #[test]
    fn scope_lexing() {
        let input = "var const let";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Ok(ScopeSpecifier(ScopeSpecifier::Var)),
                Ok(ScopeSpecifier(ScopeSpecifier::Const)),
                Ok(ScopeSpecifier(ScopeSpecifier::Let)),
            ]
        );
    }

    #[test]
    fn exp_op_lexing() {
        let input = "+ -";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(&tokens, &[Ok(BOp(BOp::Add)), Ok(BOp(BOp::Sub)),]);
    }

    #[test]
    fn term_op_lexing() {
        let input = "* / % **";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Ok(BOp(BOp::Mul)),
                Ok(BOp(BOp::Div)),
                Ok(BOp(BOp::Mod)),
                Ok(BOp(BOp::Pow))
            ]
        );
    }

    #[test]
    fn cmp_op_lexing() {
        let input = "> >= < <= != ==";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Ok(BOp(BOp::Gt)),
                Ok(BOp(BOp::Gte)),
                Ok(BOp(BOp::Lt)),
                Ok(BOp(BOp::Lte)),
                Ok(BOp(BOp::Ne)),
                Ok(BOp(BOp::Eq)),
            ]
        );
    }

    #[test]
    fn bool_unary_op_lexing() {
        let input = "!";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(&tokens, &[Ok(BoolUnaryOp(BoolUnaryOp::Not)),]);
    }

    #[test]
    fn keyword_lexing() {
        let input =
            "if else do while for break continue function class extends return extern ; : , ( ) { } [ ] = ...";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Ok(If),
                Ok(Else),
                Ok(Do),
                Ok(While),
                Ok(For),
                Ok(Break),
                Ok(Continue),
                Ok(Function),
                Ok(Class),
                Ok(Extends),
                Ok(Return),
                Ok(Extern),
                Ok(StmtEnd),
                Ok(Colon),
                Ok(Comma),
                Ok(LParen),
                Ok(RParen),
                Ok(LBracket),
                Ok(RBracket),
                Ok(LSqBracket),
                Ok(RSqBracket),
                Ok(AssignmentEq),
                Ok(Spread),
            ]
        );
    }

    #[test]
    fn number_lexing() {
        let input = "1 2.0 3.1 4.234 5 123456789 9223372036854775807 1.7976931348623157E+308";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.into_iter().collect();

        let i64_max_str = i64::MAX.to_string();

        assert_eq!(
            tokens,
            vec![
                Ok(IntVal("1")),
                Ok(FloatVal("2.0")),
                Ok(FloatVal("3.1")),
                Ok(FloatVal("4.234")),
                Ok(IntVal("5")),
                Ok(IntVal("123456789")),
                Ok(IntVal(i64_max_str.as_str())),
                Ok(FloatVal("1.7976931348623157E+308")),
            ]
        );
    }

    #[test]
    fn string_val_parser_test() {
        let input = r#""This is a test string\n" "This is another test string\t""#;
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Str("\"This is a test string\\n\"")),
                Ok(Str("\"This is another test string\\t\""))
            ]
        );
    }

    #[test]
    fn bool_val_parser_test() {
        let input = r#"true false"#;
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.into_iter().collect();

        assert_eq!(tokens, vec![Ok(BoolVal("true")), Ok(BoolVal("false"))]);
    }

    #[test]
    fn char_val_parser_test() {
        let input = r#"'a' '\u0041' '\u004A'"#;
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Char("'a'")),
                Ok(Char("'\\u0041'")),
                Ok(Char("'\\u004A'"))
            ]
        );
    }

    #[test]
    fn id_parser_test() {
        let input = "x y z my_var my_super_long_var_name";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Id("x".into())),
                Ok(Id("y".into())),
                Ok(Id("z".into())),
                Ok(Id("my_var".into())),
                Ok(Id("my_super_long_var_name".into())),
            ]
        );
    }

    #[test]
    // Included to increase code coverage
    fn debug_test() {
        let input = "x ! 10 10.5 > \\";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.into_iter().collect();

        let _tok_clone = tokens[0].clone();

        for tok in tokens {
            match tok {
                Ok(t) => {
                    dbg!(t);
                }
                Err(err) => {
                    dbg!(err);
                }
            }
        }
    }
}
