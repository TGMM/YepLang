use crate::ast::{
    str_to_cmp_op, str_to_exp_op, str_to_scope_spec, str_to_term_op, str_to_var_type,
};
use crate::ast::{CmpOp, ExpOp, ScopeSpecifier, TermOp, VarType};
use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    #[regex(r#"[\p{L}_][\p{L}\d_]*"#)]
    Id(&'input str),
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
    Str(&'input str),
    #[regex(r#"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?"#)]
    FloatVal(&'input str),
    #[regex(r#"[-+]?\d+"#, priority = 2)]
    IntVal(&'input str),
    #[token("i32", str_to_var_type)]
    #[token("i64", str_to_var_type)]
    #[token("f32", str_to_var_type)]
    #[token("f64", str_to_var_type)]
    #[token("boolean", str_to_var_type)]
    #[token("char", str_to_var_type)]
    #[token("string", str_to_var_type)]
    VarType(VarType),
    #[token("var", str_to_scope_spec)]
    #[token("const", str_to_scope_spec)]
    #[token("let", str_to_scope_spec)]
    ScopeSpecifier(ScopeSpecifier),
    #[token("+", str_to_exp_op)]
    #[token("-", str_to_exp_op)]
    ExpOp(ExpOp),
    #[token("*", str_to_term_op)]
    #[token("/", str_to_term_op)]
    #[token("%", str_to_term_op)]
    TermOp(TermOp),
    #[token(">", str_to_cmp_op)]
    #[token(">=", str_to_cmp_op)]
    #[token("<", str_to_cmp_op)]
    #[token("<=", str_to_cmp_op)]
    #[token("!=", str_to_cmp_op)]
    #[token("==", str_to_cmp_op)]
    CmpOp(CmpOp),
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
    #[error]
    #[regex(r"[\s\f\r]+", logos::skip)]
    Error,
}

#[cfg(test)]
mod test {
    use super::Token;
    use crate::lexer::{CmpOp, ExpOp, ScopeSpecifier, TermOp, VarType};
    use logos::Logos;

    #[test]
    fn type_lexing() {
        let input = "i32 i64 f32 f64 boolean char string";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Token::VarType(VarType::I32),
                Token::VarType(VarType::I64),
                Token::VarType(VarType::F32),
                Token::VarType(VarType::F64),
                Token::VarType(VarType::Boolean),
                Token::VarType(VarType::Char),
                Token::VarType(VarType::String),
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
                Token::ScopeSpecifier(ScopeSpecifier::Var),
                Token::ScopeSpecifier(ScopeSpecifier::Const),
                Token::ScopeSpecifier(ScopeSpecifier::Let),
            ]
        );
    }

    #[test]
    fn exp_op_lexing() {
        let input = "+ -";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[Token::ExpOp(ExpOp::Add), Token::ExpOp(ExpOp::Sub),]
        );
    }

    #[test]
    fn term_op_lexing() {
        let input = "* / %";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Token::TermOp(TermOp::Mul),
                Token::TermOp(TermOp::Div),
                Token::TermOp(TermOp::Mod),
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
                Token::CmpOp(CmpOp::Lt),
                Token::CmpOp(CmpOp::Lte),
                Token::CmpOp(CmpOp::Gt),
                Token::CmpOp(CmpOp::Gte),
                Token::CmpOp(CmpOp::Ne),
                Token::CmpOp(CmpOp::Eq),
            ]
        );
    }

    #[test]
    fn keyword_lexing() {
        let input = "if else do while for break continue function class extends ; : , ( ) { } [ ]";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[
                Token::If,
                Token::Else,
                Token::Do,
                Token::While,
                Token::For,
                Token::Break,
                Token::Continue,
                Token::Function,
                Token::Class,
                Token::Extends,
                Token::StmtEnd,
                Token::Colon,
                Token::Comma,
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::LSqBracket,
                Token::RSqBracket
            ]
        );
    }

    #[test]
    fn number_lexing() {
        let input = "1 2.0 3.1 4.234 5 123456789 9223372036854775807 1.7976931348623157E+308";
        let lex = Token::lexer(input);
        let tokens: Vec<Token> = lex.into_iter().collect();

        let i64_max_str = i64::MAX.to_string();

        assert_eq!(
            tokens,
            vec![
                Token::IntVal("1"),
                Token::FloatVal("2.0"),
                Token::FloatVal("3.1"),
                Token::FloatVal("4.234"),
                Token::IntVal("5"),
                Token::IntVal("123456789"),
                Token::IntVal(i64_max_str.as_str()),
                Token::FloatVal("1.7976931348623157E+308"),
            ]
        );
    }

    #[test]
    fn string_parser_test() {
        let input = r#""This is a test string\n" "This is another test string\t""#;
        let lex = Token::lexer(input);
        let tokens: Vec<Token> = lex.into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Token::Str("\"This is a test string\\n\""),
                Token::Str("\"This is another test string\\t\"")
            ]
        );
    }

    #[test]
    fn id_parser_test() {
        let input = "x y z my_var my_super_long_var_name";
        let lex = Token::lexer(input);
        let tokens: Vec<Token> = lex.into_iter().collect();

        assert_eq!(
            tokens,
            vec![
                Token::Id("x".into()),
                Token::Id("y".into()),
                Token::Id("z".into()),
                Token::Id("my_var".into()),
                Token::Id("my_super_long_var_name".into()),
            ]
        );
    }
}
