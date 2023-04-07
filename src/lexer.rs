use logos::{Lexer, Logos, Skip};
use snailquote::unescape;

struct LexerInfo {
    pub line: u32,
}
impl Default for LexerInfo {
    fn default() -> Self {
        Self { line: 1 }
    }
}

fn count_newline(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;

    Skip
}

#[derive(Debug, PartialEq)]
enum VarType {
    I32,
    I64,
    F32,
    F64,
    Boolean,
    Char,
    String,
}

fn str_to_var_type(lex: &mut Lexer<Token>) -> VarType {
    let type_str = lex.slice();
    match type_str {
        "i32" => VarType::I32,
        "i64" => VarType::I64,
        "f32" => VarType::F32,
        "f64" => VarType::F64,
        "boolean" => VarType::Boolean,
        "char" => VarType::Char,
        "string" => VarType::String,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
enum ScopeSpecifier {
    Var,
    Const,
    Let,
}

fn str_to_scope_spec(lex: &mut Lexer<Token>) -> ScopeSpecifier {
    let scope_str = lex.slice();
    match scope_str {
        "var" => ScopeSpecifier::Var,
        "const" => ScopeSpecifier::Const,
        "let" => ScopeSpecifier::Let,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
enum ExpOp {
    Add,
    Sub,
}

fn str_to_exp_op(lex: &mut Lexer<Token>) -> ExpOp {
    let exp_op_str = lex.slice();
    match exp_op_str {
        "+" => ExpOp::Add,
        "-" => ExpOp::Sub,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
enum TermOp {
    Mul,
    Div,
    Mod,
}

fn str_to_term_op(lex: &mut Lexer<Token>) -> TermOp {
    let term_op_str = lex.slice();
    match term_op_str {
        "*" => TermOp::Mul,
        "/" => TermOp::Div,
        "%" => TermOp::Mod,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
enum CmpOp {
    Lt,
    Lte,
    Gt,
    Gte,
    Ne,
    Eq,
}

fn str_to_cmp_op(lex: &mut Lexer<Token>) -> CmpOp {
    let cmp_op_str = lex.slice();
    match cmp_op_str {
        ">" => CmpOp::Lt,
        ">=" => CmpOp::Lte,
        "<" => CmpOp::Gt,
        "<=" => CmpOp::Gte,
        "!=" => CmpOp::Ne,
        "==" => CmpOp::Eq,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
struct Id(String);
impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Id(value.to_string())
    }
}

fn str_to_id(lex: &mut Lexer<Token>) -> Id {
    let slice = lex.slice();
    Id(slice.to_string())
}

fn str_to_string(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();

    unescape(slice).ok()
}

fn str_to_float(lex: &mut Lexer<Token>) -> Option<f64> {
    let slice = lex.slice();
    let f: f64 = slice.parse().ok()?;
    Some(f)
}

fn str_to_int(lex: &mut Lexer<Token>) -> Option<i64> {
    let slice = lex.slice();
    let i: i64 = slice.parse().ok()?;
    Some(i)
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexerInfo)]
enum Token {
    #[regex(r#"[\p{L}_][\p{L}\d_]*"#, str_to_id)]
    Id(Id),
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, str_to_string)]
    Str(String),
    #[regex(r#"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?"#, str_to_float)]
    FloatVal(f64),
    #[regex(r#"[-+]?\d+"#, str_to_int, priority = 2)]
    IntVal(i64),
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
    #[regex(r"[\n]+", count_newline)]
    #[regex(r"[ \t\f\r]+", logos::skip)]
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

        assert_eq!(
            tokens,
            vec![
                Token::IntVal(1),
                Token::FloatVal(2.0),
                Token::FloatVal(3.1),
                Token::FloatVal(4.234),
                Token::IntVal(5),
                Token::IntVal(123456789),
                Token::IntVal(i64::MAX),
                Token::FloatVal(f64::MAX),
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
                Token::Str("This is a test string\n".to_string()),
                Token::Str("This is another test string\t".to_string())
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
