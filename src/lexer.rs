use crate::ast::{
    str_to_bool_uop, str_to_bop, str_to_scope_spec, str_to_var_type, BOp, BoolUnaryOp, Id,
};
use crate::ast::{ScopeSpecifier, VarType};
use crate::spanned_ast::SpannedAstNode;
use logos::{Lexer, Logos};

pub fn set_decorator_parsing<'input>(lex: &mut Lexer<'input, Token<'input>>) {
    // If we find an @ symbol we start parsing a decorator
    lex.extras.is_parsing_decorator = true;
}

pub fn check_for_llvm_parsing<'input>(lex: &mut Lexer<'input, Token<'input>>) -> Id {
    let id_str = lex.slice();

    // After the first decorator id, we're no longer searching for
    // the LLVM decorator
    lex.extras.is_parsing_decorator = false;
    // This id_str corresponds to the decorator
    // id
    if id_str == "llvm" {
        lex.extras.is_llvm_decorator = true;
    }

    Id {
        id_str: id_str.to_string(),
        span: lex.span().into(),
    }
}

pub fn parse_llvm_str<'input>(lex: &mut Lexer<'input, Token<'input>>) {
    if lex.extras.is_llvm_decorator {
        lex.extras.is_llvm_decorator = false;

        let start = lex.span().start + 1;
        let mut end: usize = start;

        let mut lbracket_count = 0;
        for c in lex.remainder().chars() {
            if c == '{' {
                lbracket_count += 1;
            } else if c == '}' && lbracket_count == 0 {
                break;
            } else if c == '}' {
                lbracket_count -= 1;
            }

            lex.bump_no_span(c.len_utf8());
            end += 1;
        }

        lex.add_intermediate_token(
            Some(Ok(Token::LlvmIr(&lex.source()[start..end]))),
            start..end,
        );
    }
}

// If we ever find the zero width char
// error it anyway
pub fn error_zero_width_char<'input>(_: &Lexer<'input, Token<'input>>) -> Result<&'input str, ()> {
    Err(())
}

pub struct LexerExtras {
    pub is_parsing_decorator: bool,
    pub is_llvm_decorator: bool,
}

impl Default for LexerExtras {
    fn default() -> Self {
        Self {
            is_parsing_decorator: false,
            is_llvm_decorator: false,
        }
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(extras = LexerExtras)]
#[logos(skip r"[\s\f\r]+")]
pub enum Token<'input> {
    #[regex(r#"[\p{L}_][\p{L}\d_]*"#, check_for_llvm_parsing)]
    Id(Id),
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
    BoolUnaryOp(SpannedAstNode<BoolUnaryOp>),
    #[token("+", str_to_bop)]
    #[token("-", str_to_bop)]
    #[token("*", str_to_bop)]
    #[token("/", str_to_bop)]
    #[token("%", str_to_bop)]
    #[token(">", str_to_bop)]
    #[token(">=", str_to_bop)]
    #[token("<", str_to_bop)]
    #[token("<=", str_to_bop)]
    #[token("!=", str_to_bop)]
    #[token("==", str_to_bop)]
    #[token("||", str_to_bop)]
    BOp(SpannedAstNode<BOp>),
    #[token("=")]
    AssignmentEq,
    #[token(";")]
    StmtEnd,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("{", parse_llvm_str)]
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
    #[token("as")]
    As,
    #[token("@", set_decorator_parsing)]
    At,
    #[token("&")]
    Ampersand,
    // Put a zero-width space here since it asks for something
    #[token("​", error_zero_width_char)]
    LlvmIr(&'input str),
}

#[cfg(test)]
mod test {
    use super::Token;
    use crate::{
        ast::{BOp, BoolUnaryOp, Id},
        lexer::{ScopeSpecifier, VarType},
        spanned_ast::SpannedAstNode,
    };
    use chumsky::span::SimpleSpan;
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

        use BOp::*;
        assert_eq!(
            &tokens,
            &[
                Ok(BOp(SpannedAstNode {
                    node: Add,
                    span: SimpleSpan::new(0, 1)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Sub,
                    span: SimpleSpan::new(2, 3)
                })),
            ]
        );
    }

    #[test]
    fn term_op_lexing() {
        let input = "* / %";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        use BOp::*;
        assert_eq!(
            &tokens,
            &[
                Ok(BOp(SpannedAstNode {
                    node: Mul,
                    span: SimpleSpan::new(0, 1)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Div,
                    span: SimpleSpan::new(2, 3)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Mod,
                    span: SimpleSpan::new(4, 5)
                })),
            ]
        );
    }

    #[test]
    fn cmp_op_lexing() {
        let input = "> >= < <= != ==";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        use BOp::*;
        assert_eq!(
            &tokens,
            &[
                Ok(BOp(SpannedAstNode {
                    node: Gt,
                    span: SimpleSpan::new(0, 1)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Gte,
                    span: SimpleSpan::new(2, 4)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Lt,
                    span: SimpleSpan::new(5, 6)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Lte,
                    span: SimpleSpan::new(7, 9)
                })),
                Ok(BOp(SpannedAstNode {
                    node: Ne,
                    span: SimpleSpan::new(10, 12)
                })),
                Ok(BOp(SpannedAstNode {
                    node: CmpEq,
                    span: SimpleSpan::new(13, 15)
                })),
            ]
        );
    }

    #[test]
    fn bool_unary_op_lexing() {
        let input = "!";
        let lex = Token::lexer(input);
        let tokens: Vec<_> = lex.collect();

        assert_eq!(
            &tokens,
            &[Ok(BoolUnaryOp(SpannedAstNode {
                node: BoolUnaryOp::Not,
                span: SimpleSpan::new(0, 1)
            })),]
        );
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
                Ok(Id(Id {
                    id_str: "x".to_string(),
                    span: SimpleSpan::new(0, 1)
                })),
                Ok(Id(Id {
                    id_str: "y".to_string(),
                    span: SimpleSpan::new(2, 3)
                })),
                Ok(Id(Id {
                    id_str: "z".to_string(),
                    span: SimpleSpan::new(4, 5)
                })),
                Ok(Id(Id {
                    id_str: "my_var".to_string(),
                    span: SimpleSpan::new(6, 12)
                })),
                Ok(Id(Id {
                    id_str: "my_super_long_var_name".to_string(),
                    span: SimpleSpan::new(13, 35)
                })),
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
