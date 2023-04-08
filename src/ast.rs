use crate::lexer::Token;
use logos::Lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    I32,
    I64,
    F32,
    F64,
    Boolean,
    Char,
    String,
}

pub fn str_to_var_type<'input>(lex: &Lexer<'input, Token<'input>>) -> VarType {
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

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeSpecifier {
    Var,
    Const,
    Let,
}

pub fn str_to_scope_spec<'input>(lex: &Lexer<'input, Token<'input>>) -> ScopeSpecifier {
    let scope_str = lex.slice();
    match scope_str {
        "var" => ScopeSpecifier::Var,
        "const" => ScopeSpecifier::Const,
        "let" => ScopeSpecifier::Let,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpOp {
    Add,
    Sub,
}

pub fn str_to_exp_op<'input>(lex: &Lexer<'input, Token<'input>>) -> ExpOp {
    let exp_op_str = lex.slice();
    match exp_op_str {
        "+" => ExpOp::Add,
        "-" => ExpOp::Sub,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermOp {
    Mul,
    Div,
    Mod,
}

pub fn str_to_term_op<'input>(lex: &Lexer<'input, Token<'input>>) -> TermOp {
    let term_op_str = lex.slice();
    match term_op_str {
        "*" => TermOp::Mul,
        "/" => TermOp::Div,
        "%" => TermOp::Mod,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    Lt,
    Lte,
    Gt,
    Gte,
    Ne,
    Eq,
}

pub fn str_to_cmp_op<'input>(lex: &Lexer<'input, Token<'input>>) -> CmpOp {
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

#[derive(Debug, Clone, PartialEq)]
struct Id(String);
impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Id(value.to_string())
    }
}
