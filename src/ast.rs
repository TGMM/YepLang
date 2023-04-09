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
    Custom(Id),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveVal {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Array(ArrayVal),
    Struct(StructVal),
}
impl From<PrimitiveVal> for Expr {
    fn from(value: PrimitiveVal) -> Self {
        Expr {
            lhs: Exp::Term(Term::Factor(Factor::PrimitiveVal(value))),
            rhs: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayVal(pub Vec<PrimitiveVal>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructVal(pub Vec<(PropertyName, PrimitiveVal)>);

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
pub struct Id(pub String);
impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Id(value.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(VarDecl),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyName {
    Id(Id),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Destructure {
    Id(Id),
    Array(Vec<Destructure>),
    Object(PropertyName, Option<Id>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub scope_spec: ScopeSpecifier,
    pub destructure: Destructure,
    pub var_type: Option<VarType>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub lhs: Exp,
    pub rhs: Option<(CmpOp, Exp)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpBOp {
    pub lhs: Term,
    pub op: ExpOp,
    pub rhs: Exp,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Term(Term),
    BOp(Box<ExpBOp>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TermBOp {
    pub lhs: Term,
    pub op: TermOp,
    pub rhs: Term,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Factor(Factor),
    BOp(Box<TermBOp>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor {
    ParenExpr(Box<Expr>),
    PrimitiveVal(PrimitiveVal),
    Id(Id),
}
