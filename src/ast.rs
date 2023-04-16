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
pub enum NumericLiteral<'input> {
    Int(&'input str),
    Float(&'input str),
}
#[derive(Debug, Clone, PartialEq)]
pub enum NumericUnaryOp {
    Plus,
    Minus,
}
impl From<ExpOp> for NumericUnaryOp {
    fn from(value: ExpOp) -> Self {
        match value {
            ExpOp::Add => NumericUnaryOp::Plus,
            ExpOp::Sub => NumericUnaryOp::Minus,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral(pub bool);
#[derive(Debug, Clone, PartialEq)]
pub enum BoolUnaryOp {
    Not,
}

pub fn str_to_bool_uop<'input>(lex: &Lexer<'input, Token<'input>>) -> BoolUnaryOp {
    let exp_op_str = lex.slice();
    match exp_op_str {
        "!" => BoolUnaryOp::Not,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Numeric(NumericUnaryOp),
    Bool(BoolUnaryOp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveVal<'input> {
    Number(Option<NumericUnaryOp>, NumericLiteral<'input>),
    Boolean(Option<BoolUnaryOp>, BoolLiteral),
    Char(char),
    String(String),
    Array(ArrayVal<'input>),
    Struct(StructVal<'input>),
}
impl<'input> From<PrimitiveVal<'input>> for Expr<'input> {
    fn from(value: PrimitiveVal<'input>) -> Self {
        Expr {
            lhs: Exp::Term(Term::Factor(Factor::PrimitiveVal(value))),
            rhs: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayVal<'input>(pub Vec<Expr<'input>>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructVal<'input>(pub Vec<(PropertyName, Expr<'input>)>);

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
pub struct Block<'input> {
    pub stmts: Vec<Stmt<'input>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'input> {
    VarDecl(VarDecl<'input>),
    Block(Block<'input>),
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
pub struct VarDecl<'input> {
    pub scope_spec: ScopeSpecifier,
    pub destructure: Destructure,
    pub var_type: Option<VarType>,
    pub expr: Expr<'input>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'input> {
    pub lhs: Exp<'input>,
    pub rhs: Option<(CmpOp, Exp<'input>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpBOp<'input> {
    pub lhs: Exp<'input>,
    pub op: ExpOp,
    pub rhs: Exp<'input>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Exp<'input> {
    Term(Term<'input>),
    BOp(Box<ExpBOp<'input>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TermBOp<'input> {
    pub lhs: Term<'input>,
    pub op: TermOp,
    pub rhs: Term<'input>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Term<'input> {
    Factor(Factor<'input>),
    BOp(Box<TermBOp<'input>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor<'input> {
    ParenExpr(Option<UnaryOp>, Box<Expr<'input>>),
    PrimitiveVal(PrimitiveVal<'input>),
    Id(Id),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Indexing<'a> {
    pub indexed: Expr<'a>,
    pub indexer: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall<'a> {
    pub fn_expr: Expr<'a>,
    pub args: Vec<Expr<'a>>,
}
