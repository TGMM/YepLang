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
pub struct ValueVarType {
    pub vtype: VarType,
    // TODO: This should give the nesting level
    // instead of just saying if it's an array or not
    pub is_array: bool,
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

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral(pub bool);
impl From<&str> for BoolLiteral {
    fn from(value: &str) -> Self {
        match value {
            "true" => BoolLiteral(true),
            "false" => BoolLiteral(false),
            _ => panic!("Invalid value for bool"),
        }
    }
}

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
        Expr::PrimitiveVal(value)
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
        custom => VarType::Custom(custom.into()),
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
pub struct Id(pub String);
impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Id(value.to_string())
    }
}
impl<'a> From<Id> for Expr<'a> {
    fn from(value: Id) -> Self {
        Expr::Id(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'input> {
    pub stmts: Vec<Stmt<'input>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'input> {
    Assignment(Assignment<'input>),
    FnCall(FnCall<'input>),
    Expr(Expr<'input>),
    ClassDecl(ClassDecl<'input>),
    FnDecl(FnDecl<'input>),
    MethodDecl(MethodDecl<'input>),
    For(For<'input>),
    While(While<'input>),
    DoWhile(DoWhile<'input>),
    If(If<'input>),
    Block(Block<'input>),
    VarDecl(VarDecl<'input>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyName {
    Id(Id),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDestructure<'input> {
    /// Left hand side
    pub name: PropertyName,
    /// Right hand side
    pub alias: Option<Destructure<'input>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Destructure<'input> {
    Id(Id),
    MemberAccess(MemberAcess<'input>),
    Array(Vec<Destructure<'input>>),
    Object(Vec<PropertyDestructure<'input>>),
}
impl<'input> From<Id> for Destructure<'input> {
    fn from(value: Id) -> Self {
        Destructure::Id(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl<'input> {
    pub scope_spec: ScopeSpecifier,
    pub destructure: Destructure<'input>,
    pub var_type: Option<ValueVarType>,
    pub expr: Expr<'input>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Gt,
    Gte,
    Lt,
    Lte,
    Ne,
    Eq,
}
impl BOp {
    fn infix_binding_power(&self) -> u8 {
        match self {
            BOp::Add => 1,
            BOp::Sub => 1,
            BOp::Mul => 3,
            BOp::Div => 3,
            BOp::Mod => 3,
            BOp::Pow => 5,
            BOp::Gt => 7,
            BOp::Gte => 7,
            BOp::Lt => 7,
            BOp::Lte => 7,
            BOp::Ne => 7,
            BOp::Eq => 7,
        }
    }
}
impl From<&str> for BOp {
    fn from(value: &str) -> Self {
        match value {
            "+" => BOp::Add,
            "-" => BOp::Sub,
            "*" => BOp::Mul,
            "/" => BOp::Div,
            "%" => BOp::Mod,
            "**" => BOp::Pow,
            ">" => BOp::Gt,
            ">=" => BOp::Gte,
            "<" => BOp::Lt,
            "<=" => BOp::Lte,
            "!=" => BOp::Ne,
            "==" => BOp::Eq,
            _ => todo!(),
        }
    }
}
pub fn str_to_bop<'input>(lex: &Lexer<'input, Token<'input>>) -> BOp {
    let op = lex.slice();
    op.into()
}

impl From<BOp> for NumericUnaryOp {
    fn from(value: BOp) -> Self {
        match value {
            BOp::Add => NumericUnaryOp::Plus,
            BOp::Sub => NumericUnaryOp::Minus,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BExpr<'input> {
    pub lhs: Expr<'input>,
    pub op: BOp,
    pub rhs: Expr<'input>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'input> {
    ParenExpr(Option<UnaryOp>, Box<Expr<'input>>),
    BinaryExpr(Box<BExpr<'input>>),
    PrimitiveVal(PrimitiveVal<'input>),
    FnCall(Box<FnCall<'input>>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhile<'a> {
    pub do_block: Block<'a>,
    pub while_cond: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While<'a> {
    pub while_cond: Expr<'a>,
    pub block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For<'a> {
    pub decl_expr: Option<Expr<'a>>,
    pub cmp_expr: Option<Expr<'a>>,
    pub postfix_expr: Option<Expr<'a>>,
    pub block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If<'a> {
    pub if_expr: Expr<'a>,
    pub if_block: Block<'a>,
    pub else_if: Vec<ElseIf<'a>>,
    pub else_b: Option<Block<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseIf<'a> {
    pub else_expr: Expr<'a>,
    pub else_block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl<'a> {
    pub class_id: Id,
    pub extended_class_id: Option<Id>,
    pub block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAcess<'a> {
    pub accessed: Expr<'a>,
    pub property: Id,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl<'a> {
    pub fn_id: Id,
    pub args: Vec<(Destructure<'a>, ValueVarType)>,
    pub ret_type: Option<ValueVarType>,
    pub block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDecl<'a> {
    pub method_id: Id,
    pub args: Vec<(Destructure<'a>, ValueVarType)>,
    pub ret_type: Option<ValueVarType>,
    pub block: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment<'a> {
    pub destructure: Destructure<'a>,
    pub assigned_expr: Expr<'a>,
}
