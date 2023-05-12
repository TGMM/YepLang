use crate::lexer::Token;
use chumsky::pratt::{Associativity, InfixOperator, InfixPrecedence};
use logos::Lexer;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum VarType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F32,
    F64,
    Void,
    Boolean,
    Char,
    String,
    Custom(Id),
}
impl VarType {
    pub fn is_signed(&self) -> bool {
        match self {
            VarType::I8 | VarType::I16 | VarType::I32 | VarType::I64 | VarType::I128 => true,
            VarType::U8 | VarType::U16 | VarType::U32 | VarType::U64 | VarType::U128 => false,
            _ => panic!("Only integer types can be signed"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueVarType {
    pub vtype: VarType,
    // TODO: In case this is an array, it should specify quantity and
    // type (Ex. arr: i32[10])
    /// This is the nesting level of the array.
    /// 0 means the type is not an array
    pub array_nesting_level: u8,
    /// This is the nesting level of the pointer.
    /// 0 means the type is not a pointer
    pub pointer_nesting_level: u8,
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

#[derive(Debug, Clone, Copy, PartialEq)]
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
impl<'input> From<NumericLiteral<'input>> for PrimitiveVal<'input> {
    fn from(value: NumericLiteral<'input>) -> Self {
        PrimitiveVal::Number(None, value)
    }
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
        "i8" => VarType::I8,
        "u8" => VarType::U8,
        "i16" => VarType::I16,
        "u16" => VarType::U16,
        "i32" => VarType::I32,
        "u32" => VarType::U32,
        "i64" => VarType::I64,
        "u64" => VarType::U64,
        "i128" => VarType::I128,
        "u128" => VarType::U128,
        "f32" => VarType::F32,
        "f64" => VarType::F64,
        "void" => VarType::Void,
        "boolean" => VarType::Boolean,
        "char" => VarType::Char,
        "string" => VarType::String,
        custom => VarType::Custom(custom.into()),
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
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
pub struct TopBlock<'input>(pub Block<'input>);

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'input> {
    pub stmts: Vec<Stmt<'input>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'input> {
    Assignment(Assignment<'input>),
    Expr(Expr<'input>),
    ClassDecl(ClassDecl<'input>),
    FnDecl(FnDecl<'input>),
    For(For<'input>),
    While(While<'input>),
    DoWhile(DoWhile<'input>),
    If(If<'input>),
    Block(Block<'input>),
    VarDecl(VarDecl<'input>),
    ExternDecl(ExternDecl),
    Return(Return<'input>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return<'input>(pub Option<Expr<'input>>);

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
pub struct VarDeclAssignment<'input> {
    pub destructure: Destructure<'input>,
    pub var_type: Option<ValueVarType>,
    pub expr: Option<Expr<'input>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl<'input> {
    pub scope_spec: ScopeSpecifier,
    pub decl_assignments: Vec<VarDeclAssignment<'input>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    pub fn is_cmp(&self) -> bool {
        match self {
            BOp::Gt | BOp::Gte | BOp::Lt | BOp::Lte | BOp::Ne | BOp::Eq => true,
            _ => false,
        }
    }
}

impl<'input> InfixOperator<Expr<'input>> for BOp {
    type Strength = u8;

    fn precedence(&self) -> InfixPrecedence<Self::Strength> {
        use BOp::*;
        match self {
            Add | Sub => InfixPrecedence::new(3, Associativity::Left),
            Mul | Div | Mod => InfixPrecedence::new(5, Associativity::Left),
            Pow => InfixPrecedence::new(7, Associativity::Left),
            Gt | Gte | Lt | Lte | Ne | Eq => InfixPrecedence::new(1, Associativity::Left),
        }
    }

    fn build_expression(self, lhs: Expr<'input>, rhs: Expr<'input>) -> Expr<'input> {
        let bexpr = BExpr { lhs, op: self, rhs };
        Expr::BinaryExpr(Box::new(bexpr))
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
    Indexing(Box<Indexing<'input>>),
    MemberAccess(Box<MemberAcess<'input>>),
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
    pub decl_stmt: Option<Box<Stmt<'a>>>,
    pub cmp_expr: Option<Expr<'a>>,
    pub postfix_stmt: Option<Box<Stmt<'a>>>,
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
    pub block: ClassBlock<'a>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ClassBlock<'a> {
    pub class_stmts: Vec<ClassStmt<'a>>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDecl<'a> {
    pub id: Id,
    pub vtype: Option<ValueVarType>,
    pub assigned_expr: Expr<'a>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ClassStmt<'a> {
    // This includes the constructor
    Method(MethodDecl<'a>),
    Accessor,
    Property(PropertyDecl<'a>),
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
impl<'a> From<MethodDecl<'a>> for FnDecl<'a> {
    fn from(method_decl: MethodDecl<'a>) -> Self {
        FnDecl {
            fn_id: method_decl.method_id,
            args: method_decl.args,
            ret_type: method_decl.ret_type,
            block: method_decl.block,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment<'a> {
    pub assignee_expr: Expr<'a>,
    pub bop: Option<BOp>,
    pub assigned_expr: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExternType {
    Type(ValueVarType),
    Spread,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternDecl {
    pub ret_type: ValueVarType,
    pub fn_id: Id,
    pub arg_types: Vec<ExternType>,
}
