use crate::{
    ast::{CmpOp, ExpOp, Id, ScopeSpecifier, TermOp, VarType},
    lexer::Token,
};
use std::fmt;

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Id(id) => write!(f, "{}", id),
            Token::Str(string) => write!(f, "{}", string),
            Token::Char(c) => write!(f, "{}", c),
            Token::FloatVal(fl) => write!(f, "{}", fl),
            Token::IntVal(i) => write!(f, "{}", i),
            Token::VarType(vt) => write!(f, "{}", vt),
            Token::ScopeSpecifier(ss) => ss.fmt(f),
            Token::ExpOp(exp_op) => exp_op.fmt(f),
            Token::TermOp(term_op) => term_op.fmt(f),
            Token::CmpOp(cmp_op) => cmp_op.fmt(f),
            Token::AssignmentEq => write!(f, "="),
            Token::StmtEnd => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::LBracket => write!(f, "{{"),
            Token::RBracket => write!(f, "}}"),
            Token::LSqBracket => write!(f, "["),
            Token::RSqBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Class => write!(f, "class"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::For => write!(f, "for"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Do => write!(f, "do"),
            Token::While => write!(f, "while"),
            Token::Function => write!(f, "function"),
            Token::Extends => write!(f, "extends"),
            Token::Error => write!(f, "unknown token"),
        }
    }
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CmpOp::Lt => write!(f, "<"),
            CmpOp::Lte => write!(f, "<="),
            CmpOp::Gt => write!(f, ">"),
            CmpOp::Gte => write!(f, ">="),
            CmpOp::Ne => write!(f, "!="),
            CmpOp::Eq => write!(f, "=="),
        }
    }
}

impl fmt::Display for ExpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpOp::Add => write!(f, "+"),
            ExpOp::Sub => write!(f, "-"),
        }
    }
}

impl fmt::Display for TermOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TermOp::Mul => write!(f, "*"),
            TermOp::Div => write!(f, "/"),
            TermOp::Mod => write!(f, "%"),
        }
    }
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarType::I32 => write!(f, "i32"),
            VarType::I64 => write!(f, "i64"),
            VarType::F32 => write!(f, "f32"),
            VarType::F64 => write!(f, "f64"),
            VarType::Boolean => write!(f, "boolean"),
            VarType::Char => write!(f, "char"),
            VarType::String => write!(f, "string"),
            VarType::Custom(cs) => write!(f, "{}", cs),
        }
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for ScopeSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScopeSpecifier::Var => write!(f, "var"),
            ScopeSpecifier::Const => write!(f, "const"),
            ScopeSpecifier::Let => write!(f, "let"),
        }
    }
}
