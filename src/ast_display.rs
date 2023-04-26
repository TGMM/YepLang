use crate::{
    ast::{BOp, BoolUnaryOp, Id, ScopeSpecifier, VarType},
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
            Token::BoolVal(b) => write!(f, "{}", b),
            Token::VarType(vt) => write!(f, "{}", vt),
            Token::ScopeSpecifier(ss) => ss.fmt(f),
            Token::BOp(bop) => bop.fmt(f),
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
            Token::Return => write!(f, "return"),
            Token::BoolUnaryOp(op) => match op {
                BoolUnaryOp::Not => write!(f, "!"),
            },
            Token::Dot => write!(f, "."),
        }
    }
}

impl fmt::Display for BOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BOp::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
            Pow => write!(f, "**"),
            Gt => write!(f, ">"),
            Gte => write!(f, ">="),
            Lt => write!(f, "<"),
            Lte => write!(f, "<="),
            Ne => write!(f, "!="),
            Eq => write!(f, "=="),
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
