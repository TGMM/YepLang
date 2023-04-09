use crate::{
    ast::{CmpOp, ExpOp},
    lexer::Token,
};
use std::fmt;

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Id(id) => write!(f, "{}", id),
            Token::Str(_) => todo!(),
            Token::FloatVal(_) => todo!(),
            Token::IntVal(_) => todo!(),
            Token::VarType(_) => todo!(),
            Token::ScopeSpecifier(_) => todo!(),
            Token::ExpOp(exp_op) => exp_op.fmt(f),
            Token::TermOp(_) => todo!(),
            Token::CmpOp(cmp_op) => cmp_op.fmt(f),
            Token::AssignmentEq => write!(f, "="),
            Token::StmtEnd => write!(f, ";"),
            Token::Colon => todo!(),
            Token::Comma => todo!(),
            Token::LBracket => todo!(),
            Token::RBracket => todo!(),
            Token::LSqBracket => todo!(),
            Token::RSqBracket => todo!(),
            Token::LParen => todo!(),
            Token::RParen => todo!(),
            Token::Class => todo!(),
            Token::Break => todo!(),
            Token::Continue => todo!(),
            Token::For => todo!(),
            Token::If => todo!(),
            Token::Else => todo!(),
            Token::Do => todo!(),
            Token::While => todo!(),
            Token::Function => todo!(),
            Token::Extends => todo!(),
            Token::Error => write!(f, "invalid token"),
        }
    }
}

impl<'a> fmt::Display for CmpOp {
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

impl<'a> fmt::Display for ExpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpOp::Add => write!(f, "+"),
            ExpOp::Sub => write!(f, "-"),
        }
    }
}
