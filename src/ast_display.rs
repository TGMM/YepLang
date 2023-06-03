use crate::{
    ast::{BOp, BoolUnaryOp, Id, ScopeSpecifier, ValueVarType, VarType},
    lexer::Token,
    spanned_ast::SpannedAstNode,
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
            Token::BoolUnaryOp(op) => match op.node {
                BoolUnaryOp::Not => write!(f, "!"),
            },
            Token::Dot => write!(f, "."),
            Token::Extern => write!(f, "extern"),
            Token::Spread => write!(f, "..."),
            Token::At => write!(f, "@"),
            Token::LlvmIr(_) => write!(f, "inline llvm"),
            Token::As => write!(f, "as"),
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
            CmpEq => write!(f, "=="),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarType::I8 => write!(f, "i8"),
            VarType::U8 => write!(f, "u8"),
            VarType::I16 => write!(f, "i16"),
            VarType::U16 => write!(f, "u16"),
            VarType::I32 => write!(f, "i32"),
            VarType::U32 => write!(f, "u32"),
            VarType::I64 => write!(f, "i64"),
            VarType::U64 => write!(f, "u64"),
            VarType::I128 => write!(f, "i128"),
            VarType::U128 => write!(f, "u128"),
            VarType::F32 => write!(f, "f32"),
            VarType::F64 => write!(f, "f64"),
            VarType::Void => write!(f, "void"),
            VarType::Boolean => write!(f, "boolean"),
            VarType::Char => write!(f, "char"),
            VarType::String => write!(f, "string"),
            VarType::Custom(cs) => write!(f, "{}", cs),
        }
    }
}

impl fmt::Display for ValueVarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut output = String::new();

        for _ in 0..self.pointer_nesting_level {
            output += "*";
        }

        output += self.vtype.to_string().as_str();

        for dim in self.array_dimensions.iter() {
            output += format!("[{}]", dim).as_str();
        }

        write!(f, "{}", output)
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id_str)
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

impl<T: fmt::Display> fmt::Display for SpannedAstNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}
