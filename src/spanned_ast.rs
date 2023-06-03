use chumsky::span::SimpleSpan;

use crate::ast::{
    Assignment, Block, ClassDecl, DoWhile, Expr, ExternDecl, FnDef, For, Id, If, Return, Stmt,
    TopBlock, VarDecl, While,
};

pub struct SpannedAstNode<T> {
    node: T,
    span: SimpleSpan,
}

pub trait GetSpan {
    fn get_span(&self) -> SimpleSpan;
}

impl GetSpan for TopBlock<'_> {
    fn get_span(&self) -> SimpleSpan {
        self.0.get_span()
    }
}

impl GetSpan for Block<'_> {
    fn get_span(&self) -> SimpleSpan {
        let lbracket = self.lbracket.unwrap_or(SimpleSpan::new(0, 0));
        let rbracket = self.rbracket.unwrap_or(SimpleSpan::new(0, 0));

        if self.stmts.is_empty() {
            return SimpleSpan::new(lbracket.start, rbracket.end);
        }

        let length = self.stmts.len();
        let first = self.stmts[0].get_span().start.max(lbracket.start);
        let last = self.stmts[length - 1].get_span().end.max(rbracket.end);

        SimpleSpan::new(first, last)
    }
}

impl GetSpan for Stmt<'_> {
    fn get_span(&self) -> SimpleSpan {
        match self {
            Stmt::Assignment(a) => a.get_span(),
            Stmt::Expr(e) => e.get_span(),
            Stmt::ClassDecl(cd) => cd.get_span(),
            Stmt::FnDef(fd) => fd.get_span(),
            Stmt::For(f) => f.get_span(),
            Stmt::While(w) => w.get_span(),
            Stmt::DoWhile(dw) => dw.get_span(),
            Stmt::If(i) => i.get_span(),
            Stmt::Block(b) => b.get_span(),
            Stmt::VarDecl(vd) => vd.get_span(),
            Stmt::ExternDecl(ed) => ed.get_span(),
            Stmt::Return(r) => r.get_span(),
        }
    }
}

impl GetSpan for Assignment<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for Expr<'_> {
    fn get_span(&self) -> SimpleSpan {
        match self {
            Expr::ParenExpr(_, _) => todo!(),
            Expr::BinaryExpr(_) => todo!(),
            Expr::PrimitiveVal(_) => todo!(),
            Expr::FnCall(_) => todo!(),
            Expr::Indexing(_) => todo!(),
            Expr::MemberAccess(_) => todo!(),
            Expr::Id(id) => id.get_span(),
            Expr::Cast(_) => todo!(),
        }
    }
}

impl GetSpan for Id {
    fn get_span(&self) -> SimpleSpan {
        self.span
    }
}

impl GetSpan for ClassDecl<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for FnDef<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for For<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for While<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for DoWhile<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for If<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for VarDecl<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for ExternDecl {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}

impl GetSpan for Return<'_> {
    fn get_span(&self) -> SimpleSpan {
        todo!()
    }
}
