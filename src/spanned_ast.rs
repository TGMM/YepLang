use chumsky::span::SimpleSpan;

use crate::ast::{
    ArrayVal, Assignment, BExpr, Block, BoolUnaryOp, Casting, ClassBlock, ClassDecl, Destructure,
    DoWhile, Else, ElseIf, Expr, ExternDecl, FnCall, FnDef, FnType, For, Id, If, Indexing,
    MemberAcess, NumericUnaryOp, PrimitiveVal, Return, Stmt, TopBlock, UnaryOp, ValueVarType,
    VarDecl, VarDeclAssignment, While,
};

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedAstNode<T, SpanType = SimpleSpan> {
    pub node: T,
    pub span: SpanType,
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
        let first = self.stmts[0].get_span().start.min(lbracket.start);
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
        let start = self.assignee_expr.get_span().start;
        let end = self.assigned_expr.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for Expr<'_> {
    fn get_span(&self) -> SimpleSpan {
        match self {
            Expr::ParenExpr(uop, expr) => {
                let expr_span = expr.get_span();

                let start = uop
                    .as_ref()
                    .map(|op| op.get_span().start)
                    .unwrap_or(expr_span.start);
                let end = expr_span.end;

                SimpleSpan::new(start, end)
            }
            Expr::BinaryExpr(bexpr) => bexpr.get_span(),
            Expr::PrimitiveVal(pv) => pv.get_span(),
            Expr::FnCall(fn_call) => fn_call.get_span(),
            Expr::Indexing(idxing) => idxing.get_span(),
            Expr::MemberAccess(ma) => ma.get_span(),
            Expr::Id(id) => id.get_span(),
            Expr::Cast(cast) => cast.get_span(),
        }
    }
}

impl GetSpan for Casting<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.casted.get_span().start;
        let end = self.cast_type.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for FnCall<'_> {
    fn get_span(&self) -> SimpleSpan {
        let fn_expr_span = self.fn_expr.get_span();
        let start = fn_expr_span.start;
        let end = self.rparen.end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for MemberAcess<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.accessed.get_span().start;
        let end = self.property.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for SpannedAstNode<PrimitiveVal<'_>> {
    fn get_span(&self) -> SimpleSpan {
        self.span
    }
}

impl GetSpan for ArrayVal<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.lsqbracket.start;
        let end = self.rsqbracket.end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for Indexing<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.indexed.get_span().start;
        let end = self.indexer.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for BExpr<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.lhs.get_span().start;
        let end = self.rhs.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for SpannedAstNode<ValueVarType> {
    fn get_span(&self) -> SimpleSpan {
        self.span
    }
}

impl GetSpan for UnaryOp {
    fn get_span(&self) -> SimpleSpan {
        match self {
            UnaryOp::Numeric(n) => n.get_span(),
            UnaryOp::Bool(b) => b.get_span(),
        }
    }
}

impl GetSpan for SpannedAstNode<NumericUnaryOp> {
    fn get_span(&self) -> SimpleSpan {
        self.span
    }
}

impl GetSpan for SpannedAstNode<BoolUnaryOp> {
    fn get_span(&self) -> SimpleSpan {
        self.span
    }
}

impl GetSpan for Id {
    fn get_span(&self) -> SimpleSpan {
        self.span
    }
}

impl GetSpan for ClassDecl<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.class_kw.start;
        let end = self.block.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for ClassBlock<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.lbracket.start;
        let end = self.rbracket.end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for FnDef<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self
            .fn_kw
            .unwrap_or(self.fn_signature.fn_id.get_span())
            .start;

        let end = match &self.fn_type {
            FnType::Native(n) => n.get_span().end,
            FnType::InlineLlvmIr(ir) => ir.rbracket.end,
        };

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for For<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.for_kw.start;
        let end = self.block.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for While<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.while_kw.start;
        let end = self.block.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for DoWhile<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.do_kw.start;
        let end = self.while_cond.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for If<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.if_kw.start;

        let else_if_end = self.else_if.last().as_ref().map(|ei| ei.get_span().end);
        let else_end = self.else_.as_ref().map(|e| e.get_span().end);
        let if_b_end = self.if_block.get_span().end;

        let end = else_end.unwrap_or(else_if_end.unwrap_or(if_b_end));

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for Else<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.else_kw.start;
        let end = self.else_b.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for ElseIf<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.else_kw.start;
        let end = self.else_block.get_span().end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for Destructure<'_> {
    fn get_span(&self) -> SimpleSpan {
        match self {
            Destructure::Id(id) => id.get_span(),
            Destructure::MemberAccess(_) => todo!(),
            Destructure::Array(_) => todo!(),
            Destructure::Object(_) => todo!(),
        }
    }
}

impl GetSpan for VarDeclAssignment<'_> {
    fn get_span(&self) -> SimpleSpan {
        let destructure_span = self.destructure.get_span();
        let start = destructure_span.start;

        let expr_end = self.expr.as_ref().map(|e| e.get_span().end);
        let vvt_end = self.var_type.as_ref().map(|vt| vt.get_span().end);

        let end = expr_end.unwrap_or(vvt_end.unwrap_or(destructure_span.end));

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for VarDecl<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.scope_spec.span.start;
        let da = self.decl_assignments.last().unwrap();

        let end = da
            .expr
            .as_ref()
            .map(|e| e.get_span().end)
            .unwrap_or(da.destructure.get_span().end);

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for ExternDecl {
    fn get_span(&self) -> SimpleSpan {
        let start = self.extern_kw.start;
        let end = self.rparen.end;

        SimpleSpan::new(start, end)
    }
}

impl GetSpan for Return<'_> {
    fn get_span(&self) -> SimpleSpan {
        let start = self.ret_kw.start;
        let end = self
            .ret_val
            .as_ref()
            .map(|rv| rv.get_span().end)
            .unwrap_or(self.ret_kw.end);

        SimpleSpan::new(start, end)
    }
}
