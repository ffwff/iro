use crate::ast::*;

pub struct PreprocessVisitor {
    file: usize,
}

impl PreprocessVisitor {
    pub fn postprocess(ast: &Program, file: usize) -> VisitorResult {
        let mut visitor = PreprocessVisitor { file };
        visitor.visit_program(&ast)
    }

    fn fill_box(&self, boxed: &NodeBox) {
        boxed.span_ref().update(|mut span| {
            span.file = self.file;
            span
        });
    }

    fn ungenerate_retvar(b: &NodeBox) {
        b.generate_retvar.set(false);
    }
}

impl Visitor for PreprocessVisitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult {
        for node in &n.exprs {
            Self::ungenerate_retvar(node);
            node.visit(self)?;
        }
        Ok(())
    }

    fn visit_import(&mut self, _n: &ImportStatement, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }

    fn visit_class(&mut self, _n: &ClassStatement, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }

    fn visit_class_init(&mut self, n: &ClassInitExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        for (_, boxed) in &n.inits {
            boxed.visit(self)?;
        }
        Ok(())
    }

    fn visit_defstmt(&mut self, n: &DefStatement, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        let last_idx = n.exprs.len().wrapping_sub(1);
        for (idx, expr) in n.exprs.iter().enumerate() {
            if idx != last_idx {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_return(&mut self, n: &ReturnExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_whileexpr(&mut self, n: &WhileExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.cond.visit(self)?;
        let last_idx = n.exprs.len().wrapping_sub(1);
        for (idx, expr) in n.exprs.iter().enumerate() {
            if idx != last_idx {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_ifexpr(&mut self, n: &IfExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.cond.visit(self)?;
        let last_idx = n.exprs.len().wrapping_sub(1);
        for (idx, expr) in n.exprs.iter().enumerate() {
            if idx != last_idx && !b.generate_retvar.get() {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        let last_idx = n.elses.len().wrapping_sub(1);
        for (idx, expr) in n.elses.iter().enumerate() {
            if idx != last_idx && !b.generate_retvar.get() {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_callexpr(&mut self, n: &CallExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        for expr in &n.args {
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_letexpr(&mut self, n: &LetExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.left.visit(self)?;
        n.right.visit(self)?;
        Ok(())
    }

    fn visit_binexpr(&mut self, n: &BinExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.left.visit(self)?;
        n.right.visit(self)?;
        Ok(())
    }

    fn visit_asexpr(&mut self, n: &AsExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.left.visit(self)?;
        Ok(())
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.left.visit(self)?;
        for arm in &n.right {
            if let MemberExprArm::Index(boxed) = arm {
                boxed.visit(self)?;
            }
        }
        Ok(())
    }

    fn visit_value(&mut self, n: &Value, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        if let Value::Slice(vec) = &n {
            for expr in vec {
                expr.visit(self)?;
            }
        }
        Ok(())
    }

    fn visit_typeid(&mut self, _n: &TypeId, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }

    fn visit_break(&mut self, _n: &BreakExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }

    fn visit_borrow(&mut self, n: &BorrowExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_deref(&mut self, n: &DerefExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_unary(&mut self, n: &UnaryExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.expr.visit(self)?;
        Ok(())
    }
}
