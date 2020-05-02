use crate::ast::*;

pub struct DesugarPostprocessVisitor {}

impl DesugarPostprocessVisitor {
    pub fn postprocess(ast: &Program) -> VisitorResult {
        let mut visitor = DesugarPostprocessVisitor {};
        visitor.visit_program(&ast)
    }

    fn ungenerate_retvar(b: &NodeBox) {
        b.generate_retvar.set(false);
    }
}

impl Visitor for DesugarPostprocessVisitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult {
        for node in &n.exprs {
            Self::ungenerate_retvar(node);
            node.visit(self)?;
        }
        Ok(())
    }

    fn visit_import(&mut self, _n: &ImportStatement, _b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_class(&mut self, _n: &ClassStatement, _b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_class_init(&mut self, n: &ClassInitExpr, _b: &NodeBox) -> VisitorResult {
        for (_, boxed) in &n.inits {
            boxed.visit(self)?;
        }
        Ok(())
    }

    fn visit_defstmt(&mut self, n: &DefStatement, _b: &NodeBox) -> VisitorResult {
        let last_idx = n.exprs.len().wrapping_sub(1);
        for (idx, expr) in n.exprs.iter().enumerate() {
            if idx != last_idx {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_return(&mut self, n: &ReturnExpr, _b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_whileexpr(&mut self, n: &WhileExpr, _b: &NodeBox) -> VisitorResult {
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

    fn visit_callexpr(&mut self, n: &CallExpr, _b: &NodeBox) -> VisitorResult {
        for expr in &n.args {
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_letexpr(&mut self, n: &LetExpr, _b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        n.right.visit(self)?;
        Ok(())
    }

    fn visit_binexpr(&mut self, n: &BinExpr, _b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        n.right.visit(self)?;
        Ok(())
    }

    fn visit_asexpr(&mut self, n: &AsExpr, _b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        Ok(())
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, _b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        for arm in &n.right {
            if let MemberExprArm::Index(boxed) = arm {
                boxed.visit(self)?;
            }
        }
        Ok(())
    }

    fn visit_value(&mut self, n: &Value, _b: &NodeBox) -> VisitorResult {
        if let Value::Slice(vec) = &n {
            for expr in vec {
                expr.visit(self)?;
            }
        }
        Ok(())
    }

    fn visit_typeid(&mut self, _n: &TypeId, _b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_break(&mut self, _n: &BreakExpr, _b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_borrow(&mut self, n: &BorrowExpr, _b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_deref(&mut self, n: &DerefExpr, _b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_unary(&mut self, n: &UnaryExpr, _b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        Ok(())
    }
}
