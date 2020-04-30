use crate::ast::*;

pub struct DesugarPostprocessVisitor {}

impl DesugarPostprocessVisitor {
    pub fn postprocess(ast: &Program) -> VisitorResult {
        let mut visitor = DesugarPostprocessVisitor {};
        visitor.visit_program(&ast)
    }

    fn ungenerate_retvar(b: &NodeBox) {
        if let Some(stmt) = b.borrow().downcast_ref::<IfExpr>() {
            stmt.generate_retvar.set(false);
        }
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

    fn visit_import(&mut self, n: &ImportStatement, b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_class(&mut self, n: &ClassStatement, b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_class_init(&mut self, n: &ClassInitExpr, b: &NodeBox) -> VisitorResult {
        for (_, boxed) in &n.inits {
            boxed.visit(self)?;
        }
        Ok(())
    }

    fn visit_defstmt(&mut self, n: &DefStatement, b: &NodeBox) -> VisitorResult {
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
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_whileexpr(&mut self, n: &WhileExpr, b: &NodeBox) -> VisitorResult {
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
            if idx != last_idx && !n.generate_retvar.get() {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        let last_idx = n.elses.len().wrapping_sub(1);
        for (idx, expr) in n.elses.iter().enumerate() {
            if idx != last_idx && !n.generate_retvar.get() {
                Self::ungenerate_retvar(expr);
            }
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_callexpr(&mut self, n: &CallExpr, b: &NodeBox) -> VisitorResult {
        for expr in &n.args {
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_letexpr(&mut self, n: &LetExpr, b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        n.right.visit(self)?;
        Ok(())
    }

    fn visit_binexpr(&mut self, n: &BinExpr, b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        n.right.visit(self)?;
        Ok(())
    }

    fn visit_asexpr(&mut self, n: &AsExpr, b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        Ok(())
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        for arm in &n.right {
            if let MemberExprArm::Index(boxed) = arm {
                boxed.visit(self)?;
            }
        }
        Ok(())
    }

    fn visit_value(&mut self, n: &Value, b: &NodeBox) -> VisitorResult {
        match &n {
            Value::Slice(vec) => {
                for expr in vec {
                    expr.visit(self)?;
                }
            }
            _ => (),
        }
        Ok(())
    }

    fn visit_typeid(&mut self, n: &TypeId, b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_break(&mut self, n: &BreakExpr, b: &NodeBox) -> VisitorResult {
        Ok(())
    }

    fn visit_borrow(&mut self, n: &BorrowExpr, b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        Ok(())
    }

    fn visit_deref(&mut self, n: &DerefExpr, b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        Ok(())
    }
}
