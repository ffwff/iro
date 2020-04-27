use crate::ast::*;

pub struct PostprocessVisitor {
    file: usize,
}

impl PostprocessVisitor {
    pub fn postprocess(ast: &Program, file: usize) {
        if file == 0 {
            return;
        }
        let mut visitor = PostprocessVisitor { file };
        visitor.visit_program(&ast).unwrap();
    }

    fn fill_box(&self, boxed: &NodeBox) {
        boxed.span_ref().update(|mut span| {
            span.file = self.file;
            span
        });
    }
}

impl Visitor for PostprocessVisitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult {
        for node in &n.exprs {
            node.visit(self)?;
        }
        Ok(())
    }

    fn visit_import(&mut self, n: &ImportStatement, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }

    fn visit_class(&mut self, n: &ClassStatement, b: &NodeBox) -> VisitorResult {
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
        for expr in &n.exprs {
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
        for expr in &n.exprs {
            expr.visit(self)?;
        }
        Ok(())
    }

    fn visit_ifexpr(&mut self, n: &IfExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        n.cond.visit(self)?;
        for expr in &n.exprs {
            expr.visit(self)?;
        }
        for expr in &n.elses {
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
        self.fill_box(b);
        Ok(())
    }

    fn visit_break(&mut self, n: &BreakExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }
}
