use crate::ast::*;
use crate::compiler::sources;
use bit_set::BitSet;
use std::path::PathBuf;

pub struct PreprocessState {
    imported: BitSet<u32>,
    total_imported_statements: Vec<NodeBox>,
    prelude: Option<PathBuf>,
}

impl PreprocessState {
    pub fn new(prelude: Option<PathBuf>) -> Self {
        Self {
            imported: BitSet::new(),
            total_imported_statements: vec![],
            prelude,
        }
    }
}

pub struct PreprocessVisitor<'a> {
    file: sources::FileIndex,
    // Working directory of the current file, must be absolute
    working_path: Option<PathBuf>,
    state: Option<&'a RefCell<PreprocessState>>,
    sources: &'a mut sources::Sources,
}

impl<'a> PreprocessVisitor<'a> {
    pub fn postprocess(
        program: &mut Program,
        file: sources::FileIndex,
        working_path: Option<PathBuf>,
        state: Option<&'a RefCell<PreprocessState>>,
        sources: &'a mut sources::Sources,
    ) -> VisitorResult {
        let mut visitor = Self {
            file,
            working_path,
            state,
            sources,
        };
        visitor.visit_program(program)
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

    fn import(&mut self, working_path: PathBuf) -> VisitorResult {
        if let Some(mut state) = self.state.as_ref().map(|x| x.borrow_mut()) {
            let sources = &mut self.sources;

            let (index, _) = sources
                .read(&working_path)
                .map_err(|error| compiler::Error::io_error(error))?;

            if state.imported.insert(index) {
                std::mem::drop(state);
                let ast = compiler::parse_file_to_ast(
                    index,
                    working_path,
                    sources,
                    self.state.clone().unwrap(),
                )
                .map_err(|err| {
                    err.span.map(|mut span| {
                        span.file = index;
                        span
                    });
                    err
                })?;
                let mut state = self.state.as_ref().unwrap().borrow_mut();
                state
                    .total_imported_statements
                    .extend(ast.exprs.into_iter().filter(|ast| ast.can_import()));
            }
        }
        Ok(())
    }
}

impl<'a> Visitor for PreprocessVisitor<'a> {
    fn visit_program(&mut self, n: &mut Program) -> VisitorResult {
        for node in &n.exprs {
            Self::ungenerate_retvar(node);
            node.visit(self)?;
        }
        if self.file == 0 {
            if let Some(state_cell) = self.state {
                {
                    let state = state_cell.borrow();
                    if let Some(prelude) = &state.prelude {
                        let mut working_path = std::env::current_dir()
                            .map_err(|error| compiler::Error::io_error(error))?;
                        working_path.push(prelude);
                        working_path = std::fs::canonicalize(working_path)
                            .map_err(|error| compiler::Error::io_error(error))?;
                        std::mem::drop(state);
                        self.import(working_path)?;
                    }
                }

                let mut state = state_cell.borrow_mut();
                n.exprs.extend(std::mem::replace(
                    &mut state.total_imported_statements,
                    vec![],
                ));
            }
        }
        Ok(())
    }

    fn visit_import(&mut self, n: &ImportStatement, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        let mut working_path = self.working_path.clone().unwrap();
        working_path.pop();
        working_path.push(&n.path);
        working_path.set_extension("iro");
        working_path = std::fs::canonicalize(working_path)
            .map_err(|error| compiler::Error::io_error(error))?;
        self.import(working_path)?;
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

    fn visit_modstmt(&mut self, n: &ModStatement, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        for expr in &n.exprs {
            expr.visit(self)?;
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

    fn visit_path(&mut self, n: &PathExpr, b: &NodeBox) -> VisitorResult {
        self.fill_box(b);
        Ok(())
    }
}
