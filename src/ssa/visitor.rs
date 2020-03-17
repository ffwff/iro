use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::borrow::Borrow;
use crate::ast::*;
use crate::ssa::isa::*;
use crate::ssa::env::Env;
use crate::utils::RcWrapper;

#[derive(Debug, Clone)]
pub struct TopLevelInfo {
    pub defstmts: HashMap<Rc<str>, Rc<DefStatement>>,
    pub func_contexts: HashMap<Rc<FunctionName>, Context>,
}

impl TopLevelInfo {
    pub fn new() -> Self {
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct SSAVisitor {
    context: Context,
    envs: Vec<Env>,
    top_level: RcWrapper<TopLevelInfo>,
}

impl SSAVisitor {
    pub fn new() -> Self {
        Self {
            context: Context::new(Rc::from("main")),
            envs: vec![],
            top_level: RcWrapper::new(TopLevelInfo::new()),
        }
    }

    pub fn with_context(context: Context, top_level: RcWrapper<TopLevelInfo>) -> Self {
        Self {
            context,
            envs: vec![],
            top_level,
        }
    }

    pub fn into_context(self) -> Context {
        self.context
    }

    fn with_block_mut<T, U>(&mut self, mut callback: T) -> U where T: FnMut(&mut Block) -> U {
        let block = self.context.block_mut();
        callback(block)
    }
}

impl Visitor for SSAVisitor {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult {
        let mut outerstmts = vec![];
        for expr in &n.exprs {
            if let Some(defstmt) = expr.borrow().downcast_ref::<DefStatement>() {
                self.top_level.with_mut(|top_level| {
                    top_level.defstmts.insert(defstmt.id.clone(),
                            expr.rc().downcast_rc::<DefStatement>().unwrap());
                });
            } else {
                outerstmts.push(expr);
            }
        }

        self.context.new_block();
        for node in &outerstmts {
            node.visit(self)?;
        }
        Ok(())
    }

    fn visit_defstmt(&mut self,  n: &DefStatement) -> VisitorResult {
        unimplemented!()
    }

    fn visit_return(&mut self,   n: &ReturnExpr) -> VisitorResult {
        unimplemented!()
    }

    fn visit_ifexpr(&mut self,   n: &IfExpr) -> VisitorResult {
        unimplemented!()
    }
    
    fn visit_callexpr(&mut self, n: &CallExpr) -> VisitorResult {
        if let Some(id) = n.callee.borrow().downcast_ref::<Value>() {
            if let Value::Identifier(id) = &id {
                let id = &id.id;
                let start_var = self.context.variables.len();
                let mut arg_types = vec![];
                for arg in &n.args {
                    arg.visit(self)?;
                    arg_types.push(self.context.variables.last().unwrap().clone());
                }
                let func_name = Rc::new(FunctionName {
                    name: id.clone(),
                    arg_types
                });
                let end_var = self.context.variables.len();

                let rettype = self.top_level.with_mut(|top_level| {
                    if let Some(context) = top_level.func_contexts.get(&func_name) {
                        Ok(Some(context.rettype.clone()))
                    } else if let Some(defstmt) = top_level.defstmts.get(id) {
                        Ok(None)
                    } else {
                        Err(Error::UnknownIdentifier(id.clone()))
                    }
                })?;
                
                let retvar = self.context.insert_var(
                    if let Some(rettype) = rettype {
                        rettype
                    } else {
                        let visitor = RefCell::new(Some({
                            let func_context = Context::new(id.clone());
                            let mut visitor = SSAVisitor::with_context(func_context, self.top_level.clone());
                            let defstmt = self.top_level.with(|top_level| {
                                top_level.defstmts.get(id).cloned().unwrap()
                            });
                            visitor.visit_defstmt(defstmt.borrow());
                            (func_name.clone(), visitor)
                        }));
                        self.top_level.with_mut(move |top_level| {
                            let (func_name, visitor) = visitor.replace(None).unwrap();
                            let context = visitor.into_context();
                            let rettype = context.rettype.clone();
                            top_level.func_contexts.insert(func_name, context);
                            rettype
                        })
                    }
                );
                self.with_block_mut(|block| {
                    block.ins.push(Ins { retvar, typed: InsType::Call((
                            func_name.clone(),
                            (start_var..end_var).collect())) });
                });
            }
        }
        unimplemented!()
        // if let Some(context) = self.func_contexts.get()
    }

    fn visit_letexpr(&mut self,  n: &LetExpr) -> VisitorResult {
        unimplemented!()
    }

    fn visit_binexpr(&mut self,  n: &BinExpr) -> VisitorResult {
        unimplemented!()
    }

    fn visit_value(&mut self,    n: &Value) -> VisitorResult {
        match n {
            Value::Int(x) => {
                let retvar = self.context.insert_var(Type::I32);
                self.with_block_mut(|block| {
                    block.ins.push(Ins { retvar, typed: InsType::LoadI32(*x) });
                });
                Ok(())
            }
            _ => unimplemented!()
        }
    }
    
    fn visit_typeid(&mut self,   n: &TypeId) -> VisitorResult {
        unimplemented!()
    }
}