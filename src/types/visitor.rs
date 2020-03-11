use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use crate::ast::*;
use crate::ast;
use crate::types::types::*;
use crate::env::env::Env;

pub struct TypeVisitor {
    envs: Vec<Env>,
}

impl<'a> TypeVisitor {
    pub fn new() -> Self {
        TypeVisitor {
            envs: vec![],
        }
    }

    fn enter_scope(&mut self) {
        self.envs.push(Env::new());
    }

    fn leave_scope(&mut self) -> Option<Env> {
        self.envs.pop()
    }
    
    fn scope(&'a mut self) -> &'a mut Env {
        self.envs.last_mut().unwrap()
    }

    fn getvar(&mut self, s : &str) -> Option<Variable> {
        for scope in self.envs.iter().rev() {
            if let Some(var) = scope.getvar(s) {
                return Some(var)
            }
        }
        None
    }
}

impl Visitor for TypeVisitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult {
        self.enter_scope();
        for node in &n.exprs {
            node.visit(&node, self)?;
        }
        self.leave_scope();
        Ok(())
    }

    fn visit_defstmt(&mut self, b: &NodeBox, n: &DefStatement) -> VisitorResult {
        // TODO
        Ok(())
    }

    fn visit_return(&mut self, b: &NodeBox, n: &ReturnExpr) -> VisitorResult {
        // TODO
        Ok(())
    }

    fn visit_ifexpr(&mut self, b: &NodeBox, n: &IfExpr) -> VisitorResult {
        // If-true branch
        self.enter_scope();
        n.cond.visit(&n.cond, self)?;
        for node in &n.exprs {
            node.visit(&node, self)?;
        }
        let tscope = self.leave_scope().unwrap();

        // If-false branch
        self.enter_scope();
        for node in &n.elses {
            node.visit(&node, self)?;
        }
        let mut fscope = self.leave_scope().unwrap();

        // Combine two scopes and put it in the current one
        let curscope = self.scope();
        for (id, var) in tscope.vars() {
            curscope.setvar(id.to_string(), var.clone());
            let var_b : &RefCell<VariableData> = var.borrow();
            let var_t : &mut TypeInfo = &mut var_b.borrow_mut().type_info;
            let has_var = if let Some(altvar) = fscope.vars().get(id) {
                let altvar_b : &RefCell<VariableData> = altvar.borrow();
                let altvar_t : &TypeInfo = &altvar_b.borrow().type_info;
                var_t.add_type(altvar_t.typed().clone());
                true
            } else {
                var_t.add_type(Type::Nil);
                false
            };
            if has_var {
                fscope.mut_vars().remove(id);
            }
        }

        for (id, var) in fscope.vars() {
            curscope.setvar(id.to_string(), var.clone());
            let var_b : &RefCell<VariableData> = var.borrow();
            let var_t : &mut TypeInfo = &mut var_b.borrow_mut().type_info;
            if let Some(altvar) = tscope.vars().get(id) {
                let altvar_b : &RefCell<VariableData> = altvar.borrow();
                let altvar_t : &TypeInfo = &altvar_b.borrow().type_info;
                var_t.add_type(altvar_t.typed().clone());
            } else {
                var_t.add_type(Type::Nil);
            }
        }

        Ok(())
    }

    fn visit_callexpr(&mut self, b: &NodeBox, n: &CallExpr) -> VisitorResult {
        for node in &n.args {
            node.visit(&node, self)?;
        }
        Ok(())
    }

    fn visit_binexpr(&mut self, b : &NodeBox, n: &BinExpr) -> VisitorResult {
        match n.op {
            BinOp::Asg => {
                let node = n.left.downcast_ref::<Value>();
                match &node {
                    Some(Value::Identifier(var)) => {
                        n.right.visit(&n.right, self)?;
                        let right_type = n.right.type_info().clone().into_inner();
                        match self.getvar(var) {
                            Some(mut curvar) => {
                                // TODO unionize if this variable is "virtual"
                                curvar.borrow_mut().replace(VariableData::new_with_type(right_type));
                                n.left.type_info().replace(TypeInfo::new_with_type(Type::Identifier(curvar)));
                            }
                            None => {
                                let mut curvar = self.scope().defvar(var.to_string());
                                curvar.borrow_mut().replace(VariableData::new_with_type(right_type));
                                n.left.type_info().replace(TypeInfo::new_with_type(Type::Identifier(curvar)));
                            }
                        }
                        b.type_info().replace(n.right.type_info().borrow().derived());
                    }
                    _ => return Err(ast::Error::InvalidLHS),
                }
            }
            _ => {
                n.left.visit(&n.left, self)?;
                n.right.visit(&n.right, self)?;
                if n.left.type_info() != n.right.type_info() {
                    return Err(ast::Error::IncompatibleType)
                }
                b.type_info().replace(n.left.type_info().borrow().derived());
            }
        }
        Ok(())
    }

    fn visit_value(&mut self, b : &NodeBox, n: &Value) -> VisitorResult {
        let info = b.type_info();
        match n {
            Value::Integer(_) => {
                info.replace(TypeInfo::new_with_type(Type::Integer));
            }
            Value::Float(_) => {
                info.replace(TypeInfo::new_with_type(Type::Float));
            }
            Value::String(_) => {
                info.replace(TypeInfo::new_with_type(Type::String));
            }
            Value::Identifier(var) => {
                if let Some(curvar) = self.getvar(var) {
                    info.replace(TypeInfo::from_identity(curvar.clone()));
                } else {
                    return Err(ast::Error::UnknownIdentifier)
                }
            }
            _ => return Err(ast::Error::InternalError),
        }
        Ok(())
    }
}