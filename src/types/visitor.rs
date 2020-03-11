use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use crate::ast::*;
use crate::types::types::{Variable, TypeInfo, Type};
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

    fn leave_scope(&mut self) {
        self.envs.pop();
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
            node.visit(&node, self)?
        }
        self.leave_scope();
        Ok(())
    }

    fn visit_binexpr(&mut self, b : &NodeBox, n: &BinExpr) -> VisitorResult {
        match n.op {
            BinOp::Asg => {
                let node = n.left.downcast_ref::<Value>();
                match &node {
                    Some(Value::Identifier(var)) => {
                        println!("print: {}", var);
                        n.right.visit(&n.right, self)?;
                        match self.getvar(var) {
                            Some(mut curvar) => {
                                // TODO unionize if this variable is "virtual"
                                curvar.borrow_mut().replace(n.right.type_info().clone().into_inner());
                                n.left.type_info().replace(TypeInfo::new_with_type(Type::Identifier(curvar)));
                            }
                            None => {
                                let mut curvar = self.scope().defvar(var.to_string());
                                curvar.borrow_mut().replace(n.right.type_info().clone().into_inner());
                                n.left.type_info().replace(TypeInfo::new_with_type(Type::Identifier(curvar)));
                            }
                        }
                        b.type_info().replace(n.right.type_info().borrow().clone());
                    }
                    _ => return Err(())
                }
            }
            _ => {
                n.left.visit(&n.left, self)?;
                n.right.visit(&n.right, self)?;
                if n.left.type_info() != n.right.type_info() {
                    return Err(())
                }
                b.type_info().replace(n.left.type_info().borrow().clone());
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
                    let varinfo : &RefCell<TypeInfo> = curvar.borrow();
                    info.replace(varinfo.clone().into_inner());
                } else {
                    return Err(())
                }
            }
            _ => return Err(())
        }
        Ok(())
    }
}