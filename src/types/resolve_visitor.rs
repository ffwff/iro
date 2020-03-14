use std::collections::HashMap;
use std::rc::Rc;
use crate::ast::*;
use crate::ast;
use crate::types;

pub struct ResolveVisitor<'a> {
    node: &'a ast::NodeBox,
}

impl<'a> ResolveVisitor<'a> {
    pub fn new(node: &'a ast::NodeBox) -> Self {
        ResolveVisitor {
            node,
        }
    }

    pub fn process(&mut self) -> VisitorResult {
        let defstmt = self.node.downcast_ref::<DefStatement>().unwrap();
        self.visit_defstmt(self.node, defstmt)
    }
}

impl<'a> Visitor for ResolveVisitor<'a> {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult {
        unimplemented!()
    }
    fn visit_defstmt(&mut self,  b: &NodeBox, n: &DefStatement) -> VisitorResult {
        let func = b.type_info().borrow().with(|typed| {
            if let types::Type::Function(func) = typed {
                Ok(func.clone())
            } else {
                Err(Error::InternalError)
            }
        })?;
        func.with_mut(|func| {
            let overloads = func.overloads.as_ref().unwrap();
            for (c_args, c_returntype) in &overloads {
                for (arg, c_arg) in func.args.iter().zip(c_args) {
                    match (arg.is_unresolved(), c_arg.is_unresolved()) {
                        (false, false) => {
                            // We presumably already type-checked this, continue
                            continue;
                        }
                        (true, false) => {
                            unimplemented!()
                        }
                        (false, true) => {
                            unimplemented!()
                        }
                        (true, true) => {
                            continue;
                        }
                    }
                }
            }
            Ok(())
        })
    }
    fn visit_return(&mut self,   b: &NodeBox, n: &ReturnExpr) -> VisitorResult {
        unimplemented!()
    }
    fn visit_ifexpr(&mut self,   b: &NodeBox, n: &IfExpr) -> VisitorResult {
        unimplemented!()
    }
    fn visit_callexpr(&mut self, b: &NodeBox, n: &CallExpr) -> VisitorResult {
        unimplemented!()
    }
    fn visit_binexpr(&mut self,  b: &NodeBox, n: &BinExpr) -> VisitorResult {
        unimplemented!()
    }
    fn visit_value(&mut self,    b: &NodeBox, n: &Value) -> VisitorResult {
        unimplemented!()
    }
    fn visit_typeid(&mut self,   b: &NodeBox, n: &TypeId) -> VisitorResult {
        unimplemented!()
    }
}