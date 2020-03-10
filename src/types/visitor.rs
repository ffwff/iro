use std::borrow::BorrowMut;
use crate::ast::*;
use crate::types::types::*;

pub struct TypeVisitor {
}

impl TypeVisitor {
    pub fn new() -> Self {
        TypeVisitor {}
    }
}

impl Visitor for TypeVisitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult {
        for node in &n.exprs {
            node.visit(&node, self)?
        }
        Ok(())
    }

    fn visit_binexpr(&mut self, b : &NodeBox, n: &BinExpr) -> VisitorResult {
        n.left.visit(&n.left, self)?;
        n.right.visit(&n.right, self)?;
        if n.left.type_info() != n.right.type_info() {
            return Err(())
        }
        let info = b.type_info();
        let lhs = n.left.type_info();
        info.replace(lhs.borrow().clone());
        Ok(())
    }

    fn visit_value(&mut self, b : &NodeBox, n: &Value) -> VisitorResult {
        match n {
            Value::Integer(_) => {
                let info = b.type_info();
                info.replace(Some(TypeInfo::new_with_type(Type::Integer)));
            }
            _ => return Err(())
        }
        Ok(())
    }
}