use std::collections::HashMap;
use std::rc::Rc;
use crate::ast::*;
use crate::ast;
use crate::ssa::ssa::*;

pub struct SSAVisitor<'a> {
    funcs: HashMap<Rc<str>, Function>,
    current_func: Option<&'a Function>,
}

impl<'a> SSAVisitor<'a> {
    pub fn new() -> Self {
        SSAVisitor {
            funcs: HashMap::new(),
            current_func: None,
        }
    }
}

impl<'a> Visitor for SSAVisitor<'a> {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult {
        unimplemented!()
    }
    fn visit_defstmt(&mut self,  b: &NodeBox, n: &DefStatement) -> VisitorResult {
        unimplemented!()
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