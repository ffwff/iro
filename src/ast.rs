use std::fmt;
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use crate::types::types::TypeInfo;

pub type VisitorResult = Result<(), ()>;

pub trait Visitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult;
    fn visit_binexpr(&mut self, b: &NodeBox, n: &BinExpr) -> VisitorResult;
    fn visit_value(&mut self,   b: &NodeBox, n: &Value) -> VisitorResult;
}

pub trait Node {
    fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult;
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f)
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f)
    }
}

macro_rules! debuggable {
    () => {
        fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:#?}", self)
        }
    };
}

#[derive(Debug)]
pub struct NodeBox {
    node: Box<Node>,
    type_info: RefCell<Option<TypeInfo>>,
}

impl<'a> NodeBox {

    pub fn new<T: 'static>(node : T) -> Self where T : Node {
        NodeBox {
            node: Box::new(node),
            type_info: RefCell::new(None),
        }
    }

    pub fn node(&'a self) -> &'a Node {
        self.node.borrow()
    }

    pub fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        self.node.visit(self, visitor)
    }

    pub fn type_info(&self) -> &RefCell<Option<TypeInfo>> {
        &self.type_info
    }

}

#[derive(Debug)]
pub struct Program {
    pub exprs: Vec<NodeBox>,
}

impl Node for Program {
    debuggable!();
    
    fn visit(&self, b : &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        Err(())
    }
}

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),
}

impl Node for Value {
    debuggable!();

    fn visit(&self, b : &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_value(b, self)
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Asg,
    Adds,
    Subs,
    Muls,
    Divs,
    Or,
    And,
    Equ,
    Neq,
}

#[derive(Debug)]
pub struct BinExpr {
    pub left:  NodeBox,
    pub right: NodeBox,
    pub op: BinOp,
}

impl Node for BinExpr {
    debuggable!();

    fn visit(&self, b : &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_binexpr(b, self)
    }
}