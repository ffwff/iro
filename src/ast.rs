use std::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::any::Any;
use crate::types::types::TypeInfo;

pub type VisitorResult = Result<(), ()>;

pub trait Visitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult;
    fn visit_ifexpr(&mut self, b: &NodeBox, n: &IfExpr) -> VisitorResult;
    fn visit_binexpr(&mut self, b: &NodeBox, n: &BinExpr) -> VisitorResult;
    fn visit_value(&mut self,   b: &NodeBox, n: &Value) -> VisitorResult;
}

pub trait Node {
    fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult;
    fn as_any(&self) -> &dyn Any;
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

macro_rules! as_any {
    () => {
        fn as_any(&self) -> &dyn Any {
            self
        }
    };
}

#[derive(Debug)]
pub struct NodeBox {
    node: Box<Node>,
    type_info: RefCell<TypeInfo>,
}

impl<'a> NodeBox {

    pub fn new<T: 'static>(node : T) -> Self where T : Node {
        NodeBox {
            node: Box::new(node),
            type_info: RefCell::new(TypeInfo::new()),
        }
    }

    pub fn node(&'a self) -> &'a Node {
        self.node.borrow()
    }

    pub fn downcast_ref<T: 'static>(&'a self) -> Option<&'a T> where T : Node {
        self.node.as_any().downcast_ref::<T>()
    }

    pub fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        self.node.visit(self, visitor)
    }

    pub fn type_info(&self) -> &RefCell<TypeInfo> {
        &self.type_info
    }

}

#[derive(Debug)]
pub struct Program {
    pub exprs: Vec<NodeBox>,
}

impl Node for Program {
    debuggable!();
    as_any!();
    
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
    as_any!();

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
    as_any!();

    fn visit(&self, b : &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_binexpr(b, self)
    }
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond:  NodeBox,
    pub exprs: Vec<NodeBox>,
    pub elses: Vec<NodeBox>,
}

impl Node for IfExpr {
    debuggable!();
    as_any!();

    fn visit(&self, b : &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_ifexpr(b, self)
    }
}