use std::fmt;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::RefCell;
use std::any::Any;
use std::collections::HashMap;
use crate::types::types::{Variable, TypeInfo};

#[derive(Debug)]
pub enum Error {
    InternalError,
    InvalidLHS,
    IncompatibleType,
    CannotInfer,
    UnknownIdentifier(String),
    NotEnoughArguments,
    InvalidArguments,
}

pub type VisitorResult = Result<(), Error>;

pub trait Visitor {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult;
    fn visit_defstmt(&mut self,  b: &NodeBox, n: &DefStatement) -> VisitorResult;
    fn visit_return(&mut self,   b: &NodeBox, n: &ReturnExpr) -> VisitorResult;
    fn visit_ifexpr(&mut self,   b: &NodeBox, n: &IfExpr) -> VisitorResult;
    fn visit_callexpr(&mut self, b: &NodeBox, n: &CallExpr) -> VisitorResult;
    fn visit_binexpr(&mut self,  b: &NodeBox, n: &BinExpr) -> VisitorResult;
    fn visit_value(&mut self,    b: &NodeBox, n: &Value) -> VisitorResult;
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
        unimplemented!()
    }
}

pub struct Identifier {
    pub id : String,
    pub var : RefCell<Option<Variable>>,
}

impl Identifier {
    pub fn new(id : String) -> Self {
        Identifier {
            id,
            var: RefCell::new(None),
        }
    }
}

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var : &Option<Variable> = &self.var.borrow();
        match var {
            Some(var) => write!(f, "(Identifier {:?}, {:p})", self.id, var),
            None => write!(f, "(Identifier {:?})", self.id),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(Identifier),
    Any,
}

impl Node for Value {
    debuggable!();
    as_any!();

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
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

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
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

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_ifexpr(b, self)
    }
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: NodeBox,
    pub args: Vec<NodeBox>,
}

impl Node for CallExpr {
    debuggable!();
    as_any!();

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_callexpr(b, self)
    }
}

#[derive(Debug)]
pub struct DefStatement {
    pub id: String,
    pub args: Vec<(String, Option<NodeBox>)>,
    pub exprs: Vec<NodeBox>,
}

impl Node for DefStatement {
    debuggable!();
    as_any!();

    fn visit(&self, b: &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_defstmt(b, self)
    }
}

#[derive(Debug)]
pub struct ReturnExpr {
    pub expr: NodeBox,
}

impl Node for ReturnExpr {
    debuggable!();
    as_any!();

    fn visit(&self, b : &NodeBox, visitor: &mut Visitor) -> VisitorResult {
        visitor.visit_return(b, self)
    }
}