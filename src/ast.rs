use std::fmt;
use std::borrow::Borrow;
use std::cell::{RefCell, Cell};
use std::rc::Rc;
use std::any::Any;
use downcast_rs::Downcast;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    InternalError,
    InvalidLHS,
    IncompatibleType,
    CannotInfer,
    UnknownIdentifier(Rc<str>),
    NotEnoughArguments,
    InvalidArguments,
}

pub type VisitorResult = Result<(), Error>;

pub trait Visitor {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult;
    fn visit_defstmt(&mut self,  n: &DefStatement) -> VisitorResult;
    fn visit_return(&mut self,   n: &ReturnExpr) -> VisitorResult;
    fn visit_ifexpr(&mut self,   n: &IfExpr) -> VisitorResult;
    fn visit_callexpr(&mut self, n: &CallExpr) -> VisitorResult;
    fn visit_letexpr(&mut self,  n: &LetExpr) -> VisitorResult;
    fn visit_binexpr(&mut self,  n: &BinExpr) -> VisitorResult;
    fn visit_value(&mut self,    n: &Value) -> VisitorResult;
    fn visit_typeid(&mut self,   n: &TypeId) -> VisitorResult;
}

pub trait Node: Downcast {
    fn print(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }

    fn visit(&self, visitor: &mut Visitor) -> VisitorResult;
}
impl_downcast!(Node);

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

#[derive(Debug, Clone)]
pub struct NodeBox {
    data: Rc<Node>,
}

impl NodeBox {
    pub fn new<T: 'static>(node: T) -> Self where T: Node {
        NodeBox {
            data: Rc::new(node)
        }
    }

    pub fn rc(&self) -> Rc<Node> {
        self.data.clone()
    }

    pub fn borrow(&self) -> &Node {
        self.data.borrow()
    }

    pub fn visit(&self, visitor: &mut Visitor) -> VisitorResult {
        self.borrow().visit(visitor)
    }
}

macro_rules! debuggable {
    () => {
        fn print(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:#?}", self)
        }
    };
}

macro_rules! visitable {
    ($x:tt) => {
        fn visit(&self, visitor: &mut Visitor) -> VisitorResult {
            visitor.$x(self)
        }
    };
}

#[derive(Debug)]
pub struct Program {
    pub exprs: Vec<NodeBox>,
}

impl Node for Program {
    debuggable!();
    
    fn visit(&self, _visitor: &mut Visitor) -> VisitorResult {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Float(f64),
    String(Rc<str>),
    Identifier(Rc<str>),
    Any,
}

impl Node for Value {
    debuggable!();
    visitable!(visit_value);
}

#[derive(Debug)]
pub enum TypeId {
    Identifier(Rc<str>),
}

impl Node for TypeId {
    debuggable!();
    visitable!(visit_typeid);
}

#[derive(Debug)]
pub struct LetExpr {
    pub left: NodeBox,
    pub right: NodeBox,
}

impl Node for LetExpr {
    debuggable!();
    visitable!(visit_letexpr);
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
    visitable!(visit_binexpr);
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IfReturnType {
    None,
    OneBranch,
    BothBranch,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond:  NodeBox,
    pub exprs: Vec<NodeBox>,
    pub elses: Vec<NodeBox>,
    pub returntype: Cell<IfReturnType>,
}

impl IfExpr {
    pub fn new(cond : NodeBox, exprs: Vec<NodeBox>, elses: Vec<NodeBox>) -> Self {
        IfExpr {
            cond,
            exprs,
            elses,
            returntype: Cell::new(IfReturnType::None)
        }
    }
}

impl Node for IfExpr {
    debuggable!();
    visitable!(visit_ifexpr);
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: NodeBox,
    pub args: Vec<NodeBox>,
}

impl CallExpr {
    pub fn new(callee : NodeBox, args: Vec<NodeBox>) -> Self {
        CallExpr {
            callee,
            args,
        }
    }
}

impl Node for CallExpr {
    debuggable!();
    visitable!(visit_callexpr);
}

#[derive(Debug)]
pub struct DefStatement {
    pub id: Rc<str>,
    pub args: Vec<(Rc<str>, Option<NodeBox>)>,
    pub exprs: Vec<NodeBox>,
}

impl Node for DefStatement {
    debuggable!();
    visitable!(visit_defstmt);
}

#[derive(Debug)]
pub struct ReturnExpr {
    pub expr: NodeBox,
}

impl Node for ReturnExpr {
    debuggable!();
    visitable!(visit_return);
}