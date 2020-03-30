use crate::ssa::isa::{IntrinsicType, Type};
use downcast_rs::Downcast;
use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    InternalError,
    InvalidLHS,
    IncompatibleType,
    CannotInfer,
    UnknownIdentifier(Rc<str>),
    UnknownType(Rc<str>),
    UnknownAttribute(String),
    UnknownStatic(String),
    NotEnoughArguments,
    InvalidArguments,
    InvalidReturnType,
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "invalid first item to double"
    }

    fn cause(&self) -> Option<&(dyn std::error::Error)> {
        None
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type VisitorResult = Result<(), Error>;

pub trait Visitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult;
    fn visit_defstmt(&mut self, n: &DefStatement) -> VisitorResult;
    fn visit_return(&mut self, n: &ReturnExpr) -> VisitorResult;
    fn visit_whileexpr(&mut self, n: &WhileExpr) -> VisitorResult;
    fn visit_ifexpr(&mut self, n: &IfExpr) -> VisitorResult;
    fn visit_callexpr(&mut self, n: &CallExpr) -> VisitorResult;
    fn visit_letexpr(&mut self, n: &LetExpr) -> VisitorResult;
    fn visit_binexpr(&mut self, n: &BinExpr) -> VisitorResult;
    fn visit_value(&mut self, n: &Value) -> VisitorResult;
    fn visit_typeid(&mut self, n: &TypeId) -> VisitorResult;
}

pub trait Node: Downcast {
    fn print(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }

    fn visit(&self, visitor: &mut dyn Visitor) -> VisitorResult;
}
impl_downcast!(Node);

impl std::fmt::Display for dyn Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f)
    }
}

impl std::fmt::Debug for dyn Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f)
    }
}

#[derive(Debug, Clone)]
pub struct NodeBox {
    data: Rc<dyn Node>,
}

impl NodeBox {
    pub fn new<T: 'static>(node: T) -> Self
    where
        T: Node,
    {
        NodeBox {
            data: Rc::new(node),
        }
    }

    pub fn rc(&self) -> Rc<dyn Node> {
        self.data.clone()
    }

    pub fn borrow(&self) -> &dyn Node {
        self.data.borrow()
    }

    pub fn visit(&self, visitor: &mut dyn Visitor) -> VisitorResult {
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
        fn visit(&self, visitor: &mut dyn Visitor) -> VisitorResult {
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

    fn visit(&self, _visitor: &mut dyn Visitor) -> VisitorResult {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Value {
    I32(i32),
    I64(i64),
    Float(u64),
    String(Rc<str>),
    Identifier(Rc<str>),
    Any,
}

impl Node for Value {
    debuggable!();
    visitable!(visit_value);
}

#[derive(Debug)]
pub struct TypeId {
    pub data: TypeIdData,
    pub typed: RefCell<Option<Type>>,
}

impl Node for TypeId {
    debuggable!();
    visitable!(visit_typeid);
}

impl TypeId {
    pub fn new(data: TypeIdData) -> Self {
        Self {
            data,
            typed: RefCell::new(None),
        }
    }
}

#[derive(Debug)]
pub enum TypeIdData {
    Identifier(Rc<str>),
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
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug)]
pub struct BinExpr {
    pub left: NodeBox,
    pub right: NodeBox,
    pub op: BinOp,
}

impl Node for BinExpr {
    debuggable!();
    visitable!(visit_binexpr);
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: NodeBox,
    pub exprs: Vec<NodeBox>,
    pub elses: Vec<NodeBox>,
}

impl Node for IfExpr {
    debuggable!();
    visitable!(visit_ifexpr);
}

#[derive(Debug)]
pub struct WhileExpr {
    pub cond: NodeBox,
    pub exprs: Vec<NodeBox>,
}

impl Node for WhileExpr {
    debuggable!();
    visitable!(visit_whileexpr);
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: NodeBox,
    pub args: Vec<NodeBox>,
}

impl CallExpr {
    pub fn new(callee: NodeBox, args: Vec<NodeBox>) -> Self {
        CallExpr { callee, args }
    }
}

impl Node for CallExpr {
    debuggable!();
    visitable!(visit_callexpr);
}

#[derive(Debug)]
pub struct DefStatement {
    pub id: Rc<str>,
    pub args: Vec<(Rc<str>, Option<TypeId>)>,
    pub exprs: Vec<NodeBox>,
    pub return_type: Option<TypeId>,
    pub attrs: Option<Vec<AttributeValue>>,
    pub intrinsic: Cell<IntrinsicType>,
}

impl DefStatement {
    pub fn is_compatible_with_args(&self, arg_types: &Vec<Type>) -> bool {
        if self.args.len() != arg_types.len() {
            return false;
        }
        for ((_, declared_typed), typed) in self.args.iter().zip(arg_types.iter()) {
            if let Some(maybe_declared_rc) = declared_typed.as_ref() {
                let maybe_declared: &Option<Type> = &maybe_declared_rc.typed.borrow();
                if let Some(declared_typed) = maybe_declared {
                    if *declared_typed != *typed {
                        return false;
                    }
                }
            }
        }
        true
    }
}

impl Node for DefStatement {
    debuggable!();
    visitable!(visit_defstmt);
}

#[derive(Debug)]
pub struct AttributeValue {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug)]
pub struct ReturnExpr {
    pub expr: NodeBox,
}

impl Node for ReturnExpr {
    debuggable!();
    visitable!(visit_return);
}
