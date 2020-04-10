use crate::compiler;
use crate::ssa::isa::{IntrinsicType, Type};
use downcast_rs::Downcast;
use std::borrow::Borrow;
use std::cell::RefCell;
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
    MutatingImmutable(Rc<str>),
}

impl Error {
    pub fn into_compiler_error(self, ast: &NodeBox) -> compiler::Error {
        compiler::Error {
            error: Box::new(self),
            span: ast.span(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InternalError => write!(f, "Internal error"),
            Error::InvalidLHS => write!(f, "Invalid left-hand-side expression"),
            Error::IncompatibleType => write!(f, "Incompatible type"),
            Error::CannotInfer => write!(f, "Cannot infer type for value"),
            Error::UnknownIdentifier(id) => write!(f, "Unknown identifier {:?}", id),
            Error::UnknownType(id) => write!(f, "Unknown type {:?}", id),
            Error::UnknownAttribute(id) => write!(f, "Unknown attribute {:?}", id),
            Error::UnknownStatic(id) => write!(f, "Unknown external function {:?}", id),
            Error::NotEnoughArguments => write!(f, "Not enough arguments for function"),
            Error::InvalidArguments => write!(f, "Invalid arguments for function"),
            Error::InvalidReturnType => write!(f, "Invalid return type"),
            Error::MutatingImmutable(id) => write!(f, "Mutating immutable variable {:?}", id),
        }
    }
}

pub type VisitorResult = Result<(), compiler::Error>;

pub trait Visitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult;
    fn visit_import(&mut self, n: &ImportStatement, b: &NodeBox) -> VisitorResult;
    fn visit_defstmt(&mut self, n: &DefStatement, b: &NodeBox) -> VisitorResult;
    fn visit_return(&mut self, n: &ReturnExpr, b: &NodeBox) -> VisitorResult;
    fn visit_whileexpr(&mut self, n: &WhileExpr, b: &NodeBox) -> VisitorResult;
    fn visit_ifexpr(&mut self, n: &IfExpr, b: &NodeBox) -> VisitorResult;
    fn visit_callexpr(&mut self, n: &CallExpr, b: &NodeBox) -> VisitorResult;
    fn visit_letexpr(&mut self, n: &LetExpr, b: &NodeBox) -> VisitorResult;
    fn visit_binexpr(&mut self, n: &BinExpr, b: &NodeBox) -> VisitorResult;
    fn visit_asexpr(&mut self, n: &AsExpr, b: &NodeBox) -> VisitorResult;
    fn visit_member_expr(&mut self, n: &MemberExpr, b: &NodeBox) -> VisitorResult;
    fn visit_value(&mut self, n: &Value, b: &NodeBox) -> VisitorResult;
    fn visit_typeid(&mut self, n: &TypeId, b: &NodeBox) -> VisitorResult;
}

pub trait Node: Downcast {
    fn print(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }

    fn visit(&self, visitor: &mut dyn Visitor, b: &NodeBox) -> VisitorResult;
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
    span: (usize, usize),
}

impl NodeBox {
    pub fn new<T: 'static>(node: T, span: (usize, usize)) -> Self
    where
        T: Node,
    {
        NodeBox {
            data: Rc::new(node),
            span,
        }
    }

    pub fn rc(&self) -> Rc<dyn Node> {
        self.data.clone()
    }

    pub fn span(&self) -> (usize, usize) {
        self.span.clone()
    }

    pub fn borrow(&self) -> &dyn Node {
        self.data.borrow()
    }

    pub fn visit(&self, visitor: &mut dyn Visitor) -> VisitorResult {
        self.borrow().visit(visitor, self)
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
        fn visit(&self, visitor: &mut dyn Visitor, b: &NodeBox) -> VisitorResult {
            visitor.$x(self, b)
        }
    };
}

#[derive(Debug)]
pub struct Program {
    pub exprs: Vec<NodeBox>,
}

impl Node for Program {
    debuggable!();

    fn visit(&self, _visitor: &mut dyn Visitor, _b: &NodeBox) -> VisitorResult {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Value {
    I32(i32),
    I64(i64),
    Float(u64),
    Bool(bool),
    String(Rc<str>),
    Identifier(Rc<str>),
    Slice(Vec<NodeBox>),
}

impl Node for Value {
    debuggable!();
    visitable!(visit_value);
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum TypeIdData {
    Identifier(Rc<str>),
    Pointer(Box<TypeId>),
    Slice {
        typed: Box<TypeId>,
        length: Option<u32>,
    },
}

#[derive(Debug)]
pub struct LetExpr {
    pub left: NodeBox,
    pub right: NodeBox,
    pub is_mut: bool,
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
    Mod,
    Asg,
    Adds,
    Subs,
    Muls,
    Divs,
    Mods,
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
    pub intrinsic: RefCell<IntrinsicType>,
}

pub enum ArgCompatibility {
    None,
    WithCast(Vec<(usize, Type)>),
    Full,
}

impl DefStatement {
    pub fn compatibility_with_args(&self, arg_types: &Vec<Type>) -> ArgCompatibility {
        if self.args.len() != arg_types.len() {
            return ArgCompatibility::None;
        }
        let mut casts = vec![];
        for (idx, ((_, declared_typed), typed)) in
            self.args.iter().zip(arg_types.iter()).enumerate()
        {
            if let Some(maybe_declared_rc) = declared_typed.as_ref() {
                let maybe_declared: &Option<Type> = &maybe_declared_rc.typed.borrow();
                if let Some(declared_typed) = maybe_declared {
                    if typed.can_implicit_cast_to(declared_typed) {
                        casts.push((idx, declared_typed.clone()));
                    } else if *declared_typed != *typed {
                        return ArgCompatibility::None;
                    }
                }
            }
        }
        if casts.is_empty() {
            ArgCompatibility::Full
        } else {
            ArgCompatibility::WithCast(casts)
        }
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

#[derive(Debug, Clone)]
pub enum MemberExprArm {
    Identifier(Rc<str>),
    Index(NodeBox),
}

#[derive(Debug)]
pub struct MemberExpr {
    pub left: NodeBox,
    pub right: MemberExprArm,
}

impl Node for MemberExpr {
    debuggable!();
    visitable!(visit_member_expr);
}

#[derive(Debug)]
pub struct AsExpr {
    pub left: NodeBox,
    pub typed: TypeId,
}

impl Node for AsExpr {
    debuggable!();
    visitable!(visit_asexpr);
}

#[derive(Debug)]
pub struct ImportStatement {
    pub path: String,
}

impl Node for ImportStatement {
    debuggable!();
    visitable!(visit_import);
}
