use crate::compiler;
use crate::compiler::sources::SourceSpan;
use crate::ssa::isa::{BorrowModifier, IntrinsicType, Type};
use downcast_rs::Downcast;
use smallvec::SmallVec;
use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::rc::Rc;
pub mod pp_visitor;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    InvalidLHS,
    IncompatibleType { got: Type, expected: Type },
    CannotInfer,
    UnknownIdentifier(Rc<str>),
    UnknownType(Rc<str>),
    UnknownMemberRef(Rc<str>),
    UnknownStructField(Rc<str>),
    UnknownAttribute(String),
    UnknownStatic(String),
    NotEnoughArguments,
    InvalidArguments,
    InvalidReturnType,
    MutatingImmutable(Rc<str>),
    CannotDeref,
}

impl Error {
    pub fn into_compiler_error(self, ast: &NodeBox) -> compiler::Error {
        use compiler::error::Code;
        compiler::Error {
            error: match self {
                Error::InvalidLHS => Code::InvalidLHS,
                Error::IncompatibleType { got, expected } => {
                    Code::IncompatibleType { got, expected }
                }
                Error::CannotInfer => Code::CannotInfer,
                Error::UnknownIdentifier(x) => Code::UnknownIdentifier(x),
                Error::UnknownType(x) => Code::UnknownType(x),
                Error::UnknownMemberRef(x) => Code::UnknownMemberRef(x),
                Error::UnknownStructField(x) => Code::UnknownStructField(x),
                Error::UnknownAttribute(x) => Code::UnknownAttribute(x),
                Error::UnknownStatic(x) => Code::UnknownStatic(x),
                Error::NotEnoughArguments => Code::NotEnoughArguments,
                Error::InvalidArguments => Code::InvalidArguments,
                Error::InvalidReturnType => Code::InvalidReturnType,
                Error::MutatingImmutable(x) => Code::MutatingImmutable(x),
                Error::CannotDeref => Code::CannotDeref,
            },
            span: Some(ast.span()),
        }
    }
}

pub type VisitorResult = Result<(), compiler::Error>;

macro_rules! visit_func {
    ($name:tt, $type:tt) => {
        fn $name(&mut self, n: &$type, b: &NodeBox) -> VisitorResult;
    };
}

pub trait Visitor {
    fn visit_program(&mut self, n: &mut Program) -> VisitorResult;
    visit_func!(visit_import, ImportStatement);
    visit_func!(visit_class, ClassStatement);
    visit_func!(visit_class_init, ClassInitExpr);
    visit_func!(visit_defstmt, DefStatement);
    visit_func!(visit_return, ReturnExpr);
    visit_func!(visit_whileexpr, WhileExpr);
    visit_func!(visit_ifexpr, IfExpr);
    visit_func!(visit_callexpr, CallExpr);
    visit_func!(visit_letexpr, LetExpr);
    visit_func!(visit_binexpr, BinExpr);
    visit_func!(visit_asexpr, AsExpr);
    visit_func!(visit_member_expr, MemberExpr);
    visit_func!(visit_value, Value);
    visit_func!(visit_typeid, TypeId);
    visit_func!(visit_break, BreakExpr);
    visit_func!(visit_borrow, BorrowExpr);
    visit_func!(visit_deref, DerefExpr);
    visit_func!(visit_unary, UnaryExpr);
    visit_func!(visit_modstmt, ModStatement);
    visit_func!(visit_path, PathExpr);
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
    span: Cell<SourceSpan>,
    pub generate_retvar: Cell<bool>,
}

impl NodeBox {
    pub fn new<T: 'static>(node: T, span: (usize, usize)) -> Self
    where
        T: Node,
    {
        NodeBox {
            data: Rc::new(node),
            span: Cell::new(SourceSpan::from_tuple((span.0, span.1 + 1))),
            generate_retvar: Cell::new(true),
        }
    }

    #[inline]
    pub fn rc(&self) -> Rc<dyn Node> {
        self.data.clone()
    }

    #[inline]
    pub fn span(&self) -> SourceSpan {
        self.span.clone().into_inner()
    }

    #[inline]
    pub fn span_ref(&self) -> &Cell<SourceSpan> {
        &self.span
    }

    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub fn borrow(&self) -> &dyn Node {
        self.data.borrow()
    }

    #[inline]
    pub fn visit(&self, visitor: &mut dyn Visitor) -> VisitorResult {
        self.borrow().visit(visitor, self)
    }

    #[inline]
    pub fn can_import(&self) -> bool {
        let borrow = self.borrow();
        if borrow.downcast_ref::<DefStatement>().is_some() {
            true
        } else if borrow.downcast_ref::<ClassStatement>().is_some() {
            true
        } else if borrow.downcast_ref::<ModStatement>().is_some() {
            true
        } else {
            false
        }
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
    Uninitialized,
    I32(i32),
    I64(i64),
    ISize(i64),
    Float(u64),
    Bool(bool),
    String(Rc<str>),
    Identifier(Rc<str>),
    Slice(Vec<NodeBox>),
}

impl Value {
    pub fn as_identifier(&self) -> Option<&Rc<str>> {
        match self {
            Value::Identifier(x) => Some(x),
            _ => None,
        }
    }
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
    Path(PathVec),
    Pointer {
        typed: Box<TypeId>,
        borrow_mod: BorrowModifier,
    },
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
    pub typed: Option<TypeId>,
}

impl Node for LetExpr {
    debuggable!();
    visitable!(visit_letexpr);
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct DefArgument {
    pub name: Rc<str>,
    pub type_id: Option<TypeId>,
    pub is_mut: bool,
}

#[derive(Debug)]
pub struct FuncAttributes {
    pub intrinsic: RefCell<IntrinsicType>,
    pub always_generate: Cell<bool>,
}

impl FuncAttributes {
    pub fn default() -> Self {
        Self {
            intrinsic: RefCell::new(IntrinsicType::None),
            always_generate: Cell::new(false),
        }
    }

    pub fn with_intrinsic(typed: IntrinsicType) -> Self {
        let mut returned = Self::default();
        returned.intrinsic = RefCell::new(typed);
        returned
    }
}

#[derive(Debug)]
pub struct ModStatement {
    pub id: Rc<str>,
    pub exprs: Vec<NodeBox>,
}

impl Node for ModStatement {
    debuggable!();
    visitable!(visit_modstmt);
}

#[derive(Debug)]
pub struct DefStatement {
    pub id: Rc<str>,
    pub args: Vec<DefArgument>,
    pub exprs: Vec<NodeBox>,
    pub return_type: Option<TypeId>,
    pub attrs: Option<Vec<AttributeValue>>,
    pub func_attrs: FuncAttributes,
}

pub enum ArgCompatibility {
    None,
    WithCast(Vec<(usize, Type)>),
    Full,
}

impl DefStatement {
    pub fn compatibility_with_args(&self, arg_types: &[Type]) -> ArgCompatibility {
        if self.args.len() != arg_types.len() {
            return ArgCompatibility::None;
        }
        let mut casts = vec![];
        for (idx, (declared_arg, typed)) in self.args.iter().zip(arg_types.iter()).enumerate() {
            let declared_typed = declared_arg.type_id.as_ref();
            if let Some(maybe_declared_rc) = declared_typed {
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
    pub right: Vec<MemberExprArm>,
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

#[derive(Debug)]
pub struct BreakExpr {}

impl Node for BreakExpr {
    debuggable!();
    visitable!(visit_break);
}

#[derive(Debug)]
pub struct ClassStatement {
    pub id: Rc<str>,
    pub actual_path: RefCell<PathVec>,
    pub inners: Vec<ClassInner>,
}

impl ClassStatement {
    pub fn new(id: Rc<str>, inners: Vec<ClassInner>) -> Self {
        Self {
            id,
            actual_path: RefCell::new(PathVec::new()),
            inners,
        }
    }
}

impl Node for ClassStatement {
    debuggable!();
    visitable!(visit_class);
}

#[derive(Debug)]
pub enum ClassInner {
    MemberDef { name: Rc<str>, typed: TypeId },
}

#[derive(Debug)]
pub struct ClassInitExpr {
    pub path: PathVec,
    pub inits: Vec<(Rc<str>, NodeBox)>,
}

impl Node for ClassInitExpr {
    debuggable!();
    visitable!(visit_class_init);
}

#[derive(Debug)]
pub struct BorrowExpr {
    pub expr: NodeBox,
    pub borrow_mod: BorrowModifier,
}

impl Node for BorrowExpr {
    debuggable!();
    visitable!(visit_borrow);
}

#[derive(Debug)]
pub struct DerefExpr {
    pub expr: NodeBox,
}

impl Node for DerefExpr {
    debuggable!();
    visitable!(visit_deref);
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Uni,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: NodeBox,
}

impl Node for UnaryExpr {
    debuggable!();
    visitable!(visit_unary);
}

#[derive(Debug)]
pub struct PathExpr {
    pub path: PathVec,
}

impl Node for PathExpr {
    debuggable!();
    visitable!(visit_path);
}

pub type PathVec = SmallVec<[Rc<str>; 1]>;
