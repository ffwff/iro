use crate::codegen::structs::StructData;
use crate::ssa::isa::BorrowModifier;
use crate::utils::optcell::OptCell;
use crate::utils::uniquerc::UniqueRc;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;
use std::rc::Rc;
extern crate derivative;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    NoReturn,
    Nil,
    Bool,
    I8,
    I16,
    I32,
    I64,
    ISize,
    F64,
    Pointer(Rc<PointerType>),
    Struct(UniqueRc<StructType>),
    Slice(Rc<SliceType>),
    Union(Rc<BTreeSet<Type>>),
    NeverUsed,
}

impl Type {
    pub fn pointer(self, tag: BorrowModifier) -> Self {
        Type::Pointer(Rc::new(PointerType::new(self, tag)))
    }

    pub fn slice(self, len: u32) -> Self {
        Type::Slice(Rc::new(SliceType::new(self, Some(len))))
    }

    pub fn dyn_slice(self) -> Self {
        Type::Slice(Rc::new(SliceType::new(self, None)))
    }

    pub fn unify(&self, other: &Type) -> Self {
        match (self, other) {
            (left, right) if left == right => left.clone(),
            (Type::NoReturn, right) => right.clone(),
            (left, Type::NoReturn) => left.clone(),
            (Type::Union(set_left), Type::Union(set_right)) => Type::Union(Rc::new({
                let lset: &BTreeSet<Type> = set_left.borrow();
                let rset: &BTreeSet<Type> = set_right.borrow();
                lset & rset
            })),
            (Type::Union(set), right) => Type::Union(Rc::new({
                let bbset: &BTreeSet<Type> = set.borrow();
                let mut bset = bbset.clone();
                bset.insert(right.clone());
                bset
            })),
            (left, Type::Union(set)) => Type::Union(Rc::new({
                let bbset: &BTreeSet<Type> = set.borrow();
                let mut bset = bbset.clone();
                bset.insert(left.clone());
                bset
            })),
            (left, right) => Type::Union(Rc::new(btreeset! { left.clone(), right.clone() })),
        }
    }

    pub fn can_implicit_cast_to(&self, other: &Type) -> bool {
        match (self, other) {
            // Integers with smaller sizes can cast into bigger sizes implicitly
            (int, Type::ISize) if int.is_int() => true,
            (Type::I32, Type::I64) => true,
            (Type::I16, Type::I64) => true,
            (Type::I16, Type::I32) => true,
            (Type::I8, Type::I64) => true,
            (Type::I8, Type::I32) => true,
            (Type::I8, Type::I16) => true,
            _ => false,
        }
    }

    pub fn can_explicit_cast_to(&self, other: &Type) -> bool {
        match (self, other) {
            (left, right) if left == right => true,
            (left, right) if left.is_int() && right.is_int() => true,
            (Type::Pointer(_), int) if int.is_int() => true,
            (int, Type::Pointer(_)) if int.is_int() => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Type::Nil => true,
            Type::Union(set) => {
                let bbset: &BTreeSet<Type> = set.borrow();
                bbset.contains(&Type::Nil)
            }
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::ISize => true,
            _ => false,
        }
    }

    pub fn is_int_repr(&self) -> bool {
        match self {
            Type::Pointer(_) if !self.is_fat_pointer() => true,
            other => other.is_int(),
        }
    }

    pub fn is_fat_pointer(&self) -> bool {
        match self {
            Type::Pointer(ptr_typed) => match &ptr_typed.typed {
                Type::Slice(slice) => slice.is_dyn(),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Nil => true,
            Type::Bool => true,
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::ISize => true,
            Type::F64 => true,
            _ => false,
        }
    }

    pub fn is_value_type(&self) -> bool {
        match self {
            Type::Pointer(_) => false,
            _ => true,
        }
    }

    pub fn as_union(&self) -> Option<Rc<BTreeSet<Type>>> {
        match self {
            Type::Union(set) => Some(set.clone()),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<UniqueRc<StructType>> {
        match self {
            Type::Struct(struct_typed) => Some(struct_typed.clone()),
            _ => None,
        }
    }

    pub fn as_slice(&self) -> Option<Rc<SliceType>> {
        match self {
            Type::Slice(slice) => Some(slice.clone()),
            _ => None,
        }
    }

    pub fn instance_type(&self) -> Option<&Type> {
        match self {
            Type::Pointer(ptr_typed) => {
                if let Type::Slice(slice) = &ptr_typed.typed {
                    if slice.is_dyn() {
                        return Some(&slice.typed);
                    }
                }
                Some(&ptr_typed.typed)
            }
            Type::Slice(slice_rc) => {
                let slice: &SliceType = &slice_rc.borrow();
                Some(&slice.typed)
            }
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::NoReturn => write!(f, "(no return)"),
            Type::Nil => write!(f, "Nil"),
            Type::Bool => write!(f, "Bool"),
            Type::I8 => write!(f, "I8"),
            Type::I16 => write!(f, "I16"),
            Type::I32 => write!(f, "I32"),
            Type::I64 => write!(f, "I64"),
            Type::ISize => write!(f, "ISize"),
            Type::Pointer(ptr_typed) => write!(f, "&{}", ptr_typed.typed),
            Type::F64 => write!(f, "F64"),
            Type::Struct(struct_typed) => write!(f, "{}", struct_typed),
            Type::Slice(slice_rc) => {
                let slice: &SliceType = slice_rc.borrow();
                if let Some(length) = slice.len.clone() {
                    write!(f, "[{}; {}]", slice.typed, length)
                } else {
                    write!(f, "[{}]", slice.typed)
                }
            }
            Type::Union(_) => write!(f, "(union)"),
            Type::NeverUsed => write!(f, "(never used)"),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct StructField {
    pub idx: usize,
    pub typed: Type,
}

#[derive(Debug, Clone)]
pub struct StructType {
    name: Rc<str>,
    vars: HashMap<Rc<str>, StructField>,
    fields: Vec<StructField>,
    pub data: OptCell<Rc<StructData>>,
}

impl StructType {
    pub fn new(name: Rc<str>) -> Self {
        StructType {
            name,
            vars: HashMap::new(),
            fields: vec![],
            data: OptCell::none(),
        }
    }

    pub fn name(&self) -> &Rc<str> {
        &self.name
    }

    pub fn vars(&self) -> &HashMap<Rc<str>, StructField> {
        &self.vars
    }

    pub fn fields(&self) -> &Vec<StructField> {
        &self.fields
    }

    pub fn append(&mut self, key: Rc<str>, typed: Type) {
        let len = self.vars.len();
        let field = StructField { idx: len, typed };
        self.vars.insert(key, field.clone());
        self.fields.push(field);
    }
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialOrd, PartialEq, Eq, Hash)]
pub struct SliceType {
    pub typed: Type,
    pub len: Option<u32>,
    #[derivative(PartialOrd = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub data: OptCell<Rc<StructData>>,
}

impl SliceType {
    pub fn new(typed: Type, len: Option<u32>) -> Self {
        if let Some(len) = len {
            SliceType {
                typed,
                len: Some(len),
                data: OptCell::none(),
            }
        } else {
            SliceType {
                typed,
                len,
                data: OptCell::none(),
            }
        }
    }

    pub fn is_dyn(&self) -> bool {
        self.len.is_none()
    }
}

impl Ord for SliceType {
    fn cmp(&self, other: &SliceType) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct PointerType {
    pub typed: Type,
    pub tag: BorrowModifier,
}

impl PointerType {
    pub fn new(typed: Type, tag: BorrowModifier) -> Self {
        Self { typed, tag }
    }
}
