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
    Union(Rc<UnionType>),
    NeverUsed,
}

impl Type {
    #[inline]
    pub fn pointer(self, tag: BorrowModifier) -> Self {
        Type::Pointer(Rc::new(PointerType::new(self, tag)))
    }

    #[inline]
    pub fn slice(self, len: u32) -> Self {
        Type::Slice(Rc::new(SliceType::new(self, Some(len))))
    }

    #[inline]
    pub fn dyn_slice(self) -> Self {
        Type::Slice(Rc::new(SliceType::new(self, None)))
    }

    pub fn unify(&self, other: &Type) -> Self {
        match (self, other) {
            (left, right) if left == right => left.clone(),
            (Type::NoReturn, right) => right.clone(),
            (left, Type::NoReturn) => left.clone(),
            (Type::Union(set_left), Type::Union(set_right)) => {
                Type::Union(Rc::new(set_left.unify(set_right)))
            }
            (Type::Union(set), right) => Type::Union(Rc::new(set.unify_single(right.clone()))),
            (left, Type::Union(set)) => Type::Union(Rc::new(set.unify_single(left.clone()))),
            (left, right) => {
                Type::Union(Rc::new(UnionType::from_types(left.clone(), right.clone())))
            }
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

    #[inline]
    pub fn is_nil(&self) -> bool {
        match self {
            Type::Nil => true,
            Type::Union(set) => {
                let union_type: &UnionType = set.borrow();
                union_type.types.contains(&Type::Nil)
            }
            _ => false,
        }
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::ISize => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_int_repr(&self) -> bool {
        match self {
            Type::Pointer(_) if !self.is_fat_pointer() => true,
            other => other.is_int(),
        }
    }

    #[inline]
    pub fn is_fat_pointer(&self) -> bool {
        match self {
            Type::Pointer(ptr_typed) => match &ptr_typed.typed {
                Type::Slice(slice) => slice.is_dyn(),
                _ => false,
            },
            _ => false,
        }
    }

    #[inline]
    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Nil
            | Type::Bool
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::ISize
            | Type::F64 => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_memory_type(&self) -> bool {
        match self {
            Type::Pointer(_) => self.is_fat_pointer(),
            Type::Struct(_) => true,
            Type::Slice(_) => true,
            Type::Union(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_value_type(&self) -> bool {
        match self {
            Type::Pointer(_) => false,
            _ => true,
        }
    }

    #[inline]
    pub fn is_union(&self) -> bool {
        match self {
            Type::Union(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_copyable(&self) -> bool {
        match self {
            Type::Struct(_) => false,
            Type::Union(_) => false,
            Type::Slice(x) => x.typed.is_copyable(),
            _ => true,
        }
    }

    #[inline]
    pub fn as_union(&self) -> Option<Rc<UnionType>> {
        match self {
            Type::Union(set) => Some(set.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_struct(&self) -> Option<UniqueRc<StructType>> {
        match self {
            Type::Struct(struct_typed) => Some(struct_typed.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_slice(&self) -> Option<Rc<SliceType>> {
        match self {
            Type::Slice(slice) => Some(slice.clone()),
            _ => None,
        }
    }

    #[inline]
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
            Type::Slice(slice) => write!(f, "{}", slice),
            Type::Union(union) => write!(f, "{}", union),
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

impl std::fmt::Display for SliceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(length) = self.len {
            write!(f, "[{}; {}]", self.typed, length)
        } else {
            write!(f, "[{}]", self.typed)
        }
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

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialOrd, PartialEq, Eq, Hash)]
pub struct UnionType {
    /// A list of types in the union, it MUST be sorted
    types: Vec<Type>,
    #[derivative(PartialOrd = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub data: OptCell<Rc<StructData>>,
}

impl UnionType {
    pub fn types(&self) -> &Vec<Type> {
        &self.types
    }

    pub fn index(&self, typed: &Type) -> Option<usize> {
        if let Ok(idx) = self.types.binary_search(typed) {
            Some(idx)
        } else {
            None
        }
    }

    pub fn from_types(left: Type, right: Type) -> Self {
        UnionType {
            types: if left < right {
                vec![left, right]
            } else {
                vec![right, left]
            },
            data: OptCell::none(),
        }
    }

    pub fn unify(&self, other: &UnionType) -> Self {
        let mut cloned = self.clone();
        cloned
            .types
            .extend(other.types.iter().map(|typed| typed.clone()));
        cloned.types.sort_unstable();
        cloned.types.dedup();
        cloned
    }

    pub fn unify_single(&self, typed: Type) -> Self {
        let mut cloned = self.clone();
        if let Err(idx) = cloned.types.binary_search(&typed) {
            cloned.types.insert(idx, typed);
        }
        cloned
    }
}

impl Ord for UnionType {
    fn cmp(&self, other: &UnionType) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::fmt::Display for UnionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.types
                .iter()
                .map(|typed| {
                    use std::fmt::Write;
                    let mut x = String::new();
                    write!(&mut x, "{}", typed).unwrap();
                    x
                })
                .collect::<Vec<String>>()
                .join(" | ")
        )
    }
}
