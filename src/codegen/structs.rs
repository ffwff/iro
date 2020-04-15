use crate::ssa::isa::{Type, Builtins};
use std::collections::HashMap;
use std::rc::Rc;
use std::cmp::max;

fn align(value: usize, to: usize) -> usize {
    (value + to - 1) & !(to - 1)
}

#[derive(Debug, Clone)]
pub struct StructFieldData {
    pub offset: usize,
    pub typed: Type,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    F64,
}

impl PrimitiveType {
    pub fn from_type(typed: &Type) -> Option<Self> {
        match typed {
            Type::I8 => Some(PrimitiveType::I8),
            Type::I16 => Some(PrimitiveType::I16),
            Type::I32 => Some(PrimitiveType::I32),
            Type::I32Ptr(_) if !typed.is_fat_pointer() => Some(PrimitiveType::I32),
            Type::I64 => Some(PrimitiveType::I64),
            Type::I64Ptr(_) if !typed.is_fat_pointer() => Some(PrimitiveType::I64),
            Type::F64 => Some(PrimitiveType::F64),
            _ => None,
        }
    }

    pub fn bytes(&self) -> usize {
        match self {
            PrimitiveType::I8 => 1,
            PrimitiveType::I16 => 2,
            PrimitiveType::I32 => 4,
            PrimitiveType::I64 => 8,
            PrimitiveType::F64 => 8,
        }
    }
}

pub trait AggregateData {
    fn flattened_fields(&self) -> &Vec<PrimitiveTypeField>;
    fn size_of(&self) -> usize;
    fn align_of(&self) -> usize;
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct PrimitiveTypeField {
    pub offset: usize,
    pub multiplier: usize,
    pub typed: PrimitiveType,
}

/// Structure data
#[derive(Debug, Clone)]
pub struct StructData {
    values: HashMap<Rc<str>, StructFieldData>,
    flattened_fields: Vec<PrimitiveTypeField>,
    size_of: usize,
    align_of: usize,
    name: Rc<str>,
}

impl StructData {
    pub fn new(name: Rc<str>) -> Self {
        StructData {
            values: HashMap::new(),
            flattened_fields: vec![],
            size_of: 0,
            align_of: 0,
            name,
        }
    }

    pub fn name(&self) -> &Rc<str> {
        &self.name
    }

    pub fn values(&self) -> &HashMap<Rc<str>, StructFieldData> {
        &self.values
    }

    pub fn append_prim(&mut self, name: Rc<str>, typed: Type) {
        let primitive_type = PrimitiveType::from_type(&typed).unwrap();
        // TODO: flatten structs in typed
        self.size_of = align(self.size_of, primitive_type.bytes());
        self.flattened_fields.push(PrimitiveTypeField {
            offset: self.size_of,
            multiplier: 1,
            typed: primitive_type,
        });
        self.values.insert(
            name,
            StructFieldData {
                offset: self.size_of,
                typed,
            },
        );
        self.size_of += primitive_type.bytes();
        self.align_of = max(self.align_of, primitive_type.bytes());
    }

    pub fn append_aggregate_data(
        &mut self,
        name: Rc<str>,
        typed: Type,
        aggregate_data: &dyn AggregateData
    ) {
        let struct_offset = align(self.size_of, aggregate_data.align_of());
        for field in aggregate_data.flattened_fields() {
            self.flattened_fields.push(PrimitiveTypeField {
                offset: struct_offset + field.offset,
                multiplier: field.multiplier,
                typed: field.typed,
            });
            self.align_of = max(self.align_of, field.typed.bytes());
        }
        self.values.insert(
            name,
            StructFieldData {
                offset: struct_offset,
                typed,
            },
        );
        self.size_of = struct_offset + aggregate_data.size_of();
    }

    pub fn append_type(
        &mut self,
        name: Rc<str>,
        typed: &Type,
        builtins: &Builtins,
    ) {
        if let Some(aggregate_data) = typed.as_aggregate_data(builtins) {
            self.append_aggregate_data(name, typed.clone(), aggregate_data)
        } else {
            self.append_prim(name, typed.clone())
        }
    }
}

impl AggregateData for StructData {
    fn flattened_fields(&self) -> &Vec<PrimitiveTypeField> {
        &self.flattened_fields
    }

    fn size_of(&self) -> usize {
        self.size_of
    }

    fn align_of(&self) -> usize {
        self.align_of
    }
}

impl std::fmt::Display for StructData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Array data
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct ArrayData {
    flattened_fields: Vec<PrimitiveTypeField>,
    size_of: usize,
    align_of: usize,
}

impl ArrayData {
    pub fn new(typed: Type, multiplier: usize) -> Self {
        ArrayData {
            flattened_fields: vec![
                // TODO: flatten structs
                PrimitiveTypeField {
                    offset: 0,
                    multiplier,
                    typed: PrimitiveType::from_type(&typed).unwrap(),
                },
            ],
            size_of: typed.bytes().unwrap() * multiplier,
            align_of: typed.bytes().unwrap(),
        }
    }
}

impl AggregateData for ArrayData {
    fn flattened_fields(&self) -> &Vec<PrimitiveTypeField> {
        &self.flattened_fields
    }

    fn size_of(&self) -> usize {
        self.size_of
    }

    fn align_of(&self) -> usize {
        self.align_of
    }
}

/// Struct builder
pub struct StructBuilder<'a> {
    vec: Vec<u8>,
    data: &'a StructData,
}

impl<'a> StructBuilder<'a> {
    pub fn new(data: &'a StructData) -> Self {
        StructBuilder {
            vec: vec![0; data.size_of()],
            data,
        }
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.vec
    }

    pub fn insert(&mut self, name: &str, bytes: &[u8]) -> Option<usize> {
        if let Some(data) = self.data.values().get(name) {
            if data.typed.bytes().unwrap() != bytes.len() {
                return None;
            }
            for (i, &byte) in bytes.iter().enumerate() {
                self.vec[data.offset + i] = byte;
            }
            return Some(data.offset);
        }
        None
    }

    pub fn insert_zeroed(&mut self, name: &str) -> Option<usize> {
        if let Some(data) = self.data.values().get(name) {
            for i in data.offset..(data.offset + data.typed.bytes().unwrap() as usize) {
                self.vec[i] = 0;
            }
            return Some(data.offset);
        }
        None
    }
}
